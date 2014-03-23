(ns jdt.easyfs
  "Clojure functions for easy file system manipulation. Requires Java 7.
   Most everything in this module coerces arguments to java.nio.file.Path, and returns Path objects.
   By default, shell-like tilde expansion is also enabled on inputs to these APIs."
  (:require clojure.string)             ;split-lines
  (:use jdt.core)
  (:use [jdt.shell :only [bash-tilde-expansion]])
  (:use [jdt.closer :only [ensure-close]])
  (:use clojure.java.io)
  (:import java.io.File)
  (:import java.net.URI)
  (:import (java.nio.file DirectoryStream DirectoryStream$Filter
                          Files FileSystems FileSystem LinkOption Path Paths
                          PathMatcher))
  (:import (java.nio.file.attribute FileAttribute PosixFilePermission PosixFilePermissions))
  (:import java.nio.charset.Charset)
  )

(let [java-version-string (System/getProperty "java.version")
      major-minor-substring (second (re-find #"(\d+\.\d+)\." java-version-string))]
  (if (< (read-string major-minor-substring) 1.7)
    (throw (Exception. (str (ns-name *ns*)
                            " is only compatible with java versions >= 1.7, you are running version "
                            java-version-string)))))

(defonce ^{:doc "Default FileSystem"} default-fs (FileSystems/getDefault))
(defonce ^{:doc "Default FileSystem file name separator"} filename-seperator (.getSeparator default-fs))
(defonce ^{:private true}  empty-string-array (into-array String [])) ; for getPath

(defn- ^Path seq-to-path
  "Convert sequence of things to a path, where each element of sequence is element of path.
  This is done by taking the string representation of each thing in the sequence."
  [s]
  (.getPath default-fs (str (first s)) (into-array String (map str (rest s)))))

(defmulti ^Path as-path "Coerce object to Path. Does not perform tilde expansion. See also 'to-path'." class)
(defmethod ^Path as-path String [x] (.getPath default-fs x empty-string-array))
(defmethod ^Path as-path File [x] (.toPath x))
(defmethod ^Path as-path Path [x] x)  
(defmethod ^Path as-path clojure.lang.ISeq [x] (seq-to-path x))
(defmethod ^Path as-path URI [x] (Paths/get x))
  
(defn path? "Return true if x is a java.nio.file.Path, false otherwise."
  [x] (instance? Path x))

;; Options translators.  Convert options in option map to values expected by APIs
(defn- encoding "Translate encoding option to Charset" [x]
  (cond (string? x)
        (Charset/forName x)
        :else x))

(defn- follow "Translate follow option into LinkOptions array" [x]
  (if x
    (into-array LinkOption [])
    (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])))

(defn- posix-file-permission-enum
  "Return the the java java.nio.file.attribute.PosixFilePermission enum
   corresponding to the supplied keword, or return false if keyword does not map to a valid enum.
   Keywords should be from the set of:
      :{group,owner,others}-{read,write-execute} if x is a keyword of the correct format.
   From that we return PosixFilePermission/GROUP_EXECUTE, and so on."
  [x]
  (and (keyword? x)
       (let [tokens (map clojure.string/upper-case (clojure.string/split (name x) #"-"))]
         (and (= (count tokens) 2)
              (cl-find (first tokens) ["GROUP" "OWNER" "OTHERS"])
              (cl-find (second tokens) ["READ" "WRITE" "EXECUTE"])
              (PosixFilePermission/valueOf (str (first tokens) "_" (second tokens)))))))

(defn- positive-file-permissions-in-map
  "For all map entries in a map whose keys are posix file permission keywords,
   and whose values are logical true (i.e. positive, not negative).
   Return a sequence of PosixFilePermission objects or an empty sequence if there aren't any."
  [m]
  {:pre (map? m)}
  ;; N.B. this is not the same as jdt.core/each
  ;; *TODO*/*FINISH*:
  ;; See if we have a convenience equivalent for this in Utils.lsp or in core clojure libraries
  (filter identity (map (fn [e] (and (val e) (posix-file-permission-enum (key e)))) (seq m))))

(defn- file-attribute-permissions
  "Process a :permissions options value as described in '(options-help :permissions)'.
   Return a FileAttribute<Set<PosixFilePermissions>> instance.
   Presently invalid keywords corresponding to permissions are silently ignored."
  [optval]                              ;value of :permissions option
  (cond (string? optval)                ;"rwxr-xr-x"
        (let [permset (PosixFilePermissions/fromString optval)]
          (PosixFilePermissions/asFileAttribute permset))
        (map? optval)           ;keyword/value pairings, e.g. :group-read true
        (let [permset (into #{} (positive-file-permissions-in-map optval))]
          (PosixFilePermissions/asFileAttribute permset))
        (seq optval) ;keywords sequence, e.g. [:group-read :user-read] #{:owner-read}
        (let [permset (into #{} (map posix-file-permission-enum (seq optval)))]
          (PosixFilePermissions/asFileAttribute permset))
        :else
        (throw (Exception. (str "Unable to process permissions specification: " optval)))))

(defn- file-attribute
  "Interpret map-entries that represents FileAttribute situations, or return nil.
   TBD: Whether it works to specify the same keyword twice with non-overlapping attribute values."   
  [e]
  (let [k (key e)]
    (cond (= k :permissions)
          (file-attribute-permissions (val e))
          ;; *FINISH* *TODO*: Principals/Acls/Timess
          )))

(defn- file-attributes "Convert eligible map options into a vector of FileAttribute<T> objects."
  [opts]
  (into-array FileAttribute
              (filterv not-nil? (mapv file-attribute (seq opts)))))

;; Options keywords and default values
(def                                    ;defonce
  ^{:doc
    "Keys representing options recognized in this module.
     Not all functions take all options.  This variable is for documentation purposes."}
  valid-option-keys
  {:encoding "string or Charset, converted to charset, specifying byte<->char encodings, default UTF-8"
   :no-tilde "if true do not perform Bash style tilde expansion in pathnames, default false"
   :follow   "if true, follow Symbolic links, otherwise do not follow them, default true"
   :accept   "predicate of one argument, a path, return true if path should be kept, false if discarded,
             default: none."
   :glob     "A string specifying a globbing pattern as documented in 
             documented in java.nio.file.FileSystem.getPathMatcher() or nil/absent.  Default: none.
             An exception is thrown if the pattern is invalid.
             Globbing is case sensitive on the default Linux filesystem."
   :regex    "A string specifying a regex pattern as documented in 
             documented in java.nio.file.FileSystem.getPathMatcher() or nil/absent.  Default: none.
             An exception is thrown if the regex is invalid."
   :permissions 
             "Indicates one or more posix file permissions.
             If present these are converted into java.nio.file.attribute.FileAttribute values
             for APIs that accept FileAttribute arguments.

             The value for this the :permissions keyword can take one of three forms:
             (1) A string of nine characters similar to \"rwxr-xr-x\",
             (2) A map of keywords whose names are derived from the following set:
                 ':{owner,group,other}-{read,write,execute}'
                 Values corresponding to the above are logical true or logical false.
             (3) A sequence of keywords are derived from the same set as (2)
                 but whose values are implicitly true.

             Examples:
                 :permissions \"rwx------\"
                 :permissions {:group-read true :owner-read true :owner-write false}
                 :permissions '(:group-read :owner-read :owner-write)

             In all cases, permissions which are not explicitly specified will default to suitable
             file system defaults, possibly controlled by 'umask' or other modes.

             File attribute keywords that are not valid are presently ignored, so make sure you don't say
             :user-read, for example, when the correct keyword is :owner-read.

             Note that file system controls applied to the parent directory
             may override attempts at specifying permissions on newly created
             or updated file system entities."
   })

(defn options-help
  "Describe valid-option-keys in human readable form.
   If keys are specified only the documentation for those options is printed.
   E.g. (options-help :encoding :no-tilde)"
  [& keys]
  (doseq [e valid-option-keys]
    (when (or (nil? keys) (some #(= (key e) %) keys))
      (println (key e))
      (doseq [line (clojure.string/split-lines (val e))]
        (println "   " (clojure.string/trim line))))))

(def                                    ;defonce
  ^{:doc
    "Default options for valid-option-keys if a function needs it and it wasn't specified.
     Options not specified in this map default to false."}
  default-option-values
  {:encoding (encoding "UTF-8")
   :follow true})

;; Path Transformations
(defn ^Path expand [x suppress-tilde]
  "Ensure that argument is coerced to a path,
  tilde-expanded if necessary unless suppress-tilde is true.
  Only string arguments can be tilde exapnded."
  (cond (instance? Path x)
        x                               ;*TODO* (as-path "~/.bashrc") works, we should figure out how to expand it here.
        (and (string? x) (not suppress-tilde))
        (as-path (bash-tilde-expansion x))
        :else (as-path x)))

;;
;; Single path predicates (pred path optionmap), second arg optional.
;;

;; This macro isn't quite perfected...  The resulting doc string is missing and the arg names are horrible.
;; I was also going to write a 'wrap1' that just did the tilde, not the follow.
#_
(defmacro ^:private wrap-follow [predicate-name java-name]
  (let [qname# (symbol (str "Files/" java-name))
        doc# (str "Wrapper for java.nio.file.Files/" java-name ". Opts: :no-tilde, :follow.")]
    `(defn ^{:doc ~doc#}
       ~predicate-name
       ([x#] (~predicate-name x# nil))
       ([x# opts#]
          (let [opts# (if opts# (merge default-option-values opts#) default-option-values)]
            (~qname# (expand x# (:no-tilde opts#)) (follow (:follow opts#))))))))
#_
(do
  (println (clojure.walk/macroexpand-all '(wrap-follow dir? isDirectory)))
  (println (macroexpand '(wrap-follow dir? isDirectory)))
  (wrap-follow dir? isDirectory))


(defn exists?
  "Return true if x (a path coercible thing) exists, false otherwise.
   This is a wrapper around java.nio.file.Files/exists().
  Options:
    :follow true/false, whether or not to follow symbolic link if x is a link.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (exists? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/exists (expand x (:no-tilde opts)) (follow (:follow opts))))))

(defn not-exists?
  "Return true if x (a path coercible thing) can be confirmed not to exist, false otherwise.
   This is not a true complement for 'exists?', a false value means we couldn't confirm it doesn't exist.
   This is a wrapper around java.nio.file.Files/notExists().
  Options:
    :follow true/false, whether or not to follow symbolic link if x is a link.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (not-exists? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/notExists (expand x (:no-tilde opts)) (follow (:follow opts))))))

(defn dir?
  "Return true if x (a path coercible thing) exists and is a directory, false otherwise.
   This is a wrapper around java.nio.file.Files/isDirectory().
  Options:
    :follow true/false, whether or not to follow symbolic link if x is a link.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (dir? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/isDirectory (expand x (:no-tilde opts)) (follow (:follow opts))))))

(defn file?
  "Return true if x (a path coercible thing) exists
   and is a regular file (non-symbolic link, non directory), false otherwise.
   This function WILL follow symbolic link chains unless {:follow false} is specified.
   This is a wrapper around java.nio.file.Files/isRegularFile().
  Options:
    :follow true/false, whether or not to follow symbolic link if x is a link.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (file? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/isRegularFile (expand x (:no-tilde opts)) (follow (:follow opts))))))

(defn link?
  "Return true if x (a path coercible thing) exists and is a symbolic link, false otherwise.
   This is a wrapper around java.nio.file.Files/isSymbolicLink().
   Whether it follows symbolic links is undocumented.
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (link? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/isSymbolicLink (expand x (:no-tilde opts))))))

(defn exe?
  "Return true if x (a path coercible thing) exists and the jvm can execute the file, false otherwise.
   This is a wrapper around java.nio.file.Files/isExecutable().
   Whether it follows symbolic links is undocumented.
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (exe? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/isExecutable (expand x (:no-tilde opts))))))

(defn hidden?
  "Return true if x (a path coercible thing) exists and is considered hidden, false otherwise.
   This is a wrapper around java.nio.file.Files/isHidden().
   Whether it follows symbolic links is undocumented.
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (hidden? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/isHidden (expand x (:no-tilde opts))))))

(defn readable?
  "Return true if x (a path coercible thing) exists and is readable, false otherwise.
   This is a wrapper around java.nio.file.Files/isReadable().
   Whether it follows symbolic links is undocumented.
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (readable? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/isReadable (expand x (:no-tilde opts))))))

(defn writable?
  "Return true if x (a path coercible thing) exists and is writable, false otherwise.
   This is a wrapper around java.nio.file.Files/isWritable().
   Whether it follows symbolic links is undocumented.
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (writable? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/isWritable (expand x (:no-tilde opts))))))

;;
;; Two path predicates: (pred path1 path2 optionmap)
;;

(defn same-file?
  "Return true if two path coercible things represent the same file, false otherwise.
   This is a wrapper around java.nio.file.Files/isSameFile().
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([p1 p2] (same-file? p1 p2 nil))
  ([p1 p2 opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)
           no-tilde (:no-tilde opts)]
       (Files/isSameFile (expand p1 no-tilde) (expand p2 no-tilde)))))


;;
;; File Accessors: access information about a given file identified by a path.
;;
;; We may want a (with-attributes ...) macro for reading/writing attributes in bulk operations
;; using java AttributeView interfaces.  
;;
;; On Linux, (supported-file-attribute-views) => (basic owner user unix dos posix)

(defn owner
  "Return a string naming the owner of the file, or nil if the owner information is unavailable.
   The file must exist or an exception is thrown.
   This is a wrapper around java.nio.file.Files/getOwner().
  Options:
    :follow true/false, whether or not to follow symbolic link if x is a link.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (owner x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (.getName (Files/getOwner (expand x (:no-tilde opts)) (follow (:follow opts)))))))

(defn group
  "Return a string naming the owner of the file, or nil if the owner information is unavailable.
   The file must exist or an exception is thrown.
   This is a wrapper around java.nio.file.Files/getAttribute().
  Options:
    :follow true/false, whether or not to follow symbolic link if x is a link.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (group x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (.getName (Files/getAttribute (expand x (:no-tilde opts)) "posix:group" (follow (:follow opts)))))))

(defn ctime
  "Return a FileTime with the creation modification time of the file.
   The file must exist or an exception is thrown.
   This is a wrapper around java.nio.file.Files/getAttribute().
  Options:
    :follow true/false, whether or not to follow symbolic link if x is a link.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (ctime x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/getAttribute (expand x (:no-tilde opts)) "unix:creationTime" (follow (:follow opts))))))

(defn mtime 
  "Return a FileTime with the last modification time of the file.
   The file must exist or an exception is thrown.
   This is a wrapper around java.nio.file.Files/getLastModifiedTime().
  Options:
    :follow true/false, whether or not to follow symbolic link if x is a link.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (mtime x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/getLastModifiedTime (expand x (:no-tilde opts)) (follow (:follow opts))))))

(defn atime 
  "Return a FileTime with the last access time of the file.
   The file must exist or an exception is thrown.
   This is a wrapper around java.nio.file.Files/getLastModifiedTime().
  Options:
    :follow true/false, whether or not to follow symbolic link if x is a link.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (atime x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/getAttribute (expand x (:no-tilde opts)) "unix:lastAccessTime" (follow (:follow opts))))))

(defn file-key
  "Return an object that uniquely identifies a file (an inode, perhaps), or nil if no key can be obtained.
   The file must exist or an exception is thrown.
   This is a wrapper around java.nio.file.Files/getAttribute().
  Options:
    :follow true/false, whether or not to follow symbolic link if x is a link.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (file-key x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/getAttribute (expand x (:no-tilde opts)) "unix:fileKey" (follow (:follow opts))))))

(defn size
  "Return the size of a file in bytes. The file must exist or an exception is thrown.
   This is a wrapper around java.nio.file.Files/size().
   Whether it follows symbolic links is undocumented.
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (size x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/size (expand x (:no-tilde opts))))))

(defn file-store
  "Return the FileStore associated with a file. The file must exist or an exception is thrown.
   This is a wrapper around java.nio.file.Files/getFileStore().
   Whether it follows symbolic links is undocumented.
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (file-store x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/getFileStore (expand x (:no-tilde opts))))))

(defn perm-keys
  "Return a set of PosixFilePermission-like keywords (e.g. :GROUP_READ) for file permissions.
   The file must exist or an exception is thrown.  Remember clojure keywords are case sensitive.
   This is a wrapper around java.nio.file.Files/getPosixFilePermissions().
  Options:
    :follow true/false, whether or not to follow symbolic link if x is a link.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (perm-keys x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (into #{}
             (map (fn [perm] (keyword (str perm)))
                  (Files/getPosixFilePermissions (expand x (:no-tilde opts)) (follow (:follow opts))))))))

(defn perm-string
  "Return a unix style string of file permissions, e.g. \"rwxr-xr-x\" for a file.
   The file must exist or an exception is thrown. 
   This is a wrapper around java.nio.file.Files/getPosixFilePermissions().
  Options:
    :follow true/false, whether or not to follow symbolic link if x is a link.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (perm-string x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (PosixFilePermissions/toString
        (Files/getPosixFilePermissions (expand x (:no-tilde opts)) (follow (:follow opts)))))))

(defn content-type
  "Return a string describing content type of the file or null if the type can't be detected.
   This method may probe content and therefore be slow.
   The file must exist or an exception is thrown. 
   This is a wrapper around java.nio.file.Files/probeContentType().
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (content-type x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/probeContentType (expand x (:no-tilde opts))))))

(defn read-bytes
  "Return a (mutable) Java byte array (i.e. byte[], not Byte[]) of file content.
   The file must exist or an exception is thrown. 
   This is a wrapper around java.nio.file.Files/readAllBytes().
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (read-bytes x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/readAllBytes (expand x (:no-tilde opts))))))

(defn read-lines
  "Return a (mutable, NOT LAZY) list of strings representing file content.

   *TODO*: think about making this a lazy sequence instead of a wrapper for readAllLines(),
   which is just too opposite the goals of clojure to expose.  
 
   The file must exist or an exception is thrown. 
   This is a wrapper around java.nio.file.Files/readAllLines().
  Options:
    :encoding a string or Charset that represents the desired encoding, defaults to UTF-8.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (read-lines x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/readAllLines (expand x (:no-tilde opts)) (encoding (:encoding opts))))))

(defn read-symlink
  "Return a Path that represents the target of a symbolic link.
   The file must exist AND be a symbolic link or an exception is thrown. 
   This is a wrapper around java.nio.file.Files/readSymbolicLink().
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (read-symlink x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/readSymbolicLink (expand x (:no-tilde opts))))))

;; *TBD* readAttributes()


;;
;; DirectoryStream APIs.
;;
;; All DirectoryStream wrappers work in terms of lazy sequences.  To proactively close partially traversed
;; directory stream iterators, you should wrap the calls in with-open, but if you don't the DirectoryStream
;; will be closed when the iterator encounters the last element or the DirectoryStream is GC'd.
;; 

(defn glob-accept-fn
  "Given a PathMatcher or a glob string we can turn into a PathMatcher with syntax 'glob:',
  return a function that takes one argument, a Path, and returns true if the PathMatcher matches the
  filename portion of the path and false otherwise.  If the input argument is nil, return nil."
  [glob]
  (and glob
       (let [pm (if (instance? PathMatcher glob)
                  glob
                  (.getPathMatcher default-fs (str "glob:" glob)))]
         (fn [path] (.matches pm (.getFileName path))))))

(defn regex-accept-fn
  "Given a PathMatcher or a regex string we can turn into a PathMatcher with syntax 'regex:',
  return a function that takes one argument, a Path, and returns true if the PathMatcher matches the
  filename portion of the path and false otherwise.   If the input argument is nil, return nil."
  [regex-or-pm]
  (and regex-or-pm
       (let [pm (if (instance? PathMatcher regex-or-pm)
                  regex-or-pm
                  (.getPathMatcher default-fs (str "regex:" regex-or-pm)))]
         (fn [path] (.matches pm (.getFileName path))))))

(defn- directory-stream-seq
  "Returns a lasy sequence of Paths respresenting children of a directory via the DirectoryStream API.
   Ensures that the underlying DirectoryStream is closed either when the iterator reaches the last element
   (a proactive close), or when the DirectoryStream is garbage collected. (a lazy close).
   The most proactive way to close the underlying DirectoryStream is to wrap the
   scope of the DirectoryStream and lazy sequence realization in a 'with-open'."
  ;; The no-filter variant
  ([^DirectoryStream dir-stream]
     (ensure-close dir-stream)          ;will close when GC'd
     (let [iterator (.iterator dir-stream)
           iterator-fn (fn iterator-fn []
                         (if (.hasNext iterator)
                           (cons (.next iterator)
                                 (lazy-seq (iterator-fn)))
                           (.close dir-stream)))]
       (lazy-seq (iterator-fn)))))

;; *TODO*: walkFileTree, FileVisitor.visitFile is called with BasicFileAttributes. Overload
;; our methods that can be derived from BasicFileAttributes to use those instead of looking them up
;; so we are more efficient.  E.g. size() can be derived directly from the passed BasicFileAttributes

(defn ^DirectoryStream directory-stream
  "Return a DirectoryStream on a path-coercible spec.
   An exception is thrown if the path spec does not specify a directory.
   This is a wrapper around java.nio.file.Files.newDirectoryStream().
   Consider using 'children' or other functions instead, which wrap DirectoryStream objects in close-safe
   and lazy sequences.
  Options:
    :accept f, species a predicate 'f' that takes a path argument, and returns logical true if the the path
               should be included in the result, and logical false if the path should not be in the result.
    :glob pattern or nil, return only those files that match the specified globbing pattern as
          documented in java.nio.file.FileSystem.getPathMatcher().  Default is nil.
          An exception is thrown if the pattern is invalid.
          Globbing is case sensitive on the default Linux filesystem.
    :regex A string specifying a regex pattern or nil/absent.  Default: none.
          An exception is thrown if the regex is invalid.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (directory-stream x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)
           dir (expand x (:no-tilde opts))
           accept-fn (merge-predicates (:accept opts)
                                       (glob-accept-fn (:glob opts))
                                       (regex-accept-fn (:regex opts)))]
       (cond accept-fn (Files/newDirectoryStream
                        dir (proxy [DirectoryStream$Filter] [] (accept [path] (accept-fn path))))
             :else (Files/newDirectoryStream dir)))))

;; *TODO*: allow java.util.regex.Pattern objects for :regex as well as strings.
;; Maybe skip the whole PathMatcher thing in that case, otherwise we have to extract the regex
;; and create a new pattern indirectly via a path matcher (or create our own regex pathmatcher subtype)

(defn children
  "A DirectoryStream interface that returns a lazy sequence of all children in a directory.
   An exception is thrown if the path spec does not specify a directory.
   The resulting sequence will not contain any pseudo paths such as '.' or '..',
   but may include hidden files.  See also 'parent'.
  Options:
    :accept f, species a predicate 'f' that takes a path argument, and returns logical true if the the path
               should be included in the result, and logical false if the path should not be in the result.
    :glob pattern or nil, return only those files that match the specified globbing pattern as
          documented in java.nio.file.FileSystem.getPathMatcher().  Default is nil.
          An exception is thrown if the pattern is invalid.
    :regex A string specifying a regex pattern or nil/absent.  Default: none.
          An exception is thrown if the regex is invalid.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (children x nil))
  ([x opts]
     (if-let [directory-stream (directory-stream x opts)]
       (directory-stream-seq directory-stream))))

(defn file-children
  "Basically a convenience wrapper around 'children' that throws in a 'file?' acceptance predicate.
   Return a lazy sequence of paths that are children of a directory for which 'file?' is true.
   An exception is thrown if the path spec does not specify a directory.
   Return nil if there aren't any children.
  Options:
    :accept f, species a predicate 'f' that takes a path argument, and returns logical true if the the path
               should be included in the result, and logical false if the path should not be in the result.
    :glob pattern or nil, return only those files that match the specified globbing pattern as
          documented in java.nio.file.FileSystem.getPathMatcher().  Default is nil.
          An exception is thrown if the pattern is invalid.
    :regex A string specifying a regex pattern or nil/absent.  Default: none.
          An exception is thrown if the regex is invalid.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (file-children x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (children (expand x (:no-tilde opts)) (merge opts {:accept file?})))))

(defn dir-children
  "Basically a convenience wrapper around 'children' that throws in a 'dir?' acceptance predicate.
   Return a lazy sequence of paths that are children of a directory for which 'dir?' is true.
   An exception is thrown if the path spec does not specify a directory.
   Return nil if there aren't any children.
  Options:
    :accept f, species a predicate 'f' that takes a path argument, and returns logical true if the the path
               should be included in the result, and logical false if the path should not be in the result.
    :glob pattern or nil, return only those files that match the specified globbing pattern as
          documented in java.nio.file.FileSystem.getPathMatcher().  Default is nil.
          An exception is thrown if the pattern is invalid.
    :regex A string specifying a regex pattern or nil/absent.  Default: none.
          An exception is thrown if the regex is invalid.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (dir-children x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (children (expand x (:no-tilde opts)) (merge opts {:accept dir?})))))

;; For FileAttributeView stuff, probably best to have a (with-file-attributes ...) macro of some kind
;; to read and write the different properties.


;;;
;;; File mutators (creation, deletion, copies, updates, attributes, etc)
;;;

;; Clojure.java.io has some flexible copy stuff we don't want to reproduce if we can avoid it.
;; Inputs=(InputStream,Reader,File,byte[],String)
;; Outputs=(OutputStream,Writer,File)
;; Options=(buffer-size,encoding)

;; java.nio.file.Files supports
;; Inputs=(Path,InputStream)
;; Outputs=(Path,OutputStream)
;; Options=(ATOMIC_MOVE, COPY_ATTRIBUTES, REPLACE_EXISTING, NOFOLLOW_LINKS)

;; *TODO* copy APIs

(defn create-directory
  "Create a single directory.  Wrapper for java.nio.file.Files.createDirectory().
  Options:
    :permissions as documented in '(options-help :permissions)'.
    :no-tilde true/false, whether or not to do tilde expansion on path."
  ([path] (create-directory path nil))
  ([path opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/createDirectory (expand path (:no-tilde opts)) (file-attributes opts)))))

;; *FINISH*: test create-directory and :permissions interactions.

;; *FINISH*: rest of file apis, like create-directories and so on.



;;
;; FileSystem APIs
;;

(defn supported-file-attribute-views
  "Return the supported FileAttributeView names (strings) that can be used with Files.getAttributes()
  and similar functions on this platform.  If no FileSystem is specified, use the default FileSystem."
  ([] (supported-file-attribute-views default-fs))
  ([file-system] (seq (.supportedFileAttributeViews file-system))))



;;
;; Path APIs
;;
;; The Path interface is pretty easy to use, there's *almost* no point in wrapping it.
;; It might be useful to wrap it such that the returns are strings for predicate usage,
;; but "=" invokes equals() on Path objects just as easily as Strings.
;;
;; Still we get tilde expansion and string conversion to path arguments, that makes the user's
;; life easier, and that's what this module is about.  Our method names are generally shorter too.
;;
;; Path has a lot of methods, most of them are omitted here, use them directly if you need to.
;; What is here is mostly with an eye toward (unix-like) 'find' functionality.
;;

(defn ^Path parent
  "Return the parent path of a file or nil if there is no parent.
   Does not normalize path components such as '.' or '..', call normalize() if you need that done.
   If you want a string representation of the path instead of a Path instance, call 'str' on the result.
   This is a wrapper around java.nio.file.Path/getParent().
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (parent x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (.getParent (expand x (:no-tilde opts))))))

(defn ^Path file-name
  "Return the file name (i.e. last component of the path) as a Path. (Use 'str' to get a string).
   This is a wrapper around java.nio.file.Path/getFileName().
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (file-name x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (.getFileName (expand x (:no-tilde opts))))))

(defn ^Path root
  "Returns the root component of (path coercible) x as a path object,
  or nil if the path does not have a root component.  Probably more useful as a predicate than
  fetching the '/' on a path as a path. Wraps java.nio.file.Path/getRoot()
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (root x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (.getRoot (expand x (:no-tilde opts))))))

(defn absolute?
  "Return true if the path is absolute, false otherwise.
   This is a wrapper around java.nio.file.Path/isAbsolute().
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (absolute? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (.isAbsolute (expand x (:no-tilde opts))))))

(defn to-seq
  "Return a sequence of Path components within (path-coercible) x.
  Wraps java.nio.file.Path/iterator().
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (to-seq x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (iterator-seq (.iterator (expand x (:no-tilde opts)))))))

(defn ^Path normalize  
  "Return a path with redundant elements (such as '.') eliminated.
   This is a wrapper around java.nio.file.Path/normalize().
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (normalize x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (.normalize (expand x (:no-tilde opts))))))

(defn ^Path relativize
  "Attempts to construct a relative path to (path coercible) 'target' that when resolved against
  (path coercible) 'source' yields a path that can be used to locate the same file as 'target'.
  (I.e. finds a path from source to target in the filesystem.)

  If the paths are equal, an empty path is returned.
  Throws an exception if the target path cannot be relativized against source.
  See javadoc for caveats related to presence of root path components.
  Results are implementation dependent when symbolic links are involved.
  E.g. (relativize \"a/b\" \"a/b/c/d\") => #<Unixpath c/d>.
  A wrapper around java.nio.file.Path/relativize().
  Options:
    :no-tilde true/false, whether or not to do tilde expansion on paths."
  ([source target] (relativize source target nil))
  ([source target opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)
           ^Path target (expand target (:no-tilde opts))]
       (.relativize (expand source (:no-tilde opts)) target))))

(defn ^Path path-resolve
  "Supposedly this is the inverse of 'relativitize'.
   Attempts to merge (path coercible) source and target to yield a path, preferably based on source,
   to the target.  In other words it joins target with source, attempting to treat source as a directory,
   such that the resulting path ends with target.

   Example: (resolve \"/a/b\" \"./b/c\") => #<UnixPath /a/b/./b/c>

   If the target path has a root component results are implementation dependent (i.e. unspecified).
   If the target is an absolute path, returns target.  If target is empty, returns source.
   Throws InvalidPathException if string cannot be converted to a path.

   A wrapper around java.nio.file.Path/resolve().  It's not called 'resolve' because that would conflict
   with a namespace function of the same name in clojure.core.

  Options:
    :no-tilde true/false, whether or not to do tilde expansion on paths."
  ([source target] (path-resolve source target nil))
  ([source target opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)
           ^Path target (expand target (:no-tilde opts))]
       (.resolve (expand source (:no-tilde opts)) target))))

(defn ^Path resolve-sibling
  "Resolves (path coercible) target against (path coercible) source's parent path.
  Useful where a file name needs to be replaced with another file name.
  If source path does not have a parent path or target is absolute then return
  target. If target is an empty path return the source path parent, or if there is no parent, the empty path.

  Example: (resolve-sibling \"dir1/dir2/foo\" \"bar\") => #<UnixPath dir1/dir2/bar>
  A wrapper around java.nio.file.Path/resolveSibling().
  Throws InvalidPathException if string cannot be converted to a path.
  Options:
    :no-tilde true/false, whether or not to do tilde expansion on paths."
  ([source target] (resolve-sibling source target nil))
  ([source target opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)
           ^Path target (expand target (:no-tilde opts))]
       (.resolveSibling (expand source (:no-tilde opts)) target))))

(defn ^Path abs-path
  "If (path coercible) path is absolute, returns path.
   Otherwise attempts to resolve the path by looking at implementation
   dependent context such as a process' current working directory (e.g. java system property 'user.dir').
   May throw an IOError if the file system is not accessible.
   Wrapper around java.nio.file.Path/toAbsolutePath().
  Options:
    :no-tilde true/false, whether or not to do tilde expansion on path."
  ([path] (abs-path path nil))
  ([path opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (.toAbsolutePath (expand path (:no-tilde opts))))))

(defn ^Path real-path
  "Attempt to derive the real pathname based on existing filesystem structure.  For example,
  if case insensitive file system names are present, the returned path components will exactly match
  the corresponding places in the file system. This is basically a (->> path normalize abs-path <massage>)
  behavior. This is a wrapper around java.nio.file.Path/toRealPath().
  Throws IOException if the file indicated by the (path coercible) argument doesn't exist.
  Options:
    :follow true/false, whether or not to follow symbolic link if x is a link.
    :no-tilde true/false, whether or not to do tilde expansion on path."
  ([path] (real-path path nil))
  ([path opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (.toRealPath (expand path (:no-tilde opts)) (follow (:follow opts))))))

(defn ^Path sub-path
  "Returns a relative path that is a subsequence of name elements of (path coercible) path.
  (You could also call 'as-path' on a subsequence of 'to-seq' of path, but this is more efficient.)

  start - index of first path element, inclusive (closest to root, index 0).
  end   - index of last path element, exclusive.

  E.g. (subpath \"a/b/c\" 1 2) => #<UnixPath b>

  Options:
    :no-tilde true/false, whether or not to do tilde expansion on path."
  ([path ^Integer start ^Integer end] (sub-path path start end nil))
  ([path ^Integer start ^Integer end opts] 
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (.subpath (expand path (:no-tilde opts)) start end))))

(defn starts-with
  "Return true if (path coercible) 'path' starts with the same name components in (path coercible)
   'other', false otherwise.
   Wrapper around java.nio.file.Path/startsWith().
  Options:
    :no-tilde true/false, whether or not to do tilde expansion on arguments."
  ([path other] (starts-with path other nil))
  ([path other opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (.startsWith (expand path (:no-tilde opts)) (expand other (:no-tilde opts))))))

(defn ends-with
  "Return true if (path coercible) 'path' ends with the same name components in (path coercible)
   'other', false otherwise.
   Wrapper around java.nio.file.Path/endsWith().
  Options:
    :no-tilde true/false, whether or not to do tilde expansion on arguments."
  ([path other] (ends-with path other nil))
  ([path other opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (.endsWith (expand path (:no-tilde opts)) (expand other (:no-tilde opts))))))
       
;; compare/compareTo, note that
;; (clojure.core/compare (as-path x) (as-path y)) will invoke compareTo() in the java Comparable interface.
;; as '=' invokes equals().

(defn ^URI to-uri
  "Return an URI that represents a path.
   This is a wrapper around java.nio.file.Path/toUri().
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (to-uri x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (.toUri (expand x (:no-tilde opts))))))

(defn ^File to-file
  "Return an File that represents a path.
   This is a wrapper around java.nio.file.Path/toFile().
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (to-file x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (.toFile (expand x (:no-tilde opts))))))

(defn ^Path to-path
  "Coerce input to a path.  This wraps 'as-path' in that it knows how to observe some options
   like most of the other APIs in this module, including default behavior for tilde expansion.
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (to-path x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (let [no-tilde (:no-tilde opts)]
         (if (string? x)
           (as-path (expand x no-tilde))
           (if (instance? Path x)
             (if (.startsWith x "~")
               (expand (str x) no-tilde)
               x)
             (as-path x)))))))

(extend-protocol Coercions
  Path
  (as-url [p] (.toUri p))
  (as-file [p] (.toFile p)))
