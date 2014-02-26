(ns jdt.easyfs
  "Clojure functions for easy file system manipulation. Requires Java 7."
  (:use jdt.core)
  (:use [jdt.shell :only [bash-tilde-expansion]])
  (:use [jdt.closer :only [ensure-close]])
  (:use clojure.java.io)
  (:import java.io.File)
  (:import (java.nio.file Path Files FileSystems FileSystem LinkOption))
  (:import java.nio.charset.Charset)
  )

(defonce ^{:doc "Default FileSystem"} default-fs (FileSystems/getDefault))
(defonce ^{:doc "Default FileSystem file name separator"} filename-seperator (.getSeparator default-fs))
(defonce ^{:private true}  empty-string-array (into-array String [])) ; for getPath

(defn- seq-to-path
  "Convert sequence of things to a path, where each element of sequence is element of path."
  [x]
  (let [seqtypes (into #{} (map class x))
        first-type (first seqtypes)]
    (cond (> (count seqtypes) 1)
          (throw (Exception. (str "Can't handle inputs of multiple types at this time for: " x)))
          (isa? first-type String)
          (.getPath default-fs (first x) (into-array String (rest x)))
          :else (throw (Exception. (str "Can't unable input of type " first-type " for: " x))))))

(defmulti ^Path as-path "Coerce object to Path" class)
(defmethod ^Path as-path String [x] (.getPath default-fs x empty-string-array))
(defmethod ^Path as-path File [x] (.toPath x))
(defmethod ^Path as-path Path [x] x)  
(defmethod ^Path as-path clojure.lang.ISeq [x] (seq-to-path x))
  
;; Options translators.  Convert options in option map to values expected by APIs
(defn- encoding "Translate encoding option to Charset" [x]
  (cond (string? x)
        (Charset/forName x)
        :else x))

(defn- follow "Translate follow option into LinkOptions array" [x]
  (if x
    (into-array LinkOption [])
    (into-array LinkOption [LinkOption/NOFOLLOW_LINKS])))

;; Options keywords and default values
(def                                    ;defonce
  ^{:doc
    "Keys representing options recognized in this module.
     Not all functions take all options."}
  valid-option-keys
  [:encoding                            ;string or Charset, converted to charset
   :no-tilde                            ;true/false, no conversion
   :follow                              ;true/false, converted to LinkOption array
   ])

(def                                    ;defonce
  ^{:doc
    "Default options for valid-option-keys if a function needs it and it wasn't specified.
     Options not specified in this map default to false."}
  default-option-values
  {:encoding (encoding "UTF-8")
   :follow true})

;; Path Transformations
(defn- expand [x suppress-tilde]
  "Ensure that argument is coerced to a path,
  tilde-expanded if necessary unless suppress-tilde is true.
  Only string arguments can be tilde exapnded."
  (cond (instance? Path x)
        x
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
  "Return true if x exists, false otherwise.
   This is a wrapper around java.nio.file.Files/exists().
  Options:
    :follow true/false, whether or not to follow symbolic link if x is a link.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (exists? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/exists (expand x (:no-tilde opts)) (follow (:follow opts))))))

(defn not-exists?
  "Return true if x can be confirmed not to exist, false otherwise.
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
  "Return true if x exists and is a directory, false otherwise.
   This is a wrapper around java.nio.file.Files/isDirectory().
  Options:
    :follow true/false, whether or not to follow symbolic link if x is a link.
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (dir? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/isDirectory (expand x (:no-tilde opts)) (follow (:follow opts))))))

(defn file?
  "Return true if x exists and is a regular file (non-symbolic link, non directory), false otherwise.
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
  "Return true if x exists and is a symbolic link, false otherwise.
   This is a wrapper around java.nio.file.Files/isSymbolicLink().
   Whether it follows symbolic links is undocumented.
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (link? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/isSymbolicLink (expand x (:no-tilde opts))))))

(defn exe?
  "Return true if x exists and whether the jvm can execute the file, false otherwise.
   This is a wrapper around java.nio.file.Files/isExecutable().
   Whether it follows symbolic links is undocumented.
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (exe? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/isExecutable (expand x (:no-tilde opts))))))

(defn hidden?
  "Return true if x exists and is considered hidden, false otherwise.
   This is a wrapper around java.nio.file.Files/isHidden().
   Whether it follows symbolic links is undocumented.
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (hidden? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/isHidden (expand x (:no-tilde opts))))))

(defn readable?
  "Return true if x exists and is readable, false otherwise.
   This is a wrapper around java.nio.file.Files/isReadable().
   Whether it follows symbolic links is undocumented.
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (readable? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/isReadable (expand x (:no-tilde opts))))))

(defn writable?
  "Return true if x exists and is writable, false otherwise.
   This is a wrapper around java.nio.file.Files/isWritable().
   Whether it follows symbolic links is undocumented.
  Options:
    :no-tilde true/false, whether or not to do tilde expansion."
  ([x] (writable? x nil))
  ([x opts]
     (let [opts (if opts (merge default-option-values opts) default-option-values)]
       (Files/isWritable (expand x (:no-tilde opts))))))

