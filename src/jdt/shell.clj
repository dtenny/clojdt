;; EMACS NOTE: M-x cider-jack-in (C-C M-j)
;; to get the emacs<->clojure ball rolling on this file.
;; C-z to switch between repl and file.

(ns ^{:doc "Operating system shell environment wrapping native/host commands"}
  jdt.shell
  (:use jdt.core)
  (:use clojure.java.io)                ;copy, input-stream, output-stream, etc
  (:use me.raynes.conch)
  (:import java.io.File)
  )

(defn- bash-user-homedir-token-expansion
  "Helper routine to expand ~[user] appropriately."
  [string]                              ;~joe
  {:pre (= (first string) \=)}
  (let [user (subs string 1)           ;"" or "joe"
        homedir (System/getProperty "user.home")] ; /home/joe
    (if (= "" user)
      homedir
      (let [myusername (System/getProperty "user.name")]
        ;; Replace /home/xxx with 'joe'
        (.replaceFirst homedir (str "/" myusername) (str "/" user))))))

(defn user-homedir
  "If a user name (string) is supplied, return the home directory of the indicated user (presently a hack.)
  If no name is specified, supply the home directory of the user owning the calling process."
  [& [user]]
  (let [homedir (System/getProperty "user.home")]
    (if (string? user)
      (str (-> homedir File. .getParent) File/separator user)
      homedir)))

(defn- bash-user-homedir-expansion
  "Helper function to return ~[user] exapnsion or nil if there is no such reference."
  [string]
  (let [[_ token rest-path] (re-matches #"(~\w*)(/.*)?" string)] ;_ is fully matched string
    (and token
         (str (bash-user-homedir-token-expansion token) rest-path))))

(defn- bash-user-homedir-pwd-expansion
  "Helper function to process simple ~+ expansion if present, or nil otherwise."
  [string]                              ;e.g. "~+/abc/def
  (let [[match rest-path] (re-matches  #"~\+(/.*)?" string)]
    (and match
         (str (System/getProperty "user.dir") rest-path))))

(defn bash-tilde-expansion
  "Approximate 'bash' compatible expansion of tilde's (~) at the start of pathnames.
  Return the possibly expanded (or otherwise original) path string.
  For username qualified tildes, we assume other users have the same home directory prefix,
  but this isn' actually correct, a correct lookup would examine the user passwd attributes.
  This method also attempts '~+' (PWD expansion), but no other aspects of directory stack substitution."
  [path]
  (let [string (str path)]
    (or (bash-user-homedir-expansion string)
        (bash-user-homedir-pwd-expansion string)
        string)))

;; CONCH NOTES:
;; -- ARGS --
;; 1) An optional last argument as a map presents options interpreted by CONCH (not the external program)
;;    for execution.  Valid map k/v values are:
;;    :seq true - output a sequence of lines
;;    :out {java.io.File, java.io.Writer, function} - specifies a destination for the output
;;       Note that java.io.File will be turned into an unbuffered java.io.FileWriter internally
;;       Two arguments are passed to functions, the line produced by the command, and
;;       a map of proc data that is usually ignored.
;;    :err - accepts same types as :out, default unknown
;;    :verbose true - if you want extended return information in a map
;;    :timeout n - force timeout on the command in 'n' milliseconds
;;    :in {string, seq, java.io.Reader} - line oriented input sources
;;       If you specify a string, it should have newlines to separate the input lines.
;;       If you specify a seq, each element in the seq is treated as a logical line (as if it had a newline).
;;    :background true - run the process in the background (returns a future)
;;    :buffer {n,:none, :line} -
;;       Make a buffer of N *characters* for program output, normally all output is read immediately
;;       which could potentially use a lot of memory.  This restricts it to N characters.
;;       Note that each N characters will be returned as a seaparate string.
;;       If :none is specified, no buffering is done, and the returned sequence (if :seq is true)
;;       is a sequence of single characters instead of strings.
;;       If :line is specified, input is buffered into logical lines (??)
;;       Examples of :buffer
;;       user> (ls {:seq true :buffer 5})
;;       ("#READ" "ME.md" "#\nREA" "DME.m" "d\ncla" "sses\n" "conch" "\ndocs" "\nfoo." "py\nli" "b\nlol" "\npom." "xml\np" "om.xm" "l.asc" "\nproj" "ect.c" "lj\nsr" "c\ntar" "get\nt" "est\nt" "estfi" "le\n")
;;       user> (ls {:seq true :buffer :none})
;;       (\# \R \E \A \D \M \E \. \m \d \# \newline \R \E \A \D \M \E \. \m \d \newline ...)
;;            
;; 2) A (single) lazy sequence where in the input args.  If there are multiple lazy sequences only the first
;;    is used and the rest are discarded.  Under the covers this is put into a map as :in.
;; 3) Anything else (not last map, lazy seq) are arguments to the command.
;; 4) :in and :out values are interpreted by conch :Drinkable and :Redirectable protocols respectively,
;;    which can be extended if you want to add support for other source/sink setups.
;; ?) :redirect-err :env :dir options?
;;
;; -- OUTPUT --
;; With no special arguments, the output of a conch wrapper returns the output as one big string,
;; with newlines and such.
;;
;; e.g. (programs ls) (ls)
;; => "doc\nLICENSE\npom.xml\nproject.clj\nproject.clj~\nREADME.md\nresources\nsrc\ntarget\ntest\n"
;; 
;; To get a lazy seq of lines instead of a monolithic string, pass {:seq true}
;;
;; user> (let [writer (java.io.StringWriter.)] (echo "foo" {:out writer}) (str writer))
;; "foo\n"
;;user> (echo "foo" {:out (java.io.File. "conch")})
;; nil
;; user> (slurp "conch")
;; "foo\n"
;;
;; -- INPUT --
;; If you specify {:in "string"} then newlines in the string separate input elements.
;;  user> (cat {:in "a\nb\nc\n\n"})
;; "a\nb\nc\n\n"
;;
;; If you specify a sequence as input, the newlines are implicit in each element in the sequence
;; user> (cat {:in ["a" "b" "c"]})
;; "a\nb\nc\n"
;;
;; If you specify a lazy sequence without :in, then it must truely be lazy (via 'seq')
;; user> (cat ["a" "b" "c"])
;; IllegalArgumentException array element type mismatch  java.lang.reflect.Array.set (Array.java:-2)
;; user> (cat (seq ["a" "b" "c"]))
;; "a\nb\nc\n"
;;
;; Here's a sample function for output capture:
;; user> (cat {:in "a\nb\n\n" :out (fn [line x] (println "line=" line) (println "x=" x))})
;; line= a
;; x= {:out (a b ), :in #<BufferedOutputStream java.io.BufferedOutputStream@5b1ea65f>, :err (), :process #<UNIXProcess java.lang.UNIXProcess@48f27b6e>}
;; line= b
;; x= {:out (a b ), :in #<BufferedOutputStream java.io.BufferedOutputStream@5b1ea65f>, :err (), :process #<UNIXProcess java.lang.UNIXProcess@48f27b6e>}
;; line= 
;; x= {:out (a b ), :in #<BufferedOutputStream java.io.BufferedOutputStream@5b1ea65f>, :err (), :process #<UNIXProcess java.lang.UNIXProcess@48f27b6e>}
;;
;; -- PIPES (really just call nesting) --
;; One way to specify one program as input to another is, based on what's been said above,
;; user> (programs grep ps)
;; #'user/ps
;; user> (grep "ssh" {:in (ps "-e" {:seq true})})
;; " 4554 ??         0:00.77 /usr/bin/ssh-agent -l\n"
;; 
;; Of course :in above is just a big monolithic string.
;; 
;; You can also do this for pipes:
;; user> (grep "ssh" (ps "-e" {:seq true}))
;; " 4554 ??         0:00.77 /usr/bin/ssh-agent -l\n"
;;
;; In the last example, the command parser looks for lazy sequences in the command arg list so you can
;; eliminate the map with :in (though under the hood it's turned right back to a map arg with :in.



(defn do-substitutions
  "Do bash construct expansion to the extent we support it.
  'args' should be a collection."
  [args]
  (map (fn [arg]
         (if (string? arg)
           (bash-tilde-expansion arg)
           arg))
       args))

;; These are lifted from your PATH.  We could automatically round up the ones from the bin directory
;; for now I've just lifted my personal working set
;; TODO: this 'find' would supersede clojure's find
;;(programs ls find grep fgrep egrep echo bash java javac lein )

;; TODO: want things for bash internal commands, like 'history' ?  Maybe called bash-history

(defn executeWithDefaultOptions         ;must be public for macro use below
  "Make (:seq true) be the default options if no :seq options were specified with command."
  [name & args]
  (let [args (do-substitutions args)
        [[options] only-args] ((juxt filter remove) map? args)]
    (if (:seq options)
      (apply execute name args) ;leave already specified :seq value alone
      (apply execute name (concat only-args (list (assoc options :seq true)))))))

(defmacro defprogram
  "Define a function that wraps an external program on the host operating system shell PATH.
Similar to conch 'programs', but allows us to name the resulting function to avoid conflict.
Also arranges for {:seq true} to be the default options for conch-wrapped commands it defines."
  ;; Note that conch/let-programs will allow access to programs NOT on PATH.
  [fn-name program-name]
  `(defn ~fn-name [& ~'args]
     (apply executeWithDefaultOptions ~(str program-name) ~'args)))

;; Can we use :dir :env with the above commands?

(defprogram ^{:doc "Invoke 'ls' from the calling shell's PATH."} ls ls)
(defn lf [& args]
  (take 40 (rest (apply ls "-lt" args)))) ;rest skips header line
(defprogram ^{:doc "Invoke 'which' from the calling shell's PATH."} which which)
(defprogram ^{:doc "Invoke 'apropos' from the calling shell's PATH."} os-apropos apropos)
(defprogram ^{:doc "Invoke 'cat' from the calling shell's PATH."} cat cat)
(defprogram ^{:doc "Invoke 'tail' from the calling shell's PATH."} tail tail)
(defprogram ^{:doc "Invoke 'head' from the calling shell's PATH."} head head)

(defn- run-less
  "Run the 'less' program on the indicated file.  Wait for it to complete."
  [file]
  (let [less-cmd
        (if (System/console)
          ;; If you have a console, e.g. if you invoked lein repl from a command line
          (into-array ["sh" "-c" (str "less " file " </dev/tty >/dev/tty")])
          ;; If you don't have a console, perhaps running from emacs
          ;; Or consider medium-r-normal
          (into-array ["xterm" "-fn" "*liberation mono-bold-r-normal*" "-e" (str "less " file)]))
        ^Process p (.exec (Runtime/getRuntime) less-cmd)]
    (.waitFor p)))
  
(defn- run-less-with-temp-fn
  "Allocate a temporary file, call fn on the the temporary file to write data to it, which will
  in turn be passed to run-less, after which the temporary file is deleted."
  [fn]
  (with-temporary-file [tempfile]
    (fn tempfile)
    (run-less tempfile)))

(defn less
  "Given an input that is one of
  1) a string of logical lines seperated by newlines
  2) a seq or collection of logical lines
  3) Some other thing compatible with clojure.java.io/copy, e.g.
      InputStream, Reader, File, byte[]
  invoke 'less' on the input. (May use a temporary files)."
  [input]
  (cond (nil? input) nil                ;don't do anything if there's nothing to display
        ;; Print sequences to a temp file, run less on that.
        (or (seq? input) (coll? input)) (run-less-with-temp-fn
                                         (fn [tempfile]
                                           (with-open [writer (writer tempfile)]
                                             (binding [*out* writer]
                                               (printlines input)))))
        ;; If the input is a file, we don't need to do anything
        (instance? java.io.File input) (run-less input)
        ;; Assume it's something we can copy to a temporary file
        true (run-less-with-temp-fn
              (fn [tempfile]
                (copy input tempfile)))))

;; Next, use me.raynes/fs (add to dependencies, etc)
;; rename host.clj sh.clj or something?  core.clj is unnecessary

(defn pwd []
  "Return the working directory of the process. See also (user-homedir)"
  (System/getProperty "user.dir"))

(defprogram ^{:doc "Invoke 'tar' from the calling shell's PATH."} tar tar)
(defn tar-zcf
  "Invoke 'tar' to one or more files or directories and generate an output file that has a timestamp
  spliced into its name.  Also defaults any input arguments such that the are tarred from the perspective
  of the parent directory that contains them (since this command doesn't have any notion of
  working directory except that of the parent process.)

  Arguments are those that would FOLLOW 'tar zcf', such that the first argument is the tar file path
  to which a timestamp will be spliced in, and
  subsequent arguments are files/directories to back up relative to the parent directory of
  the first input path.  E.g.

  (tar-zcf \"/tmp/backup.tgz\" \"a\" \"b\") is transformed into equivalent:
  $ tar zcf /tmp/backup.2014DEC23-144320.tgz -C <parent-dir-of-a> a b

  Input and output file references are tilde expanded (e.g. ~/foo => /home/dave/foo).

  Returns the name of the created tar file."
  [tarfile-name f1 & args]
  (let [filename (timestamp-filename (bash-tilde-expansion tarfile-name))
        f1 (bash-tilde-expansion f1)
        parentDir (str (.getParentFile (as-file f1)))
        args (map bash-tilde-expansion args)]
    (apply tar "zcf" filename "-C" parentDir f1 args)
    filename))
  
