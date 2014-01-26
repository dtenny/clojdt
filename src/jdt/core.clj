(ns jdt.core
  (import java.io.File)
  (import java.util.Date)
  (import java.text.SimpleDateFormat)
  (require clojure.java.io)
  (use clojure.test))                  ;'is'

#_
(defn defined?
  "True if symbol is defined in namespace, untrue otherwise.  See also: bound?"
  ([name] (get (ns-map *ns*) name))
  ([name ns] (get (ns-map ns) name)))

;; Here's a little exercise where we define dispatch based on whether the args are strings or symbols
;; and convert from strings to symbols.
(defmulti defined? 
  "True if symbol is defined in namespace, untrue otherwise"
  (fn [ & args ] (map class args)))
(defmethod defined? [clojure.lang.Symbol] [name] (get (ns-map *ns*) name))
(defmethod defined? [clojure.lang.Symbol clojure.lang.Namespace] [name ns] (get (ns-map ns) name))
(defmethod defined? [java.lang.String] [name] (defined? (symbol name)))
(defmethod defined? [java.lang.String clojure.lang.Namespace] [name ns] (defined? (symbol name) ns))

(defn undef
  "Opposite of def, remove name from namepsace map (defaults to *ns*).
   Return true if the symbol was undefined, false if it was not."
  ;; ns-unmap always returns false, correct for it.
  ([name] (and (defined? name *ns*) (do (ns-unmap *ns* name) (not (defined? name *ns*)))))
  ([name ns] (and (defined? name ns) (do (ns-unmap ns name) (not (defined? name *ns*))))))


(defmacro def-
  "Like 'def', only the resulting symbol is private to the namespace.
   Why clojure doesn't define this I have no idea."
  [var & rest]
  `(def ~(with-meta var (assoc (meta var) :private true)) ~@rest))

(defmacro defdynamic
  "Define a dynamic var that can be bound using the 'binding' form."
  [var & rest] 
  ;; Careful using reader macros like ^ in macros, and meta is particularly tricky.
  ;; See http://stackoverflow.com/questions/989374/help-me-write-a-clojure-macro-which-automatically-adds-metadata-to-a-function-de
  `(def ~(with-meta var (assoc (meta var) :dynamic true)) ~@rest))

(defn logically-true?
  "Tests a value for logical truth (non-nil and non-false),
  returns the value if it's logically true, returns nil otherwise.
  See 'not-nil?' for a subset of this test.
  I think I like the 'false->nil' name for this better."
  ^{:see-also 'false->nil}
  [val]
  (if val
    val
    nil))

(defn false->nil
  "Convert false to nil, return anything else as-is.

  Tests a value for logical truth (non-nil and non-false),
  returns the value if it's logically true, returns nil otherwise.
  See 'not-nil?' for a subset of this test.
  An alternative name for 'logically-true?'."
  ^{:see-also 'logically-true?}
  [val]
  (if val
    val
    nil))

(defn not-nil?
  "Returns true if the specified value is not nil (including if the value is false)."
  [value]
  (not (nil? value)))

(defmacro unless [condition & body]
    `(when (not ~condition)
       ~@body))

(defmacro until
  "Repeatedly executes body until test expression is true. Presumes
  some side-effect will cause test to become false/nil. Returns nil"
  [test & body]
  `(loop []
     (do ~@body)
     (or ~test 
         (recur))))

(defn prompt
  "Prompt user for input and return what was typed.
  Not a sticky/looping prompt, if the user enters an empty line, prompt returns an empty line."
  [prompt-string]
  (println prompt-string)
  (flush)
  (read-line))

(defn y-or-n-p?
  "Prompt user for 'y' or 'n' until it is entered in response to a given prompt string.
  Return true if 'y' is specified, nil otherwise."
  [ prompt-string]
  (logically-true?
   (= "y"
      (until (re-matches #"[yn]" (prompt prompt-string))))))


(defn find-if
  "Returns the first element in coll for which predicate returns true,
   unlike 'some' that returns the true if there is any object in coll for which pred returns true."
  [pred coll]
  (if (empty? coll)
    nil
    (let [item (first coll)]
      (if (pred item)
        item
        (recur pred (rest coll))))))

(defmacro and-let 
  "Like LET, but if any binding is NIL AND-LET immediately returns NIL and BODY is not executed.
  Neither are subsequent bindings.
  Return value is the last value returned by body, or nil if body is not executed.
  This macro has semantic effects something like the following (if Clojure had a return operation):
    (and-let [x 1
             y nil]
     (print x))
  to:
    (let ([x (or 1 (return))
          y (or nil (return)))]
     (print x))"

  [bindings & body]
  (if (seq bindings)
    `(if-let [~(first bindings) ~(second bindings)]
       (and-let ~(drop 2 bindings) ~@body))
    `(do ~@body)))

(comment  ;; no #| |# block comment in clojure, alas
(println "A")(and-let [x nil y 1] (println x) (println y))
(println "B")(and-let [x 1 y nil] (println x) (println y))
(println "C")(and-let [x false y nil] (println x) (println y))
(println "D")(and-let [x true y 1] (println x) (println y))
(println "E")(and-let [x true] x)
)

;; (use 'clojure.walk) -> provides macroexpand-all, useful sometimes where macroexpand isn't enough
;; recursive macros like and-let being one of those cases

(defn probe-file
  "Tries to coerce its argument to a java.io.File object,
   then returns the File object if the file exists on disk, or nil if the file does not exist.
   Existence does not imply file type, for example the file could be a directory."
  [file-coercible-thing]
  (let [file (clojure.java.io/as-file file-coercible-thing)]
    (and (or (.exists file) nil) ; false->nil for return value
         file)))

(defn cl-print
  "A Common Lisp style PRINT function.
  Output a newline, a mostly READable representation of OBJECT, and a space to the specified STREAM. 
  Return OBJECT."
  ([object] (cl-print object *out*))
  ([object stream]
     (binding [*out* stream
               *print-readably* true]
       (newline)
       (pr object)
       (print " ")                      ;not readable, it should bind *print-readably* to nil
       object)))
;(cl-print "testing") ; be sure to look for the space after the end quote

(defn symbol-namespace
  "Return the namespace of a symbol or nil if there is no such namespace
  or the symbol wasn't created with one.

  Notes on symbols:
  1) The symbol constructor (symbol name [namespace]) allows a namespace name,
     but it doesn't have to be a valid namespace.
  2) symbol equality (= or equals) does consider the namespace field of a namespace
  3) (namespace sym) will return the namespace as a string
  4) (symbol-namespace) (this function) returns the namespace as a namespace
  "
  [sym]
  (if-let [ns-name (namespace sym)]
    (find-ns (symbol ns-name))))
;;(println (symbol-namespace 'abc))
;;(println (symbol-namespace 'user/abc))
;;(println (symbol-namespace 'no-such-namespace/abc))

(defn ns-apropos
  "Find namespaces whose name matches the specified string.  Case insensitive."
  [string]
  (let [pattern (re-pattern (str "(?i).*" string ".*"))]
    (filter (fn [ns] (re-matches pattern (name (ns-name ns))))  (all-ns))))
;; (ns-apropos "jdt") will find this package

(defn ns-symbol-relationship
  "Return a map of entries that describe the relationship of a symbol to a given namespace."
  [ns symbol]
  (if (not (symbol? symbol))
    (throw (Exception. (str "ns-symbol-relationship expected symbol, got " symbol))))
  ;; ns-name ns-aliases ns-imports ns-interns ns-map ns-publics ns-refers
  (with-local-vars [result (transient {})]
    (when (= (ns-name ns) symbol)        ;ns-name gives a symbol
      (var-set result (assoc! (var-get result) :names-ns ns))) ;symbol names namespace
    ; ns-aliases gives a map, but the k/v types/semantics are presently unknown
    (doseq [[k v] (seq (ns-aliases ns))]
      (when (= k symbol)
        (var-set result (assoc! (var-get result) :alias-for v)))
      (when (= v symbol)
        (var-set result (assoc! (var-get result) :aliased-by k))))
    ;; ns-imports keyed by symbol, valued by class
    (if-let [v (get (ns-imports ns) symbol)]
      (var-set result (assoc! (var-get result) :imports v)))       ;symbol names imported class
    ;; ns-interns gives a map, keys are symbols in the namespace, values are vars( and special forms?)
    ;; that they map to. 
    (if-let [v (get (ns-interns ns) symbol)]
      (var-set result (assoc! (var-get result) :interns v)))
    ;; ns-maps gives a map, value types/semantics are unknown
    (if-let [v (get (ns-map ns) symbol)]
      (var-set result (assoc! (var-get result) :maps-to v)))
    ;; ns-publics gives a map of public intern mappings, presumably a subset of ns-interns
    (if-let [v (get (ns-publics ns) symbol)]
      (var-set result (assoc! (var-get result) :interns-publicly v)))
    ;; ns-refers gives a map of refer-mappings, value semantics unknown
    (if-let [v (get (ns-refers ns) symbol)]
      (var-set result (assoc! (var-get result) :refers-to v)))
    (persistent! (var-get result))))
;;(println (ns-symbol-relationship *ns* 'ns-symbol-relationship))
;; => {:interns #'jdtutil.core/ns-symbol-relationship, :maps-to #'jdtutil.core/ns-symbol-relationship, :interns-publicly #'jdtutil.core/ns-symbol-relationship}    

;;; Some alternatives to the above implementation
#_
(defn ns-symbol-relationship-2
  [ns symbol]
  {:pre [(symbol? symbol)]}
  (as-> {} result
        (assoc result :name-ns (when (= (ns-name ns) symbol) ns))
        (assoc result :imports (get (ns-imports ns) symbol))
        (assoc result :interns (get (ns-interns ns) symbol))
        (assoc result :maps-to (get (ns-map ns) symbol))
        (assoc result :interns-publicly (get (ns-publics ns) symbol))
        (assoc result :refers-to (get (ns-refers ns) symbol))
        (apply (partial merge-with conj result)
               (map (fn [[k v]]
                      {:aliased-for (when (= k symbol) [v])
                       :aliased-by  (when (= (ns-name v) symbol) [k])})
                    (seq (ns-aliases ns))))))
#_
(defn ns-symbol-relationship-3
  [ns symbol]
  {:pre [(symbol? symbol)]}
  (-> {}
      (assoc :name-ns (when (= (ns-name ns) symbol) ns))
      (assoc :imports (get (ns-imports ns) symbol))
      (assoc :interns (get (ns-interns ns) symbol))
      (assoc :maps-to (get (ns-map ns) symbol))
      (assoc :interns-publicly (get (ns-publics ns) symbol))
      (assoc :refers-to (get (ns-refers ns) symbol))
      (->> 
       (partial merge-with conj))
      (apply 
       (map (fn [[k v]]
              {:aliased-for (when (= k symbol) [v])
               :aliased-by  (when (= (ns-name v) symbol) [k])}) (seq (ns-aliases ns))))))
#_
(defn ns-symbol-relationship-4
  [ns symbol]
  {:pre [(symbol? symbol)]}
  {:name-ns (when (= (ns-name ns) symbol) ns)
   :imports (get (ns-imports ns) symbol)
   :interns (get (ns-interns ns) symbol)
   :maps-to (get (ns-map ns) symbol)
   :interns-publicly (get (ns-publics ns) symbol)
   :refers-to (get (ns-refers ns) symbol)
   :aliased-for (map (fn [[k v]] (when (= k symbol) [v])) (seq (ns-aliases ns)))
   :aliased-by (map (fn [[k v]] (when (= (ns-name v) symbol) [k])) (seq (ns-aliases ns)))})
#_
(do
  (println "r1: " (str (ns-symbol-relationship   *ns* 'ns-symbol-relationship)))
  (println "r2: " (str (ns-symbol-relationship-2 *ns* 'ns-symbol-relationship)))
  (println "r3: " (str (ns-symbol-relationship-3 *ns* 'ns-symbol-relationship)))
  (println "r4: " (str (ns-symbol-relationship-4 *ns* 'ns-symbol-relationship))))
;; Comments:
;; pastebin link: good
;; :pre/post: good
;; as-> good
;; direct map construction: good
;; pr-str, redundant with str?  Or not because of the *print-xxx* options?
;; key/nil pairs not desired.
;; #4 if easiest to read, but adds the most nil values

(defn map-get
  "Perform GET of a key across multiple maps, returning a lazy sequence of the results of the get.
  Unlike GET, this function requires a KEY-NOT-FOUND value for all lookups.

  This function is useful when you're checking for a key in multiple maps, for example, the many
  maps associated with namespaces.

  user> (map-get 'map-get :key-not-found (ns-publics *ns*) (ns-map *ns*) (ns-refers *ns*))
  (#'jdtutil.core/map-get #'jdtutil.core/map-get :key-not-found)
  "

  [key key-not-found & maps]
  (map #(get %1 key key-not-found) maps))
;;(println (map-get 'map-get :key-not-found (ns-publics *ns*) (ns-map *ns*) (ns-refers *ns*)))


(defn printlines
  "Given a sequence, attempt to print one line for each element of the sequence, as if by
   (doseq [x seq] (println x)).  Any non-sequence arguments are printed as if by (println x)."
  [& args]
  (doseq [x args]
    (if (coll? x)
      (doseq [x x] (println x))
      (println x))))
;;(printlines '(a b c))
;;(printlines 1 '(2 3 4) [5 6 7])

;;; This addition to Clojure's assoc is not compatible with Common Lisp's assoc-if (neither is
;;; Clojure's assoc compatible with CL's assoc).
(defn assoc-if
  "Like assoc only the k/v pair is added only if the supplied predicate returns
  logical true for the k/v pair to be added. Otherwise the unaltered map is returned.

  If pred is nil, uses #(logically-true? val) as a predicate which will filter
  false as well as nil valued pairs.
  You might specify (fn [k v] (not (nil? v))) if you want to filter only nil valued k/v pairs."
  ([map pred key val]
     (let [pred (or pred (fn [k v] (logically-true? v)))]
       (if (pred key val)
         (assoc map key val)
         map)))
  ([map pred key val & kvs]
     (if (empty? kvs)
       (assoc-if map pred key val)
       (let [[nextk nextv & nextkvs] kvs]
         (recur (assoc-if map pred key val) pred nextk nextv nextkvs)))))
#_
(do
  (def l '[:a a :b b :c nil :d false])
  (is (= (assoc-if {} nil :a nil) {}))
  (is (= (assoc-if {} nil :a false) {}))
  (is (= (assoc-if {} nil :a 'a) {:a 'a}))
  (is (= (apply assoc-if {:g "g"} (fn [k v] (not (nil? v))) l)) {:d false, :b 'b, :a 'a, :g "g"}))

(defmacro with-temporary-file
  "Execute body with a binding to a temporary File that will be created on entry and deleted on exit
  even with a non-local exit from body.  Returns the value of body unless an exception is thrown,
  which is uncaught by this macro.  The File binding will represent a file that exists but is empty.

  Example: 
  (with-temporary-file [file]
    (with-open [stream (something-or-other file)]
       (do-stuff-with-stream stream)))"
  [bindings & body]
  (assert (vector? bindings) "a vector of length 1 for bindings")
  (assert (= (count bindings) 1) "one form in the binding vector")
  (assert (symbol? (bindings 0)) "binding must be a Symbol")
  (let [sym (bindings 0)]
    `(let [file# (File/createTempFile "jdt.core." ".tmp")]
       (try
         (let [~sym file#]
           ~@body)
         (finally (.delete file#))))))


(defn filename-friendly-timestamp
  "Return a string that makes for an easily typed no-escapes-needed substring of a file name.
  The following optional arguments are permitted in any order:
  - A single java.util.Date argument which will be used to represent the date/time.
  - A keyword indicating the desired format, :date (yyyyMMMdd), :datetime (yyyyMMMdd-HHmmss, the default),
    :time (HHmmss)."
  [& args]
  (let [date (or (find-if (fn [x] (instance? java.util.Date x)) args) (Date.))
        format (or (find-if keyword? args) :datetime)
        format-string (or (format {:date "yyyyMMMdd"
                                   :datetime "yyyyMMMdd-HHmmss"
                                   :time "HHmmss"})
                          (throw (Exception. (str "Invalid format keyword: " format))))
        ;; N.B. SimpleDateFormat objects are not thread safe.
        formatter (SimpleDateFormat. format-string)]
    (.format formatter date)))

(defn timestamp-filename
  "Splice a filename-friendly-timestamp into a file name before the file suffix.
  If the file name has no suffix, append the timestamp.  Return the updated file name.
  E.g. (timestamp-filename \"/tmp/backup.tgz\") => /tmp/backup.2014DEC23-144320.tgz
       (timestamp-filename \"abc\") => abc.2014DEC23-144320"
  [filename]
  (let [dot-pos (.lastIndexOf filename ".")
        have-dot (>= dot-pos 0)
        prefix (if have-dot (.substring filename 0 dot-pos) filename)
        suffix (if have-dot (.substring filename dot-pos) "")]
    (str prefix "." (filename-friendly-timestamp) suffix)))

