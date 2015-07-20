(ns jdt.core
  (:import java.io.File)
  (:import java.io.BufferedReader)
  (:import java.util.Date)
  (:import java.text.SimpleDateFormat)
  (:require [clojure.walk :as walk])
  (:require [clojure.java.io :as io])
  (:require [clojure.pprint :refer [cl-format]]))

#_
(defn defined?
  "True if symbol is defined in namespace, untrue otherwise.  See also: bound?"
  ([name] (get (ns-map *ns*) name))
  ([name ns] (get (ns-map ns) name)))

;; Here's a little exercise where we define dispatch based on whether the args are strings or symbols
;; and convert from strings to symbols.
(defmulti defined? 
  "True if symbol is defined in namespace, untrue otherwise"
  (fn [ & args ] (mapv class args)))
(defmethod defined? [clojure.lang.Symbol] [name] (get (ns-map *ns*) name))
(defmethod defined? [clojure.lang.Symbol clojure.lang.Namespace] [name ns] (get (ns-map ns) name))
(defmethod defined? [java.lang.String] [name] (defined? (symbol name)))
(defmethod defined? [java.lang.String clojure.lang.Namespace] [name ns] (defined? (symbol name) ns))

(defn undef
  "Opposite of def, remove name (a string or symbol) from namepsace map (defaults to *ns*).
   Return true if the symbol was undefined, false if it was not."
  ;; ns-unmap always returns false, correct for it.
  ([name] (and (defined? name *ns*) (do (ns-unmap *ns* name) (not (defined? name *ns*)))))
  ([name ns] (and (defined? name ns) (do (ns-unmap ns name) (not (defined? name *ns*))))))

(defn ns-vars
  "Return sequence of vars interned in ns.
  Note that these may represent functions as well as other values, since every value in the map
  returned by ns-interns is a clojure.lang.Var."
  ([] (ns-vars *ns*))
  ([ns] (->> (ns-interns ns)
             vals)))

(defn ns-unfun-vars
  "Return sequence of vars interned in ns that do not have functions as values.
  Useful if you want to see what might resemble 'traditional' variables in the namespace."
  ([] (ns-unfun-vars *ns*))
  ([ns] (->> (ns-interns ns)
             (filter (fn [e] (not (fn? (var-get (val e))))))
             vals)))

(defmacro def-
  "Like 'def', only the resulting symbol is private to the namespace."
  [var & rest]
  `(def ~(with-meta var (assoc (meta var) :private true)) ~@rest))

(defmacro defonce-
  "Like 'defonce', only the resulting symbol is private to the namespace."
  [var & rest]
  `(defonce ~(with-meta var (assoc (meta var) :private true)) ~@rest))

(defmacro defdynamic
  "Define a dynamic var that can be bound using the 'binding' form."
  [var & rest] 
  ;; Careful using reader macros like ^ in macros, and meta is particularly tricky.
  ;; See http://stackoverflow.com/questions/989374/help-me-write-a-clojure-macro-which-automatically-adds-metadata-to-a-function-de
  `(def ~(with-meta var (assoc (meta var) :dynamic true)) ~@rest))

(defn logically-false?
  "Return true if the supplied argument is logically false, otherwise nil."
  [x]
  (or (false? x)
      (nil? x)))

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

(defn empty->nil
  "Convert empty sequences to nil, since () is != nil in clojure.
  Return non-empty sequences as is."
  [s]
  (if (empty? s)
    nil
    s))

(defn not-nil?
  "Returns true if the specified value is not nil (including if the value is false)."
  [value]
  (not (nil? value)))

(defn not-empty?
  "Returns coll if the specified collection or sequence is not empty, otherwise nil.
   (seq coll) is supposed to be idiomatic clojure for this, but I'm uncomfortable with the
   fact that 'seq' appears to allocate objects instead of just returning a (non-consed) value.
   So this predicate is not equivalent to (seq coll), because it isn't necessarily going to return a seq."
  [coll]
  (if (empty? coll)
    nil
    coll))

(defn false-arg-wrap
  "Wraps a function f so that if any arguments passed to f are logically false, the returned function
  returns nil.  Otherwise the returned function applies f to its args.  See also 'fnil'.
  E.g.
    (def *nil (false-arg-wrap *))
    (*nil 1 2 3) => 6
    (*nil 1 nil 3) => nil"
  [f]
  (fn [& args]
    (if (some logically-false? args)
     nil
     (apply f args))))

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

(defn exception-retry
  "Call function f with exception handler fe.  If an
   exception is thrown by f, call fe on the exception, and then call f again
   if the result of calling fe is logically true or unless fe throws.

   If f completes execution without throwing we return the return value of f.
   If f throws and fe returns logical false, return nil.

   f must not return a function, this is prohibited and will result in an exception.

   This function is a non-recursive workaround for situations for where
   you want to recurse in a catch/finally statement (i.e. retry a try block in the face of
   exceptions thrown from the try block).  You can't use 'recur' in catch/finally.
   This works around it.

   E.g. (loop [] (try (f) (catch Exception e (recur)))) ; WILL NOT WORK
   but  (exception-retry f (fn [] true)) ; WILL WORK

   In practice you want fe to examine the exception raised
   and probably sleep before returning to try f again.  Maybe print a message
   that this a retry is happening."
  [f fe]
  (let [tryfn
        (fn [] (try (let [result (f)]
                      (assert (not (fn? result)))
                      result)
                    (catch Exception e (if (fe e) f nil))))]
    (trampoline tryfn)))

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

;; Let's talk about finding things.
;; clojure.core/find only finds map entries for keys in maps.
;; clojure.core/some could do it, though I'm often too lazy to want to wrap the object to be found
;; in the right predicate.
;; clojure.core/filter can do it, but might return more items than I want, I only want one.

(defn cl-find
  "A version of the common lisp 'find' function.
   Return the item in sequence that for which test returns true.

   Options:
   :test - a function of two arguments (default '=') whose logical boolean result is used to find an item.
           The test is called with the 'item' argument as the first test fn argument.
   :start - first element index (inclusive, default 0) in the sequence to begin examination of elements.
   :end - last element index (exclusive, default nil = length of sequence) to examine in sequence.
   :from-end - logical true if you want to search from the end of the sequence, false otherwise.
   :key - a function of one argument (default identity) applied to the collection elements before applying
          'test'.  Remember that item is returned, not the key applied to the sequence member.

   Forces partial or full valuation of lazy sequences."
  [item sequence & {:keys [start end from-end key test]}]
  (let [sequence (if start (drop start sequence) sequence)
        sequence (if end (take end sequence) sequence)
        sequence (if from-end (reverse sequence) sequence)
        keyfn (or key identity)
        testfn (or test =)]
    (if key
      (some (fn [x] (let [y (keyfn x)] (if (testfn item y) x))) sequence)
      (some (fn [x] (if (testfn item x) x)) sequence))))

(defn index-of 
  "Return the index of the first element for which pred return true in sequence.
  Return nil if there is no element in the index for which pred returns true.
  This is basically Common Lisp's POSITION-IF in simplified form."
  [pred s]
  (loop [n 0 s s]
    (if (empty? s)
      nil
      (if (pred (first s))
        n
        (recur (+ n 1) (rest s))))))

(def cl-eq
  "A version of the comon lisp eq predicate."
  identical?)

(defn cl-eql
  "A version of the common lisp eql predicate.
    The value of eql is true of two objects, x and y, in the folowing cases:
    1. If x and y are cl-eq.
    2. If x and y are both numbers of the same type and the same value.
    3. If they are both characters that represent the same character."
  ;; Clojure's identical? does not work like lisps for things like floats, symbols, etc.
  ;; We may need additional adjustments to this function.
  [a b]
  (cond (number? a) (== a b) ; (identical? 1.0 1.0) -> false, (identical? 1 1) => true
                             ; (identical? 1.0 1) -> false
        (symbol? a) (= a b) ; (identical? 'a 'a) -> false, (identical? :b :b) -> true
        :else (cl-eq  a b)))

(defn cl-member
  "A version of the common lisp 'member' function, for which I could not find a roughly equivalent
   clojure function.

   Search list for item or for a top-level element that satisfies the test.
   The argument to the predicate function is an element of list.
   If some element satisfies the test, the tail of list beginning with this element is returned;
   otherwise nil is returned. List is searched on the top level only.

   test - a designator for a function of two arguments that returns a generalized boolean.
   key - a designator for a function of one argument, or nil.

   Example: (cl-member 2 '(1 2 3)) =>  (2 3)"
  [item list & {:keys [key test]
                :or {key identity test cl-eql}}]
  (loop [list list]
    (if (empty? list)
      nil
      (if (test item (key (first list)))
        list
        (recur (rest list))))))

(defn each
  "Return a lazy sequence containing every element in seq for which predicate returns logical true."
  [pred seq]
  (filter identity (map (fn [item] (if (pred item) item)) seq)))

(defn apropos-match-fn
  "Given a string or regular expression, return a function that will invoke .contains or re-find 
  respectively on the pattern string or regular expression against an input string.
  (So the returned function is a function of one argument, the string to be tested against the pattern)."
  [str-or-pattern]
  (cond (instance? java.util.regex.Pattern str-or-pattern)
        (fn [string] (re-find str-or-pattern string))
        (string? str-or-pattern)
        (fn [string] (.contains string str-or-pattern))
        true (throw (Exception. (cl-format nil  "Expected String or Pattern, got '~a'~%" str-or-pattern)))))

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

(defn seqable? "True if x can be turned into a seq via 'seq'"
  [x]
  (or (seq? x)
      (coll? x)
      (list? x)
      (string? x)))

(defn seqable-except-strings?
  "True if x can be turned into a seq via 'seq', except for strings which are seqable but not always
   desireable as seqs of chars."
  [x]
  (or (seq? x)
      (coll? x)
      (list? x)))

(defn seqify
  "If x is a singleton datum, return some kind of sequence of one element, x.
   If x is a collection return a sequence for it.
   If x is a sequence, return it as is.
   Nil is given special treatment and is turned into an empty sequence,
   however false is nto converted into an empty sequence.

   ** NOTE **: we do NOT convert strings into sequences, though they are seqable.  But that defeats
   most use cases for this function."
  [x]
  (cond (seq? x) x
        ;; The string case is deliberately omitted here.
        ;; (string? x) (seq x)             ;note that (seq "") => nil
        (coll? x) (seq x)
        (nil? x) ()
        :else (list x)))

(defn listify
  "Similar to seqify, but ensures that the returned collection type is a List.
   If x is a singleton datum, return a list of one element, x.
   If x is a non-list collection return a list for it.
   If x is a sequence, return it as a list.
   Nil is given special treatment and is turned into an empty list
   however false is not converted into an empty sequence.
   Element order is retained so that sorted inputs preserve their order."
  [x]
  (cond (list? x) x
        (seq? x) (apply list x)
        (coll? x) (apply list x)
        (nil? x) ()
        :else (list x)))

;; (use 'clojure.walk) -> provides macroexpand-all, useful sometimes where macroexpand isn't enough
;; recursive macros like and-let being one of those cases

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

(defn get-valid
  "Return the value of (get map key). If key is not in map,
   throw an IllegalArgumentException.  Note that 'get' applies to sets as well as maps."
  [map key]
  (let [result (get map key get-valid)]
    (if (= result get-valid)
      (throw (IllegalArgumentException. (str "No such key " key " in map.")))
      result)))


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

(defn pr-to
  "Bind *out* to writer, invoke 'pr' on objects.  Return nil."
  [writer & objects]
  (binding [*out* writer] (apply pr objects)))

(defn prn-to
  "Bind *out* to writer, invoke 'prn' on objects.  Return nil."
  [writer & objects]
  (binding [*out* writer] (apply prn objects)))

(defn print-to
  "Bind *out* to writer, invoke 'print' on objects.  Return nil."
  [writer & objects]
  (binding [*out* writer] (apply print objects)))

(defn println-to
  "Bind *out* to writer, invoke 'println' on objects.  Return nil."
  [writer & objects]
  (binding [*out* writer] (apply println objects)))

(defn printlines-to
  "Bind *out* to writer, invoke 'printlines' on args.  Return nil."
  [writer & args]
  (binding [*out* writer] (apply printlines args)))

(defn flush-to
  "Flush *out* to writer.  Return nil."
  [writer]
  (binding [*out* writer] (flush)))

(defn append-to-file
  "Append string to File coercible file-spec."
  [file-spec string]
  (with-open [w (io/writer file-spec :append true)]
    (println-to w string)))

(defn has-repl?
  "Return true if we appear to be running with a REPL, false otherwise."
  []
  ;; *TBD*: Consider looking for some particular repl routine in jvm stack traces
  ;; Don't care about the clojure.* prop vals, just that the property exists.
  (if (or (System/getProperty "clojure.debug") 
          (System/getProperty "clojure.compile.path")
          (if-let [command (System/getProperty "sun.java.command")] ;sun jvm specific
            (.contains command "clojure.main")))
    true
    false))

(defn readlines
  "Given some I/O source compatible with clojure.java.io/reader (which will be closed),
  return a vector of lines readable from the source.
  If 'count' is nil, read all lines, otherwise read at most 'count' lines."
  ([source]
     ;; Note, to return a lazy sequence, we'd need to return some lazy opener/closer
     ;; and ensure it was invoked by storing it some java weak reference so that we close it when gc'd.
     (with-open [^BufferedReader rdr (io/reader source)] ;returns a BufferedReader
       (loop [lines []]
         (if-let [line (.readLine rdr)]
           (recur (conj lines line))
           lines))))
  ([source count]
     {:pre [(or (nil? count) (integer? count))]}
     (if (nil? count)
       (readlines source)
       (with-open [^BufferedReader rdr (io/reader source)]
         (loop [lines [] count count]
           (if (> count 0)
             (if-let [line (.readLine rdr)]
               (recur (conj lines line) (- count 1))
               lines)
             lines))))))

(defn empty-string-alternative
  "If string is not a string (e.g. nil) or is an empty string, return alternative, otherwise return string."
  [string alternative]
  (or (and (string? string) (> (count string) 0) string)
      alternative))

(defn string-contains? 
  "Return the zero-based index of substring in string if present, nil if not present.
   This method exists because you can't pass '.contains' as a function for java interop,
   and 'contains?' does not work for this use case.

   See 'contained-in-string?' for function with reversed argument order."
  [^String string ^String substring]
  (let [index (.indexOf string substring)]
    (if (>= index 0)
      index
      nil)))                            ;transform -1 to nil

(defn contained-in-string?
  "Reverse argument order for 'string-contains?'
   Accepts the substring to be matched in the same argument position as the regular expression in
   're-find' or 're-matches'"
  [substring string]
  (string-contains? string substring))
  
(defn map-matches
  "Match patterns in a sequence of map key/val pairs against a line as described in 'select-matching-strings',
   if there are matches return a sequence of keys in the map whose values matched the line.

   We require a sequence of map key/val pairs instead of the map to avoid
   regenerating the (sequence of) pairs for every call to this function for some map.

   E.g. (map-matches \"abc\" (seq {:b-key \"b\" :a-key \"a\" :d-key \"d\"}) contained-in-string?)
   =>   [:a-key :b-key]

   There will be no duplicate keys in the result."
  [string pattern-map-seq match-predicate]
  (map first                            ;extract keys from filter returning kv-pairs
   (filter (fn [kvpair]
             (match-predicate (second kvpair) string))
           pattern-map-seq)))

(defn select-matching-strings
  "Given a input sequence of strings, a map of key/pattern pairs, and a match-predicate to apply
   to the pattern values in the map, return a map keyed by the keys in the input map,
   and valued by a vector of lines that matched the corresponding keyword's value/pattern.

   E.g.
   (select-matching-strings (readlines \".bashrc\") {:comments #\"^\\s*#.*\"} re-matches)
   => {:comments [\"#!/bin/bash\" \"# some comment in .bashrc\" ...]}

   You can specify multiple key/pattern pairs, if there are multiples they are applied to each line
   (so an input line can potentially match multiple patterns).

   In all cases, the predicate is invoked as (match-predicate pattern line-string)

   Specify 're-find' if you want to match partial input strings with regualar expressions.
   E.g. (select-matching-strings lines {:comments \"^#\"} re-find)
   will match any line that starts with a hash in the first column.

   Specify 'contained-in-string?' (or similar predicate)
   if you want to match partial input strings against non-regular-expression text.
   E.g. (select-matching-strings lines {:comments \"#\"} contained-in-string?)
   will match any line containing a # in any position.

   This is not meant to be particularly efficient, it is a convenience for picking up a few matching
   lines from small files and such, such as parsing credentials out of files that have similar but different
   formats. (AWS comes to mind)."
  
  ;;*TODO*: Figure out a good way to include the matched text, not just the input line, like re-groups.
  ;;*TODO*: Figure out a way to allow the result collection specification by specifying a prototype input,
  ;; e.g. #(), [], {}, or ().

  [lines pattern-map match-predicate]
  (loop [lines (seq lines), result {}]
    (let [line (first lines)]
      (if line
        (if-let [matching-keys (empty->nil (map-matches line pattern-map match-predicate))]
          (recur (next lines) (merge-with concat result
                                          ;; Add line to each key's pattern that was matched
                                          (apply hash-map
                                                 (interleave matching-keys
                                                             (repeat (count matching-keys) [line])))))
          (recur (next lines) result))
        result))))

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

;; *TODO*: a version of this, or an option to this, that does the function of unix 'tee'.
;; *TODO*: append option for the resulting file
(defmacro with-output
  "Execute body while binding *out* and *err* to one (or two) writer-coercible destination(s).
   Return the value of body.  See also 'clojure.core/with-out-str'"
  [destinations & body]
  {:pre [(vector? destinations)
         (and (> (count destinations) 0) (< (count destinations) 3))]}
  (let [d1 (first destinations)
        d2 (second destinations)]
    `(let [d2# ~d2]
       (with-open [writer1# (~io/writer ~d1)
                   writer2# (or (and d2# (~io/writer d2#)) writer1#)]
         (binding [*out* writer1# *err* writer2#]
           (let [result# (do ~@body)]
             (. *out* (flush))
             (. *err* (flush))
             result#))))))

(defmacro with-temporary-file
  "Execute body with a binding to a temporary File that will be created on entry and deleted on exit
  even with a non-local exit from body.  Returns the value of body unless an exception is thrown,
  which is uncaught by this macro.  The File binding will represent a file that exists but is empty.

  Example: 
  (with-temporary-file [file]
    (with-open [stream (something-or-other file)]
       (do-stuff-with-stream stream)))

  See also: jdt.easyfs/with-temp-file, which deals with Path objects and java.nio.file interfaces,
  See also: jdt.easyfs/with-temp-directory."

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

(defn date->utc
  "Get a Z suffixed UTC date/time string in sortable, filename-friendly form of
   yyyyMMdd-HHmmssZ."
  ([] (date->utc (java.util.Date.)))
  ([^java.util.Date date]
     (let [formatter (java.text.SimpleDateFormat. "yyyyMMdd-HHmmssX")]
       (.setTimeZone formatter (java.util.TimeZone/getTimeZone "GMT"))
       (.format formatter date))))

(defn date->local
  "Get a local timezone suffixed date/time string in a NOT QUITE fully sortable,
  filename-friendly form of yyyyMMdd-HHmmss[+-]NNNN.  I you want a fully sortable
  timestamp try 'date->utc' or strip off the timezone suffix.
  This method is suitable for build logs and such, just more difficult to sort with,
  say, bash shell utilities than the output of date->utc."
  ([] (date->local (java.util.Date.)))
  ([^java.util.Date date]
     (let [formatter (java.text.SimpleDateFormat. "yyyyMMdd-HHmmssZ")]
       (.format formatter date))))

(defn filename-friendly-timestamp
  "Return a string that makes for an easily typed no-escapes-needed substring of a file name.
  The following optional arguments are permitted in any order:
  - A single java.util.Date argument which will be used to represent the date/time.
  - A keyword indicating the desired format, :date (yyyyMMdd), :datetime (yyyyMMdd-HHmmss, the default),
    :time (HHmmss)."
  [& args]
  (let [date (or (find-if (fn [x] (instance? java.util.Date x)) args) (Date.))
        format (or (find-if keyword? args) :datetime)
        format-string (or (format {:date "yyyyMMdd"
                                   :datetime "yyyyMMdd-HHmmss"
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
  [^String filename]
  (let [dot-pos (.lastIndexOf filename ".")
        have-dot (>= dot-pos 0)
        prefix (if have-dot (.substring filename 0 dot-pos) filename)
        suffix (if have-dot (.substring filename dot-pos) "")]
    (str prefix "." (filename-friendly-timestamp) suffix)))

(defn merge-predicates
  "This function takes a set of arguments which should be either
   1) a predicate, that is, a function of one argument for which 'fn?' is true.
   2) anything else
   and combines all predicates in (1) into a single predicate which returns true only if
   all predicates in (1) return true.
   If there are no predicates input, return nil.

   Examhimple: to obtain a predicate that returns true only if its argument is both positive and even:
   (let [pred (merge-predicates even? pos?)]
     (pred 2)) => true"
  [& args]
  (let [preds (filter fn? args)]
    (if (seq preds)
      (let [composite (apply every-pred preds)]
        (fn [path] (composite path))))))

(defn get-indexes
  "Return a sequence of distinct index paths (as subsequences) that can be used with 'get-in'
  on 'form'.  The initial call is typically (get-indexes form []).
  This is very useful if you want to figure out some useful search paths in nested structures."
  [form result]
  (cond
   (map? form)
   (mapcat (fn [e] (get-indexes (val e) (concat result [(key e)]))) form)
   (seqable-except-strings? form)
   (reduce concat (map-indexed (fn [i val] (get-indexes val (concat result [i]))) form))
   :else [result]))

(defn remove-key-value-pair 
  "Return a lazy seq or the input sequence, such that if 'k' is in the input seq 's',
  it is treated as a keyword/value pair, and the keyword and value are removed.
  Only the first such k/v pair is removed.
  This function does not detect or complain about missing values for the keyword.
  This function does not care if the list has an odd or even number of values,
  so if the input has an odd number of values a value could be mistaken for a keyword."
  [k s]
  (let [seqs (partition-by (fn [i] (= i k)) s)
        n-seqs (count seqs)]
    (println seqs)
    (if (= n-seqs 1)
      s                                 ;k wasn't in s
      (if (= n-seqs 2)
        (first seqs)                    ;k wasn't followed by any additional tokens
        (concat (first seqs) (rest (nth seqs 2)))))))

(defn get-key-value 
  "Return the value associated value for keyword 'k' in sequence 's', or 'missing-value' if it
   is not found.  If 'k' is the last element in 's' nil is returned, not missing-value."
  [k s & [missing-value]]
  (let [tail (drop-while (fn [i] (not (= i k))) s)]
    (if tail
      (fnext tail)
      missing-value)))

