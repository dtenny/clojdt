(ns jdt.core
  (:import java.io.File)
  (:import java.io.BufferedReader)
  (:import java.util.Date)
  (:import java.text.SimpleDateFormat)
  (:require [clojure.java.io :as io])
  (:require [clojure.pprint :refer [cl-format]]))

(defmulti defined? 
  "True if symbol is defined in namespace, untrue otherwise"
  (fn [ & args ] (mapv class args)))
(defmethod defined? [clojure.lang.Symbol] [name] (get (ns-map *ns*) name))
(defmethod defined? [clojure.lang.Symbol clojure.lang.Namespace] [name ns] (get (ns-map ns) name))
(defmethod defined? [java.lang.String] [name] (defined? (symbol name)))
(defmethod defined? [java.lang.String clojure.lang.Namespace] [name ns] (defined? (symbol name) ns))

(defmulti undef
  "Opposite of def, remove name (a string or symbol) from namepsace map (defaults to *ns*).
   Return true if the symbol was undefined, false if it was not."
  ;; Note, ns-unmap does not accept strings, requires symbol.
  (fn [ & args ] (mapv class args)))
(defmethod undef [clojure.lang.Symbol] [name] 
  (and (defined? name *ns*) (do (ns-unmap *ns* name) (not (defined? name *ns*)))))
(defmethod undef [clojure.lang.Symbol clojure.lang.Namespace] [name ns]
  (and (defined? name ns) (do (ns-unmap ns name) (not (defined? name ns)))))
(defmethod undef [java.lang.String] [name] (undef (symbol name)))
(defmethod undef [java.lang.String clojure.lang.Namespace] [name ns] (undef (symbol name) ns))


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

(defn fn-name-info
  "Given a function, try to deduce the function name and clojure namespace by looking 
  at the function's java class name and decoding stuff.  Return a vector of name, namespace strings, 
  or nil if we can't find anything.
  
  For example, if in jdt.core we do (defn foo []), the resulting value foo is a thing whose class is
  jdt.core$foo, for which we can return [\"jdt.core\" \"foo\"].
  
  However, for anonymous functions like (fn []) typed into the REPL, 
  you're going to get something like [\"jdt.core\" \"eval3676$fn__3677\"]

  Fortunately we can call clojure.lang.Compiler/demunge to do this
  (clojure.lang.Compiler/demunge (.getName (class logically-false?))) => \"jdt.core/logically-false?\""
  [f]
  (clojure.string/split (clojure.lang.Compiler/demunge (.getName (class f))) #"/" 
                        2))             ;number of split tokens, not number of times we split on '/'

(defmulti arglist 
  "Given something we can coerce to a Var, return arglist metadata for the Var if it exists, nil otherwise.
  Arglist data is list of vectors of symbols, one vector for each arity function/macro/method
  associated with the Var."
  (fn [ & args ] (mapv class args)))
(defmethod arglist [clojure.lang.Var] [var] (:arglists (meta var)))
(defmethod arglist [clojure.lang.Symbol] [sym] (arglist (ns-resolve *ns* sym)))
(defmethod arglist [clojure.lang.Symbol clojure.lang.Namespace] [sym ns] (arglist (ns-resolve ns sym)))
(defmethod arglist [java.lang.String] [str] (arglist (symbol str)))
(defmethod arglist [java.lang.String clojure.lang.Namespace] [str ns] (arglist (symbol str) ns))
(defmethod arglist [clojure.lang.MultiFn] [f] 
  ;; there is no meta :arglists information on multis!
  ;; If we call (.getMethodTable on multifn) we can see the IfN's for the multimethod, and maybe could
  ;; work out at least arity, but it isn't worth while.
  (comment
    ;; Defmulti objects will be of type MultiFn. They do have a non-public name field that might help
    ;; i.e. the one for this method is 'arglist'.  However we have to go through hoops to get it.
    ;; Meanwhile, let's try. 
    (let [field (.getDeclaredField (class f) "name")]
      (.setAccessible field true)         ;else we'll get access violation on 'final' field.
      (if-let [name (.get field f)]
        (arglist name)))))
(defmethod arglist [clojure.lang.IFn] [f] 
  (let [[ns-name fn-name] (fn-name-info f)
        ns (find-ns (symbol ns-name))]
    (if ns
      (arglist fn-name ns))))

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

(defn third
  "(nth s 2) - because I like it"
  [s]
  (nth s 2))

(defn fourth
  "(nth s 3) - because I like it"
  [s]
  (nth s 3))

(defn fifth
  "(nth s 4) - because I like it"
  [s]
  (nth s 4))

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

(defn start-time "Get a start time for 'elapsed-msecs'" 
  [] 
  (. System (nanoTime)))

(defn elapsed-msecs "Return elapsed time in msecs given a value from 'start-time'."
  [start-time]
  (/ (double (- (. System (nanoTime)) start-time)) 1000000.0))
  
(defmacro time-msecs
  "Evaluates expr and returns the time it took in milliseconds.  Will not trap exceptions (for now)."
  [expr]
  `(let [start# (start-time)]
     ~expr
     (elapsed-msecs start#)))

(defmacro value-time-msecs
  "Evaluates expr and returns a vector with the expression value 
   and the time it took to execute in milliseconds.  Will not trap exceptions (for now)."
  [expr]
  `(let [start# (start-time)
         value# ~expr]
     [value# (elapsed-msecs start#)]))

(defn exception-retry
  "Call function f with exception handler fe.  If an
   exception is thrown by f, call fe on the exception, and then call f again
   if the result of calling fe is logically true or unless fe throws.

   If f completes execution without throwing we return the return value of f.
   If f throws and fe returns logical false, return nil.

   'f' is a function of no args and must not return a function. 
   This is prohibited and will result in an exception.

   'fe' must take one argument, the exception that was thrown by f.
   It should return logical true if 'f' should be retried, logical false if not.

   This function is a non-recursive workaround for situations for where
   you want to recurse in a catch/finally statement (i.e. retry a try block in the face of
   exceptions thrown from the try block).  You can't use 'recur' in catch/finally.
   This works around it.

   E.g. (loop [] (try (f) (catch Exception e (recur)))) ; WILL NOT WORK
   but  (exception-retry f (fn [e] true)) ; WILL WORK

   In practice you want fe to examine the exception raised
   and probably sleep before returning to try f again.  Maybe print a message
   that this a retry is happening."
  [f fe]
  (letfn [(tryfn [] 
            (try (let [result (f)]
                   (assert (not (fn? result)))
                   result)
                 ;; #(tryfn) for subtle behavior of trampoline, otherwise we'd just return 'f'
                 (catch Exception e (if (fe e) #(tryfn) nil))))]
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
;; clojure.core/contains? might do it, but looking for values in vectors or lists won't work as expected.

(defn contains-value?
  "Like clojure.core/contains?, only we do value tests for numerically indexed collections instead of
  the stupid numeric-key-is-within-array-bounds check.  You could use 'some', though in some loops
  you'd be consing up function objects fairly needlessly (with a 'some' predicate for every
  value you're looking for."
  [coll key]
  (cond (or (set? coll) (map? coll))
        (contains? coll key)
        :else
        (loop [vals (seq coll)]
          (if (empty? vals)
            false
            (if (= (first vals) key)
              true
              (recur (rest vals)))))))

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

(defn cl-count-if 
  "A version of the Common Lisp 'count-if' function.  Differs in that the 'test-not' and 'from-end'
  arguments are not supported.

  Deemed less likely to set my hair on fire than routinely using
  (filter identity (map (fn [x] ...) s)).

  Count and return the number of elements in the 'sequence' bounded by 'start' and 'end'
  that satisfy the 'predicate'.

  sequence   - a proper sequence.
  predicate  - a designator for a function of one argument that returns a logical boolean.
  start, end - bounding index designators of sequence. The defaults for start and end are 0 and nil 
               (implying length), respectively.
  key        - a designator for a function of one argument, or nil."
  [predicate sequence & {:keys [start end key]}]
  (loop [result 0 
         s (or (and start (drop start sequence)) sequence)
         n 0]
    (if (or (empty? s)
            (and end (= n end)))
      result
      (let [item (first s)
            item-val (if key (key item) item)]
        (recur (if (predicate item-val)
                 (+ result 1)
                 result)
               (rest s)
               (+ n 1))))))

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

;; There are more elegant ways to express this, I wrote it before I knew how...
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
  (doseq [x seq] (println x)).  Any non-sequence arguments are printed as if by (println x).
  Returns nil."
  [& args]
  (doseq [x args]
    (if (and (not (string? x)) (seqable? x))
      (doseq [x x] (println x))
      (println x))))
;;(printlines '(a b c)) => a b c (at one line each)
;;(printlines 1 '(2 3 4) [5 6 7]) => 1 2 3 4 5 6 7 (at one line each)
;;(printlines "abc") => "abc" (as one string)

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

(defn parse-path 
  "Given a String which starts with file path optionally followed by other stuff
  return a vector of two strings 
  (1) string with the first file path listed
  (2) a substring with the remainder of the string following the matched file path
  
  Returns logical false if there no match.

  A path is basically an unquoted sequence of non-whitespace characters, 
  or a quoted sequence of characters.  In the case of quoted paths, strips out the quotes or unquotes
  quoted elements.

  Only parses double quoted paths for now, not single quoted paths."
  [s]
  (or (and (not (.startsWith s "\""))
           (let [m1 (re-matcher #"[^\"\p{Space}]+" s)]
             (and (.find m1 0)
                  [(.group m1 0) (.substring s (.end m1))])))
      (loop [chars (rest s) result [] escape nil]
        (if (empty? chars)
          false                         ;never found terminating quote
          (let [char (first chars)]
            (cond escape
                  (recur (rest chars) (conj result char) false) ;add escaped char
                  (= char \\)
                  (recur (rest chars) result true) ;skip escape char
                  (= char \")
                  [(String. (into-array Character/TYPE result))
                   (String. (into-array Character/TYPE (rest chars)))] ;found string termination, done
                  :else
                  (recur (rest chars) (conj result char) false)))))))
;; (parse-path "abc def") => ["abc" " def"]
;; (parse-path "\"ab \\\"cde\" def") => ["ab \"cde" " def"]

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

(defn resource-as-file
  "Given the name or relative string path of file in the leiningen project's 'resources' directory
  return a File object that describes the file object.
  E.g. (resouce-as-file \"foo.sh\") => #<File \"/home/dtenny/clojure/ha/resources/foo.sh\"
  Returns nil if the specified resource file does not exist."
  [resource-relative-path]
  (clojure.java.io/as-file
   (clojure.java.io/resource resource-relative-path)))

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

(defn mapply
  "Applies a function f to the argument list formed by concatenating
  everything but the last element of args with the last element of
  args.  This is useful for applying a function that accepts keyword
  arguments to a map.  E.g.

  (defn foo [a b & {:keys [c d]}]
    (println 'foo a b c d))
  (defn bar [a b & {:keys [c d] :as options}]
    (mapply foo a b options))

  Because you can NOT do 
  (apply foo a b options) in bar."
  [f & args]
  (apply f (apply concat (butlast args) (last args))))


(defn map-keys-valid?
  "Given a map m, verify that all keys in 'm' are in the collection 'valid-keys',
  Return logical true if all keys in the map are valid, logical false if they are not.
  If there are many keys, the caller should probably specify the valid keys
  as a set.  See also: validate-map-keys.

  While you could implement this as something like
  (empty? (clojure.set/difference (into #{} (keys m)) (into #{} valid-keys)))
  the present implementation is expected to be more efficient for most use cases.
  It's also more readable when called in my book: -jdt."
  [m valid-keys]
  (loop [ks (keys m)]
    (if (empty? ks)
      true
      (let [k (first ks)]
        (if (contains-value? valid-keys k)
          (recur (rest ks))
          false)))))

(def ^{:dynamic true :private true
       :doc "Function to construct exception, because I wanted to reuse a function I didn't 
            normally want to throw IllegalArgumentException."}
  *validate-maps-exception* 
  (fn [str] (Exception. str)))

(defn validate-map-keys 
  "Given a map m, verify that all keys in 'm' are in the collection 'valid-keys',
  Throw an exception if this is not the case.  Useful to ensure that 
  a map contains only the keys you expect and not something you'll ignore, 
  such as for keyword maps on function calls.

  Return nil if there are no failures.
  If there are many keys, the caller should probably specify the valid keys
  as a set."
  [m valid-keys]
  (doseq [k (keys m)]
    (if-not (contains-value? valid-keys k)
      (throw (*validate-maps-exception* (str "Key '" k "' in map is not a valid key."))))))

(defn validate-fn-keywords
  "Given a function Var, or other objects that can be fed to 'arglist' to get argument list information
  about a function, and some map representing keywords passed to the function, 
  do one of the following:
  1) If we can't obtain arglist information or you don't have any keywords specified in the funciton 
     definition throw an IllegalStateException. Sorry, you're out of luck and can't use 
     validate-fn-keywords.
  2) If we obtain arglist information with keywords, validate keywords in the passed map
     to ensure they're known to the function declaration, and throw IllegalArgumentException
     if they were not expected (declared).
  If keywords passed muster, return nil.

  Example: (defn foo [& {:keys [a b] :as options}]
              {:pre ... stuff }
              (validate-fn-keywords foo options)
              ... rest of functiobn ...)

  Note that in the above case we're going to reverse engineer the function object bound to foo
  to find the argument metadata.  You could also specify #'foo for a more sure-fire bet.
  If you have the valid keywords in some other plase, use 'validate-map-keys' instead, 
  this function is for parsing arglist metadata about functions and applying that information
  to the map validatiobn.

  Caveat: if you have multiple maps in your function declaration, we only observe keywords form the 
  last map."
  [arglist-input map-to-validate]
  (let [arg-lists (arglist arglist-input)]
    (if-not arg-lists
      (throw (IllegalStateException. (str "Arglist info was not found for " arglist-input))))
    ;; (defn foo [a b & {:keys [c d]}])         =>  ([a b & {:keys [c d]}])
    ;; (defn bar [a b {:keys [c d]}])           =>  ([a b {:keys [c d]}])
    ;; (defn baz [{:keys [a]} {:keys [b]}])     =>  ([{:keys [a]} {:keys [b]}])
    ;; (defn ack [a b & {c :c d :d}] [a b c d]) =>  ([a b & {c :c, d :d}])
    ;; Note that you can't declare multi-arity functions multiple variadic forms and some other restrictions.
    ;; Not going to worry about it now.

    ;; Now ... gather up the keywords from all arg lists, which probably isn't the right thing to do
    ;; but will serve as a start.  Scan the vectors returned by arglist to find maps and pick put the :keys
    ;; elements.  We'll only take the last map in each arglist.
    (let [destructuring-maps
          (filter identity
                  (map (fn [arglist] 
                         (let [hopeful-map (last arglist)]
                           (and (map? hopeful-map)
                                hopeful-map)))
                       arg-lists))
          ;; destructuring map will look like {:keys [a b]} or {c :c}, etc
          allkeys (apply concat
                         (map (fn [destructuring-map]
                                (or (empty->nil (map keyword (:keys destructuring-map)))
                                    (vals destructuring-map)))
                              destructuring-maps))]
      ;; Okay, all-keys is now a list of keywords, do the map validation
      (binding [*validate-maps-exception* (fn [str] (IllegalArgumentException. str))]
        (validate-map-keys map-to-validate allkeys)))))

(defn key-seq->map 
  "Use elements of sequence 's' as key in a map, assigning each key the value 'v'.
  Return the resulting map.  Current implementation is not lazy, seq will be realized.
  Example: (key-seq->map [1 2 3] 'x) => {1 x, 2 x, 3 x}"
  [s v]
  (loop [result {} s s]
    (if (empty? s)
      result
      (recur (assoc result (first s) v) (rest s)))))

(defn invert-1-to-many-map
  "Given a map with singleton keys and sequence values,
  invert yielding a map keyed by the original individual values,
  and valued by seq of keys that originally indirectly referenced the value.
  The resulting map values may be of varying types, e.g. seqs OR lists OR vectors, so
  if you care about it you'll need to clean it up (or we need to fix this function.)
  Example: 
  (invert-1-to-many-map {:a [1 2] :b [2 3 4] :c [1] :d [4 5]})
  => {1 (:a :c), 2 (:a :b), 3 [:b], 4 (:b :d), 5 [:d]}"
  [map]
  (loop [result {} kvs (seq map)]
    (if (empty? kvs)
      result
      (let [kv (first kvs)]
        (recur (merge-with concat result (key-seq->map (second kv) [(first kv)]))
               (rest kvs))))))

(defn repl-loaded? 
  "Return true if it looks like we have the REPL available in our lisp."
  []
  ;; *TODO*: have a interactive? predicate, maybe satisfied form inspecting
  ;; some state in repl namespaces?
  (some (fn [ns] (= (ns-name ns) 'clojure.tools.nrepl)) (all-ns)))

