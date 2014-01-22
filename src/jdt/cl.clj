(ns jdt.cl
  (:import (java.lang.management ManagementFactory MemoryMXBean MemoryPoolMXBean))
  (:use jdt.core)
  (:use clojure.pprint))                ;cl-format and friends

;;; Common Lisp transition utilities

;;; Things I'd like:
;; cl case-insensitivce symbols and string comparisons that work
;; FUNCALL
;; defun (with docstring before args)
;; one or more of defvar/defparameter/defconstant (with implied dynamic bindings)
;; STEP/TRACE/DEBUG/BREAK .... debugging!
;; LET ... that processed dyanmic bindings (need to use "binding")

;; Defvar or defspecial
;; (defvar name init doc) -> (def name doc init)
;;     (or maybe defonce) (note that 'def' is a special form and defines a Var)
;;     BINDING vars need ^:dynamic in their meta data, which is poorly documented

;; defgeneric -> defmulti

;; Optional vars:
;; (defun foo (a b &optional c d) ...)  -> (defn foo [a b & [c d]) ...)
;; Keyword vars:
;; (defn bar (&key a (b 10)) ...) -> (defn bar [{:keys [a b] :or {b 10}}] ...)
;; (bar :a 1)  ->  (bar {:a 1})

;; For default Optional vars, need to use arity, e.g.
;; (defn make-rectangle
;;     ([width] (make-rectangle width width))
;;     ([width height] {:width width :height height}))

;; Sample, not ready for prime time
#_
(defmacro defvar
  "Defines a var with an optional intializer and doc string"
  ([name]
     (list `def name))
  ([name init]
     (list `def name init))
  ([name init doc]
     (list `def (with-meta name (assoc (meta name) :doc doc)) init)))

;; (use 'clojure.walk) -> provides macroexpand-all, useful sometimes where macroexpand isn't enough
;; recursive macros like and-let being one of those cases

(defmacro caar
  "This is just to remind me about a Clojure thing that exists,
except that ffirst operates on seq, unlike lisp's caar.  Don't forget destructuring bindings too."
  [x] `(ffirst ~x))

(defmacro cadr
  "This is just to remind me about a Clojure thing that exists, except
that fnext operates on any seq, unlike lisp's cadr."
  [x] `(fnext ~x))

(defmacro cddr
  "Remind me to use nthrest"
  [x] `(nthrest ~x 2))

(defn- room-nil
  "Print to *out* the free memory available in bytes"
  []
  ;; TODO: Integrate  maxMemory & totalMemory into output, and also gc regions
  ;; (from com.sun.management.GcInfo and friends, or java.lang.management.MemoryPoolMXBean ?)
  (cl-format true "~%Available memory: ~:d bytes~%" 
             (.. (Runtime/getRuntime) freeMemory)))

(defmacro progn [& body] `(do ~@body))

(defn- printMemoryUsage
  "Print data derived from a MemoryUsage object"
  [memoryUsage]
  (cl-format true "Init: ~13:d  Committed: ~13:d  Max: ~13:d  Used: ~13:d~%"
             (.getInit memoryUsage)
             (.getCommitted memoryUsage)
             (.getMax memoryUsage)
             (.getUsed memoryUsage)))

(defn- room-t
  "Print to *out* a verbose display of memory management information."
  []
  (let [garbageCollectorMXBeans (ManagementFactory/getGarbageCollectorMXBeans)
        memoryMXBean (ManagementFactory/getMemoryMXBean)
        memoryPoolMXBeans (ManagementFactory/getMemoryPoolMXBeans)]

    (println)                           ;MemoryMXBean
    (cl-format true "Heap Memory:     ") (printMemoryUsage (.getHeapMemoryUsage memoryMXBean))
    (cl-format true "Non-Heap Memory: ") (printMemoryUsage (.getNonHeapMemoryUsage memoryMXBean))

    (println)                           ;GarbageCollectorMXBeans
    (cl-format true "Memory Managers~%")
    (doseq [garbageCollectorMXBean garbageCollectorMXBeans]
      (cl-format true "  ~17a  Total Collections: ~7d  Total Elapsed MS: ~9d~%"
                 (.getName garbageCollectorMXBean)
                 (.getCollectionCount garbageCollectorMXBean)
                 (.getCollectionTime garbageCollectorMXBean)))

    (println)
    (println "Memory Pools")            ;MemoryPoolMXBeans
    (doseq [memoryPoolMXBean memoryPoolMXBeans]
      (cl-format true
                 ;; width from args (~v...), relative indentation (@T), comma separators (:d)
                 "~17a ~10a ~13:d bytes used~%" 
                 (.getName memoryPoolMXBean)
                 (.toString (.getType memoryPoolMXBean))
                 (.getUsed (.getUsage memoryPoolMXBean)))


      (cl-format true "  CollectionUsageThreshold: ~13:d     UsageThresholdCount ~9:d~%"
                 (if (.isCollectionUsageThresholdSupported memoryPoolMXBean)
                   (.getCollectionUsageThreshold memoryPoolMXBean)
                   "N/A")
                 (if (.isUsageThresholdSupported memoryPoolMXBean)
                   (.getUsageThresholdCount memoryPoolMXBean)
                   "N/A"))
      (and-let [usage (.getCollectionUsage memoryPoolMXBean)]
               (cl-format true "  Last Collection Usage: ")
               (printMemoryUsage usage))
      (and-let [usage (.getPeakUsage memoryPoolMXBean)]
               (cl-format true "  Peak Usage:            ")
               (printMemoryUsage usage))
      (and-let [usage (.getUsage memoryPoolMXBean)]
               (cl-format true "  Current Usage:         ")
               (printMemoryUsage usage))
      )))

(defn room
  "A Common Lisp style ROOM function, printing memory information to *out*.
With no arguments or nil/false for the optional argument, print minimal memory information.
With any non-false optioanl argument (e.g. (room true)), print verbose memory system information."
  ([] (room-nil))
  ([arg] (if arg (room-t) (room-nil))))

;;(room)
;;(room true)
;;(room nil)

;; OK, my "describe", and/or my "apropos"
;; The clojure "apropos" form doesn't print packages of symbols.  Maybe that's okay.
;; The clojure "doc" won't find symbols however if they're not appropriately packaged qualified
;; because it searches only for vars or special forms, not namespace entries.
;; (And you won't know a symbol's namespace printed by clojure's apropos.)
;;
;; So we have two solutions to the problem: Make an apropos that prints the package, or a
;; doc variant that can find the symbols when incorrectly package qualified, or, at least, that can
;; find symbols in namespace use lists and such.  

(declare describe-aux)                  ;forward declaration, ugh
(defn- describe-var [indent object stream]
  (let [value (var-get object)]
    (cl-format stream "~%~v@Twith value ~s~%~v@Tof type ~s~%" indent value indent (type value))))

;; Note tha symbols have name and namespace componets, but I'm not sure how often the namespace
;; component is filled in.  If you just type 'foo when in the 'user namespace, it will not have a namespace
;; because it isn't automatically interned.  Similarly, the symbols used in various namespace maps
;; are NOT namespace qualified (because symbol equality *does* consider the namespace component),
;; and when we search for an  unqualified symbol in namespaces, we find them, try 'replace for example.
(defn- describe-symbol [indent object stream ns]
  "If ns is nil, check for symbol in all namespaces, otherwise only look for the symbol
with respect to the indicated namespace"
  (with-local-vars [things-to-look-up (transient #{})]
    (doseq [ns (or (and ns [ns]) (all-ns))]
      (let [ns-map (ns-symbol-relationship ns object)]
        (unless (empty? ns-map)
          (cl-format stream "~%~v@Tin namespace ~a:~%~v@T~s~%"
                     indent (ns-name ns) (+ indent 2) ns-map)
          (if-let [interns (get ns-map :interns)]
            (var-set things-to-look-up (conj! (var-get things-to-look-up) interns)))
          (if-let [maps-to (get ns-map :maps-to)]
            (var-set things-to-look-up (conj! (var-get things-to-look-up) maps-to))))))
    (doseq [thing (persistent! (var-get things-to-look-up))]
      (describe-aux indent thing stream))))

(defn- print-metadata-doc 
  "Given a metadata :doc value DOC,
print each line of the doc string to STREAM, with minimum indentation INDENT."
  [indent doc stream]
  ;; We don't (use 'clojure.string) becaused it conflicts wth a couple of symbols in other clojure namespaces
  (doseq [line (clojure.string/split-lines doc)]
    (cl-format stream "~v@T~a~%" indent line)))

(defn- describe-aux
  [indent object stream]
  (cl-format stream "~%~v@T~s~%~v@Tis a ~s~%" indent object (+ indent 2) (type object))

  ;; Meta data
  (if-let [meta-stuff (meta object)]
    (do
      (cl-format stream "~%~v@Twith metadata:~%" (+ indent 2))
      ;; Print non-:doc keys first, then the :doc string last
      (if-let [meta-no-key (dissoc meta-stuff :doc)]
        (doseq [[k v] (seq meta-no-key)]
          (cl-format stream "~v@T~s ~s~%" (+ indent 4) k v)))
      (if-let [doc (get meta-stuff :doc)]
        (do 
          (cl-format stream "~v@T:doc~%" (+ indent 4))
          (print-metadata-doc (+ indent 4) doc stream)))))

  ;; Type specific data
  (cond (symbol? object) (describe-symbol (+ indent 2)
                                          (symbol (name object)) ;strip out the namespace
                                          stream (symbol-namespace object))
        (var? object) (describe-var (+ indent 2) object stream))
  nil)
  
;; *TODO* Don't dump namespace info for symbols when the namespace only :maps-to or :refers-to
;; because the namespace happened to use another namespace.  The interesting cases are
;; those namespaces that intern the symbol, I think.

;; See jdtutil.core/symbol-namespace for some interesting tips on symbols.
(defn describe
  "Describe hopefully useful properties of the indicate object.

NOTE: if object is a symbol without a namespace qualification, we'll
examine the symbol in all namespaces, which can make for lengthy output for some symbols
(try (describe 'replace) to see a bad example).  Remember that unlike Common Lisp, symbols in Clojure
are not interned (thus associated with a namespace) by default."
  ([object] (describe object *out*))
  ([object stream] (describe-aux 0 object stream)))

;;(describe "abc")
;;(describe (quote ^{:doc "abc doc"} abc))
;;(describe 'describe)
;;(describe 'describe-aux)
;;(describe 'replace) ; long output
;;(describe 'user/replace) ; shorter output
