(ns ^{:doc "Utilities for working with Java"}
    jdt.java
    (:use jdt.core)
    (:require [clojure.java.classpath :as classpath])
    (:require [clojure.java.io :as io])
    ;;(:import java.io.File)
    (:import java.util.jar.JarFile)
    )

;; namespace-listing methods (for functions in a namespace)
;; java package/class listing methods (for class/functions/variables in a java class/package)

(defn classpaths
  "Return a sequence representing the java class path"
  []
  (seq (.getURLs (java.lang.ClassLoader/getSystemClassLoader))))

(defn classpath-jar-filename-apropos
  "Given a string or regular expression, find jar files in classpath whose names match the argument"
  [string-or-regexp]
    (filter (apropos-match-fn string-or-regexp)
            (filter classpath/jar-file? (classpaths))))

(defn jar-files
  "Given a jar file name, as from 'classpath-jar-filename-apropos', return a list of files in the jar.
  E.g. (jdt.java/jar-files (first (jdt.java/classpath-jar-apropos \"enlive\")))"
  [jar-filename]
  (classpath/filenames-in-jar (JarFile. (io/as-file jar-filename))))

(defn package-list [] (map #(.getName %1) (Package/getPackages)))

(defn package-apropos
  "Given a regular expression or stringable thing,
return a seq of all accessible java packages that match the string-or-regexp"
  [string-or-regexp]
    (filter (apropos-match-fn string-or-regexp) (map #(.getName %1) (Package/getPackages))))

(defn class-direct-methods 
  "Return a seq of Method members directly associated with a class 'c' (not including inherited behaviors."
  [c]
  (seq (.getDeclaredMethods c)))

(defn class-all-methods
  "Return a seq of Method members associated with a class 'c' whether directly or via its superclasses."
  [c]
  (loop [classes (conj (supers c) c) result []]
    (if (empty? classes)
      result
      (recur (rest classes) (concat result (seq (.getDeclaredMethods (first classes))))))))

(defn class-member-list
  "List public 'member' methods and fields of a Java class.
Not sure about inherited stuff and static methods."
  [class]
  ;; *TBD* Consider .getDeclaredMethods so we don't see supertype methods
  (concat (.getFields class) (.getMethods class)))

;; this function is not done yet, need to observe doInvoke vs. invoke and maybe return range
;; of min/max args (which don't map to parameters unless you inspect metadata arglist)
#_
(defn clojure-fn-arity
 "Returns the maximum parameter count of a function by examining each invoke method found by reflection
  on function. The returned value can be then interpreted as the arity
  of the input function. The count does NOT detect variadic functions, and any optional arguments
  will all be lumped into one function argument."
  [f]
  ;; Variadic functions will inherit/implement clojure.lang.RestFN which will define
  ;; (.getRequiredArity f) on the function to return the minimum required number of arguments.
  ;; These functions also use a 'doInvoke' api instead of a 'invoke' api.
  ;; Fixed argument functions will 'invoke' and no 'getRequiredArity'.
  (let [direct-methods-named-invoke
        (filter #(= "invoke" (.getName %)) (.getDeclaredMethods (class f)))]
    (reduce max 0 (map #(alength (.getParameterTypes %1)) direct-methods-named-invoke))))

(defn class-apropos "Perform apropos on a class and return class members that match str-or-pattern"
  [class str-or-pattern]
  (let [matches? (apropos-match-fn str-or-pattern)]
    (filter (fn [method] (matches? (.getName method))) (class-member-list class))))

#_
(defn find-classes 
  "Find package classes given a (File) directory to examine for package contents"
  [dir package-name classes]
  (clojure.pprint/cl-format *out* "Here I am on ~s\n" dir)
  (let [allFiles (.listFiles dir)
        ;; Assume files end with .class, may need to filter those that don't
        files (filter #((not (.isDirectory %1))) allFiles)
        ;; Assumes '.' and '..' aren't in the dirs
        dirs (clojure.set/difference (set allFiles) (set files))]
    (concat files (mapcat (fn [dir] (find-classes dir (str package-name "." (.getName dir)) classes)) dirs))))

#_
(defn package-list-classes
  "Find classes in the indicated package name"
  [packageName]
  (let [classLoader (.getContextClassLoader (Thread/currentThread))
        path (.replace packageName "." "/")
        resourceEnum (.getResources classLoader path) ;java Enum
        resourceSeq (enumeration-seq resourceEnum)
        dirs (map (fn [resource] (.getFile resource)) (enumeration-seq resourceEnum))
        classes (map (fn [dir] (find-classes dir packageName)) dirs)]
    (clojure.pprint/cl-format *out* "cl: ~s, path: ~s, resourceEnum: ~s, resourceSeq: ~s, dirs: ~s\n"
                              classLoader path resourceEnum resourceSeq dirs)
    (println classes)
    resourceEnum))

;; Turns out these class loaders are the same
;;(println (enumeration-seq (.getResources (.getContextClassLoader (Thread/currentThread)) "")))
;;(println (enumeration-seq (.getResources (ClassLoader/getSystemClassLoader) "")))

;;; Apparently there is no way to get a list of classes in a package,
;;; it requires smarter class loaders (which you can write but Java doesn't include by default)
;;; See http://code.google.com/p/reflections/ for a tool to do this stuff

(def ^{:tag ThreadGroup}               ;the type hint
  get-root-thread-group
  "Return the root ThreadGroup in the JVM. This doesn't change over the JVM lifetime and can be cached."
  (memoize 
   (fn []
     (loop [^ThreadGroup tg (.getThreadGroup (Thread/currentThread))]
       (let [^ThreadGroup parent (.getParent tg)]
         (if parent
           (recur parent)
           tg))))))

(defn get-thread-count 
  "Return an estimated count of active threads in the JVM, or specified ThreadGroup and its child groups
  if a ThreadGroup is specified."
  [& [^ThreadGroup tg]]
  (let [tg (or tg (get-root-thread-group))]
    (.activeCount tg)))

(defn get-all-threads
  "Return sequence containing all known threads in the JVM, or specified ThreadGroup and its child groups
  if specified. Bash users, consider 'jstack'."
  [& [^ThreadGroup tg]]
  (let [tg (or tg (get-root-thread-group))
        count (+ (get-thread-count tg) 20) ;in case someone is allocating threads, some headroom
        result-array (make-array Thread count)]
    (.enumerate tg result-array true)      ;recurse
    (filter identity (seq result-array))))
  
