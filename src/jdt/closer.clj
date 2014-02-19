(ns
    ^{:doc "Wrap java.io.closable objects in a machanism that will eventually ensure they're closed."}
    jdt.closer
  (import java.io.Closeable)
  (import java.util.Vector)
  (import [java.lang.ref PhantomReference ReferenceQueue]))


(defonce ^{:private true
           :doc "Queue to receive PhantomReference subtypes when they're ready to GC."}
  reference-queue (ReferenceQueue.))

(defonce ^:private phantom-queue (Vector.))

(def ^{:private true :doc "Field with which to access the phantom object reference in a dirty way."}
  referent-field 
  (let [field (.getDeclaredField java.lang.ref.Reference "referent")]
    (.setAccessible field true)
    field))

(defn ensure-close
  "Ensure that a Closable object will have .close invoked on it when it's GC'd"
  [obj]
  {:pre (instance? Closeable obj)}
  (let [phantom-reference
        (proxy [PhantomReference] [obj reference-queue])] ;second vector are constructor/super args
    (.add phantom-queue phantom-reference))
  obj)
    
(defonce close-count (atom 0))

(defn- process-queue []
  (while true
    (let [reference (.remove reference-queue)
          obj (.get referent-field reference)]
      (println "process-queue: got one!")
      (try
        (when (instance? java.io.Flushable obj)
          (try
            (.flush obj)
            (catch Exception e
              (clojure.repl/pst e))))
        (.close obj)
        (swap! @close-count inc)
        (catch Exception e
          (clojure.repl/pst e)))
      ;; This enables final removal from heap
      (.remove phantom-queue reference))))

(defonce ^{:private true :doc "Process dead Closables in the queue"}
  the-thread
  (doto (Thread. process-queue)
    (.setDaemon true)
    (.start)))
           
#_
(defn- test []
  (jdt.shell/rm "/tmp/foo")
  (let [x (ensure-close (clojure.java.io/writer "/tmp/foo"))]
    (.write x "hello world")
    (System/gc)
    (println (jdt.shell/ls "-l" "/tmp/foo")))
  (System/gc)
  (println (jdt.shell/ls "-l" "/tmp/foo")))

;; What about lazily creating and starting the thread based on the first call to ensure-close?
;; More tests.  Then wrap lazy directory or other type FS metods.
