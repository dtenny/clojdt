(ns
    ^{:doc "Wrap java.io.closable objects in a machanism that will eventually ensure they're closed."}
    jdt.closer
  (import java.io.Closeable)
  (import [java.lang.ref PhantomReference ReferenceQueue]))


(defonce ^{:private true
           :doc "Queue to receive PhantomReference subtypes when they're ready to GC."}
  queue (ReferenceQueue.))

(defn ensure-close
  "Ensure that a Closable object will have .close invoked on it when it's GC'd"
  [obj]
  {:pre (instance? Closeable obj)}
  (proxy [PhantomReference clojure.lang.IDeref] [obj queue] ;second vector are constructor/super args
         (deref [] obj)))               ;@(closer "foo") => "foo"
    
(defn- process-queue []
  (while true
    (let [reference (.remove queue)]
      (try
        (.close @reference)
        (catch Exception e
          (clojure.repl/pst e))))))

(defonce ^{:private true :doc "Process dead Closables in the queue"}
  the-thread
  (doto (Thread. process-queue)
    (.setDaemon true)
    (.start)))
           
(println "*FINISH* test closer.clj, possibly add optional 'flush' arg to ensure-close, tie into dir-seq")
    
