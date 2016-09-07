(ns jdt.linux                           ;perhaps jdt.system-env instead?
  "Tools for gathering information about the Linux environment."
  )

;;; Various ways of getting the process id.
;;; 1) ManagementFactory.getRuntimeMXBean().getName() => 12345@hostname
;;; 2) Java 9: ProcessHandle.current().getPid(); - but may not work in security environment
;;; 3) What I used

(defn get-process-id 
  "Return the linux process id of the caller as an Integer."
  []
  (-> (java.io.File. "/proc/self") .getCanonicalFile .getName Integer/parseInt))


;; For cross-os stuff if you can accept N/A status for pids
(comment
  (defn- get-possible-process-id 
  "Return process id of the caller in a somewhat undependable fashion.
  Return -1 if it can't be determined.  This code will be called for OSX,
  since there is no /proc file system.  Note that Java9 has 
  ProcessHandle.current().getPid(), but we aren't on java9 yet."
  []
  ;; 12345@hostname
  (let [userAtHost (-> (ManagementFactory/getRuntimeMXBean) .getName)]
    (if-let [pid-str (second (re-matches #"([0-9]+)@.*" userAtHost))]
      (Integer/parseInt pid-str)
      -1)))

  (defn- get-linux-process-id 
  "Return linux process id of the caller.
  This is a reliable process ID on Linux or other systems that support
  compatible /proc semantics."
  []
  (-> (java.io.File. "/proc/self") .getCanonicalFile .getName Integer/parseInt))

  (defn- get-process-id
  "Return the process ID of the caller, or -1 if it can't be determined."
  []
  (try (get-linux-process-id)
       (catch Exception e
         (get-possible-process-id))))

  )
