(ns ^{:doc "Java native SSH client tools"}
  jdt.ssh
  (:require [jdt.core :refer [remove-key-value-pair get-key-value printlines]])
  (:require [jdt.easyfs :refer [as-path abs-path parent file-name-string probe-file size]])
  (:require [clojure.java.io :refer [copy input-stream output-stream]])
  (:require [clojure.string :as string])
  (:import [ch.ethz.ssh2 Connection HTTPProxyData Session StreamGobbler SCPClient]))

;;; This is what things look ilke if you don't use a native client instead shell out
;;; to the command line.  Yuck.  The problem is getting tty's.
#_
(defn- native-ssh-command
  "Perform a native ssh command to a scribe host indicated by public dns name.
   E.g. (ssh-command \"ec2-23-23-83-179.compute-1.amazonaws.com\"
                     \"mykey\" \"ec2-user\" \"ls\")
   Return output if any or throw an exception if things don't work."
  [private-key user dnsname command]
  ;; Getting a pseudoterminal is a pain here, so we don't use the jdt.shell stuff.
  ;; *TBD* Better to use ganymed2 or something like that, long term, which has some
  ;; request[Dumb]Pty() methods in its Session api
  (let [ssh-args ["ssh" "-t" "-o" "StrictHostKeyChecking=false" "-i" private-key
                 "-l" user dnsname command]
        ssh-cmd (join " " ssh-args)
        cmdfile (spit "/tmp/ssh.cmd.sh" ssh-cmd)
        exec-cmd
        (if (System/console)
          ;; This is probably wrong, join ssh-cmd *FINISH*
          (into-array ["sh" "/tmp/ssh.cmd.sh" "</dev/tty" ">/tmp/ssh.out" "2>/tmp/ssh.err"])
          ;; xterm -e 'sh -c "echo abc >/tmp/foo 2>/tmp/bar"'
          (into-array ["xterm" "-e" "sh /tmp/ssh.cmd.sh >/tmp/ssh.out 2>/tmp/ssh.err"]))
        ^Process p (.exec (Runtime/getRuntime) exec-cmd)]
    ;;(println "Exec:" (join " " exec-cmd))
    (.waitFor p)
    (unless (= 0 (.exitValue p))
      (printlines (slurp "/tmp/ssh.err"))
      (throw (Exception. (str "exit value " (.exitValue p) " with ssh of " command))))
    (slurp "/tmp/ssh.out")))
  

(defn ssh
  "Perform command on target-host via ssh client on local host.
   Return a vector of [return-code stdout stderr] from the command.
   This suggests you don't want too much output from the command in question.
   since the stdout/err results are strings.

   Most parameters are specified by keyword, but that doesn't necessarily mean they're optional.
   The only positional command right now is the command to be executed on the remote host.
   
   Keywords:
   :private-key string specifying path to a private key file, analogous to '-i' in ssh command. Required.
   :target-user string specifying the name of the user on the remote host. Required.
   :target-host string specifying the remote host IP or DNS address. Required.
   :proxy-host  string specifying the proxy host IP or DNS address, if a proxy is to be used. Optional.
   :proxy-port  integer specifying the proxy host port number. Required if proxy-host specified.
   :tty         true if you want a pseudo-tty associated with the connection.  Optional.
                Note that you may see carriage returns appended to the EOL sequence for returned output
                if you select this option, i.e. CRLF instead of LF for remote linux commands.
   :verbose     if true, print out ssh parameters."
  [command & {:keys [private-key target-user target-host proxy-host proxy-port tty verbose]}]
  {:pre [private-key target-user target-host (if proxy-port (number? proxy-port) true)]}
  (if verbose
    (println "ssh" command "\n" :private-key private-key :target-user target-user 
             :proxy-host proxy-host :proxy-port proxy-port :tty tty))
  (let [result-rc (atom nil)
        result-stdout (atom nil)
        result-stderr (atom nil)]
    (with-open [connection (Connection. target-host)]
      (if proxy-host
        (.setProxyData connection (HTTPProxyData. proxy-host proxy-port)))
      (.connect connection)
      (if private-key
        (if-not (.authenticateWithPublicKey connection
                                            target-user (java.io.File. private-key) nil)
          (throw (Exception. (str "Unable to authenticate as user " target-user
                                  " with key file " private-key)))))
      (with-open [session (.openSession connection)]
        (if tty
          (.requestDumbPTY session)) 
        (.execCommand session command)  ; see FAQ, may want startShell()
        (with-open [err (StreamGobbler. (.getStderr session))]
          (with-open [out (StreamGobbler. (.getStdout session))]
            (reset! result-stdout (slurp out))
            (reset! result-stderr (slurp err))))
        ;; Not all servers may support rc, and getExitStatus returns null
        ;; if the status isn't available yet.  Consider a session.WaitForStatus(EOF|CLOSED)
        ;; or something like that.  Meanwhile this seems to work.  Might want to
        ;; limit the number of retries
        (loop [rc (.getExitStatus session)]
          (if-not rc
            (do (Thread/sleep 2000)
                (recur (.getExitStatus session)))
            (reset! result-rc rc)))))
    [@result-rc @result-stdout @result-stderr]))

(defn ssh-valid
  "Invoke 'ssh' with args as per 'ssh'.
   Throw an exception if the return code is not zero (include stderr output in exception)
   and otherwise return the standard output."
  [& args]
  (let [[rc stdout stderr] (apply ssh args)]
    (if (not= rc 0)
      (throw (Exception. (str rc " return from ssh command with " args
                              ", stderr= " \newline stderr))))
    stdout))

(defn print-ssh-output
  "Take the output of the ssh function and try to print it nicely 
  (as opposed to a big string with \\n escapes).
  Return the ssh return code."
  [ssh-result-vector]
  ;; Ssh output is [return-code stdout stderr]
  (let [[return-code stdout stderr] ssh-result-vector]
    (if (and stderr (> (count stderr) 0))
      (printlines (clojure.string/split-lines stderr)))
    (printlines (clojure.string/split-lines stdout))
    return-code))

(defn ssh-output-lines
  "Take the output of the ssh function and return a list of lines of standard output."
  [ssh-result-vector]
  (clojure.string/split-lines (nth ssh-result-vector 1)))


(defn ssh-test
  "Invoke a simple ssh command that does nothing too useful except verify
   that we're able to execute an ssh command on the remote host.
   Args are as per 'ssh', except that 'command' is implicit.
   (So specify all the keywords per ssh that you need, but not the command.).
   Return true if the ssh command worked, false if it did not."
  [& args]
  (try
    (let [test "Hello World"
          [rc stdout stderr] (apply ssh (str "echo " test) args)]
      (if (not= rc 0)
        false
        ;; Further validate command output
        (= test (string/trim stdout))))
    ;; Expected: ch.ethz.ssh2.HTTPProxyException: HTTP Proxy Error (503 Service Unavailable)
    ;; Unexpected: java.io.FileNotFoundException: (private key, usually)
    (catch Exception e false)))

(defn ssh-wait 
  "Wait until ssh-test says true. 
  Typically used to wait for sshd to become available on some booting cloud instance.
  Argument are as per the 'ssh' function, except that 'verbose' is not passed through to ssh
  unless it has the value '2' (as in, extra-verbose, or two levels, or whatever)."
  [& args]
  (let [v (get-key-value :verbose args)
        v2 (= v 2)
        args (remove-key-value-pair :verbose args)
        ssh-fn (fn []
                 (if v2
                   (apply ssh-test :verbose true args)
                   (apply ssh-test args)))]
    (if-not (ssh-fn)
      (do
        (when v
          (println "Waiting for remote host to respond to SSH requests.")
          (flush))
        (Thread/sleep 10000)
        (loop [ready (ssh-fn)]
          (if-not ready
            (do (when v
                  (print ".")
                  (flush))
                (Thread/sleep 10000)
                (recur (ssh-fn)))
            (when v (println))))))))

;; re: SCP: you'll want to read this
;; http://docstore.mik.ua/orelly/networking_2ndEd/ssh/ch03_08.htm
;; scp: undocumented -t is "to", -f is "from".
(defn- with-scp-client 
  "Set up an SCPClient instance and invoke function 'f' on it.  Return whatever 'f' returns.
  Keywords:
  :args               a (possibly empty) list of arguments for 'f'.  Typically source/dest pathnames.
                      The arguments are passed to 'f' after the SCPClient object via 'apply'.
                      'f' must take at least on argument (the SCPClient object), any more depends on 
                      what is passed via args (if any).
  :private-key string specifying path to a private key file, analogous to '-i' in ssh command. Required.
  :target-user string specifying the name of the user on the remote host. Required.
  :target-host string specifying the remote host IP or DNS address. Required.
  :proxy-host  string specifying the proxy host IP or DNS address, if a proxy is to be used. Optional.
  :proxy-port  integer specifying the proxy host port number. Required if proxy-host specified.
  :verbose     if true, print out other parameters before trying to connect."
  [f & {:keys [args private-key target-user target-host proxy-host proxy-port verbose]}]
  {:pre [target-user target-host (if proxy-port (number? proxy-port) true)]}
  (if verbose
    (println "scp" :private-key private-key :target-user target-user 
             :proxy-host proxy-host :proxy-port proxy-port))
  (with-open [connection (Connection. target-host)]
    (if proxy-host
      (.setProxyData connection (HTTPProxyData. proxy-host proxy-port)))
    (.connect connection)
    (if private-key
      (if-not (.authenticateWithPublicKey connection
                                          target-user (java.io.File. private-key) nil)
        (throw (Exception. (str "Unable to authenticate as user " target-user
                                " with key file " private-key)))))
    (apply f (SCPClient. connection) args)))
      
(defn scp-to
  "Perform scp command to target-host via ssh client on local host.
  Return the number of bytes copied if we succeed, nil (or throw an exception) if we fail.

  Most parameters are specified by keyword, but that doesn't necessarily mean they're optional.
  'source-path' string indicating path of file to be copied out.
  'dest-path'   string describing the path of the file to be created on the remote host.
  Any directory in the dest-path is assumed to exist on the remote host.
  
  Keywords:
  :private-key string specifying path to a private key file, analogous to '-i' in ssh command. Required.
  :target-user string specifying the name of the user on the remote host. Required.
  :target-host string specifying the remote host IP or DNS address. Required.
  :proxy-host  string specifying the proxy host IP or DNS address, if a proxy is to be used. Optional.
  :proxy-port  integer specifying the proxy host port number. Required if proxy-host specified.
  :verbose     if true, print out other parameters before trying to connect."
  [source-path dest-path & args]
  (let [local-path (abs-path (as-path source-path))
        local-path-dir-string (str (parent local-path))
        local-file-name-string (file-name-string local-path)
        remote-path (as-path dest-path)
        remote-path-dir-string (str (parent dest-path))
        remote-path-file-name-string (file-name-string dest-path)
        path-to-read (probe-file local-path)
        byte-length (and path-to-read (size path-to-read))]
    (if-not path-to-read
      (throw (Exception. (str source-path " does not exist or cannot be read."))))
    (apply with-scp-client
           (fn [scp-client]
             (with-open [put-stream (.put scp-client remote-path-file-name-string byte-length remote-path-dir-string nil)]
               (with-open [in-stream (input-stream source-path)]
                 (copy in-stream put-stream)
                 ))
             byte-length)
           args)))

(defn scp-from
  "Perform scp command from target-host via ssh client on local host.
   Returns nil if we succeed or throws an Exception otherwise.

  Most parameters are specified by keyword, but that doesn't necessarily mean they're optional.
  'source-path' string indicating path of file to be copied in from the remote host.
  'dest-path'   string describing the path of the file to be created on the local host.
  Any directory in the dest-path is assumed to exist on the local host.
  
  Keywords:
  :private-key string specifying path to a private key file, analogous to '-i' in ssh command. Required.
  :target-user string specifying the name of the user on the remote host. Required.
  :target-host string specifying the remote host IP or DNS address. Required.
  :proxy-host  string specifying the proxy host IP or DNS address, if a proxy is to be used. Optional.
  :proxy-port  integer specifying the proxy host port number. Required if proxy-host specified.
  :verbose     if true, print out other parameters before trying to connect."
  [source-path dest-path & args]
  ;; Try to open local path before we establish the connection, to fail-fast if necessary
  ;; due to permissions and whatnot.
  (with-open [out-stream (output-stream dest-path)]
    (apply with-scp-client
           (fn [scp-client]
               (with-open [in-stream (.get scp-client source-path)]
                 (copy in-stream out-stream)))
           args)))
      

