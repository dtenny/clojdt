(ns ^{:doc "Java native SSH client tools"}
    jdt.ssh
  (:require [clojure.string :as string])
  (:import [ch.ethz.ssh2 Connection HTTPProxyData Session StreamGobbler]))

;;; This is what things look ilke if you don't use a native client instead shell out
;;; to the command line.  Yuck.  The problem is getting tty's.
#_
(defn- ssh-command
  "Perform an ssh command to a scribe host indicated by public dns name.
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
   E.g. (ssh \"mykey\" \"ec2-user\" 
                    \"ec2-23-23-83-179.compute-1.amazonaws.com\" \"ls\")
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
                if you select this option, i.e. CRLF instead of LF for remote linux commands."
  [command & {:keys [private-key target-user target-host proxy-host proxy-port tty]}]
  {:pre [private-key target-user target-host (if proxy-port (number? proxy-port) true)]}
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