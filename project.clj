(defproject jdt "0.1.2"
  :description "These are not the tools you're looking for."
  :url "https://github.com/dtenny/clojdt"
  :scm {:name "git"
        :url "git@github.com:dtenny/clojdt.git"}
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [;[org.clojure/clojure "1.6.0"]
                 [org.clojure/clojure "1.5.1"]
                 [me.raynes/conch "0.6.0"]
                 [me.raynes/fs "1.4.5"] ; presently a locally managed repo
                 [org.clojure/java.classpath "0.2.2"]
                 [enlive "1.1.5"] ; net.cgrand.enlive-html - for html parsing (and minor fetching)
                 [ch.ethz.ganymed/ganymed-ssh2 "261"]
                 ])
