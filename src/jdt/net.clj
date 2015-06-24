(ns jdt.net "Tools for gathering network information"
    )

(defn get-my-local-ip-address 
  "Return your local/private address (as a string), e.g. 192.168.1.6" 
  []
  (-> (java.net.InetAddress/getLocalHost) (.getHostAddress) (.toString)))

(defn get-my-remote-ip-address 
  "Return the ip address that is what the world sees if you reach out to them from the node
  (as a string), e.g. 173.76.26.45" 
  []
  (let [url (java.net.URL. "http://checkip.amazonaws.com")]
    (with-open [in (clojure.java.io/reader (.openStream url))]
      (.readLine in))))
  
