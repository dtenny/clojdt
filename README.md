# jdt

Tools I prefer to have around.  Some are more useful from the REPL, some
are more useful for general coding.  

## Usage

Add something like this to your ~/.lein/profiles.clj, or otherwise import
the kits.

```clojure
{:user {:dependencies [[jdt "0.1.0-SNAPSHOT"]
                       [org.clojure/tools.trace "0.7.6"] 
                       ]
        :injections [(use 'jdt.cl)  ;pulls in jdtutil.core
                     (println "~/.lein/profiles.clj loaded jdt.{cl,core}")
                     (use 'clojure.tools.trace)
                     (println "~/.lein/profiles.clj loaded clojure.tools.trace")
                     ]}}
```

## License

Copyright © 2013 Jeffrey D. Tenny

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
