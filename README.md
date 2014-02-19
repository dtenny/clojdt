# jdt

Seriously, these aren't the tools you're looking for.  I'm just messing
around. Think of this as a ~/.clojurerc in progress and hosted on github
to learn github-ish things.


## Usage

Add something like this to your ~/.lein/profiles.clj, or otherwise import
the kits.

```clojure
{:user {:dependencies [[jdt "0.1.0-SNAPSHOT"]
                       [org.clojure/tools.trace "0.7.6"] 
                       ]
        :injections [(use '[jdt core cl shell java])
                     (println "~/.lein/profiles.clj loaded jdt.*")
                     (use 'clojure.tools.trace)
                     (require 'clojure.inspector)
                     (println "~/.lein/profiles.clj use: clojure.tools.trace, require: clojure.inspector")
                     ]}}
```

## License

Copyright © 2013 Jeffrey D. Tenny

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
