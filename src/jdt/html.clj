(ns jdt.html
  ;; (:require [org.httpkit.client :as client]) ; only if we need high performance fetching
  (require [net.cgrand.enlive-html :as enlive])
  (require clojure.string)
  (import java.io.File))

;; From playing around in ~clojure/test/htlm project.

(defn- fetch-javadoc-page
  "Retrieve an html parse tree in 'enlive' format.  Url-string can be network or file urls.
  Haven't tried it with jar files yet.
  The URL path should generally be xxx/docs/api/package/name/classname.html,
  e.g. file:///usr/java/jdk1.7.0_45/docs/api/java/lang/String.html."
  [url-string]
  (enlive/html-resource (java.net.URL. url-string)))

;; Intesting parts of a javadoc page (not exact, use pattern 
;; <div class=details
;;   <!-- ======== FIELD DETAIL ======== -->
;;   <a name="field_detail"
;;   <!-- ======= CONSTRUCTOR DETAIL =========== -->
;;   <a name="constructor detail"
;;   <ul class=blocklist
;;     <a name="String()"
;;   <!-- =========== METHOD DETAIL =========== -->
;;   <a name="method_detail"
;;   <ul class=blocklist
;;     <a name="indexOf(int)">

;; selectors: (see enlive markdown page)
;; (enlive/select string-page [[:a (enlive/attr= :name "field_detail")]])
;; Matches an <a> element with a name attribute of the given value.
;; (enlive/select string-page [:a (enlive/attr= :name "field_detail")])
;; Matches any element with a name attribute that is within an <a> element.

(defn- javadoc-page-nodes
  "Return html fragments for div class=header and div class=contentContainer,
  which is most of the interesting part of the page, for a 'page' returned by enlive/html-resource."
  [page]
  ;; For overall page: really want div.{header,contentContainer,inheritance,description,summary,details}
  ;; Note that contentContainer contains all the nodes listed to the rigt of it in the preceding line.
  ;; Maybe can use enlive/left[s] and enlive/right[s] as bounds?
  ;(enlive/select page [[:div (enlive/attr= :class "description")]])

  ;; This does it, though the two divs are returned in a sublist (({div}{div}))
  (first (enlive/select
          page {[[:div (enlive/attr= :class "header")]][[:div (enlive/attr= :class "contentContainer")]]}))
  )

(defn- javadoc-class-text 
  "Return basically the whole page of class documentation"
  [page]
  (enlive/texts (javadoc-page-nodes page)))

(defn- javadoc-method-text [page method-name]
  "Return the javadoc bits for methods signatures exactly matching the supplied name.
  Best seen through jdt.shell/less."
  ;; Fragments starting at <a> and ending at the next <ul>
  ;; Notes: range selectors can only be used as the top level node-selector, not further down in the expression.
  ;; Looking for the :ul required [:ul], a valid node-selector, not just :ul, for the end node.
  (let [node-list
        (enlive/select
         page {[[:div (enlive/attr= :class "details")] [:a (enlive/attr= :name method-name)]] [:ul]})]
    ;; There's a sequence of:
    ;; <a name="foo()">
    ;; <ul> ... </ul>
    ;; The ul has the method details
    (if-not (empty? node-list)
      (map clojure.string/trim (enlive/texts (first node-list))))))

;;; now a version of the above that will do a partial or regexp match on method-name

(defn- javadoc-method-text-1 [page method-name]
  "Return the javadoc bits for methods loosely matching the supplied name.  Best seen through jdt.shell/less."
  ;; Fragments starting at <a> and ending at the next <ul>
  ;; Notes: range selectors can only be used as the top level node-selector,
  ;; not further down in the expression.
  ;;
  ;; Looking for the :ul required [:ul], a valid node-selector, not just :ul, for the end node.
  ;;
  ;; The function to enlive/pred will receive a map representing an element, e.g.
  ;; {:tag :a, :attrs {:name 'field_detail}, :content '({:type :comment, :data  nil  })}
  ;; HOWEVER... the element content isn't necessarily fully formed, so don't go into it!
  (let [node-list (enlive/select page {[[:div (enlive/attr= :class "details")]
                                        [:a (enlive/pred
                                             #(when-let [v (:name (:attrs %))]
                                                (re-find (re-pattern (str "(?i)" method-name)) v)))]]
                                       [:ul]})]
    ;; There's a sequence of:
    ;; <a name="foo()">
    ;; <ul> ... </ul>
    ;; The ul has the method details
    (if-not (empty? node-list)
      (map clojure.string/trim (enlive/texts (first node-list))))))

(defn- remove-empty-leading-and-trailing-lines [seq]
  (reverse (drop-while empty? (reverse (drop-while empty? seq)))))

(defn fetch-javadoc-class-text
  "Return text describing a class given a javadoc class URL"
  [url-string]
  (remove-empty-leading-and-trailing-lines
   (javadoc-class-text (fetch-javadoc-page url-string))))

(defn fetch-javadoc-method-text-exact
  "Return the documentation for a method given a javadocc class url-string and an exact method signature.
   E.g. (this \"file:///usr/java/jdk1.7.0_45/docs/api/java/lang/String.html\" \"length()\")"
  [url-string method-signature]
  (remove-empty-leading-and-trailing-lines
   (javadoc-method-text (fetch-javadoc-page url-string) method-signature)))
   
(defn fetch-javadoc-method-text
  "Return the documentation for a method given a javadocc class url-string and an method signature
   string or regex fragment.
   E.g. (this \"file:///usr/java/jdk1.7.0_45/docs/api/java/lang/String.html\" \"length\")"
  [url-string method-signature]
  (remove-empty-leading-and-trailing-lines
   (javadoc-method-text-1 (fetch-javadoc-page url-string) method-signature)))

(def ^{:dynamic true} javadoc-dir "/usr/java/jdk1.7.0_45/docs/api")

(defn jdk-javadoc-url
  "Given a String representing package qualified java class name in the standard JDK,
  OR given a java.lang.Class object instance (e.g. java.lang.String), return an URL that can be used
  to look up javadoc for the class. (Consults javadoc-dir).
  E.g. \"java.lang.String\" might translate to 
  \"/usr/java/jdk1.7.0_45/docs/api/java/lang/String.html\""
  [class-or-class-name]
  (let [class-name (if (class? class-or-class-name) (.getName class-or-class-name) class-or-class-name)]
    (str "file://" javadoc-dir File/separator (.replace class-name "." File/separator) ".html")))

(defn test []
  (jdt.core/printlines (fetch-javadoc-method-text (jdk-javadoc-url "java.lang.String") "length"))
  (jdt.core/printlines (fetch-javadoc-method-text (jdk-javadoc-url String) "length")))

(println "(jdt.html/test) *FINISH*: use me in a an augmented 'doc' routine, support multiple url sources.")
