(ns jdt.core-test
  (:require [clojure.test :refer :all]
            [jdt.core :refer :all]))

#_
(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(deftest test-cl-find
  (testing "cl-find"
    (is (= (cl-find :a '(:b :a :c)) :a))
    (is (= (cl-find :d '(:b :a :c)) nil))
    (is (nil? (cl-find :a '(:b :a 1 :c 2 :a :e) :key str)))
    (is (= (cl-find ":a" '(:b :a 1 :c 2 :a :e) :key str)))
    (is (= (cl-find 1 '(1 0 -1 2 3 0) :test >) 0))
    (is (= (cl-find 1 '(1 0 -1 2 3 0) :test <) 2))
    (is (nil? (cl-find 1 '(1 0 -1 2 3 0) :test > :key (fn [x] (+ x 10)))))
    (is (= (cl-find 1 '(1 0 -1 2 3 0) :test < :key (fn [x] (+ x 10))) 1))
    (is (= (cl-find 1 '(1 0 -1 2 3) :start 1) nil))
    (is (= (cl-find 1 '(1 0 -1 2 3) :start 0) 1))
    (is (= (cl-find 1 '(1 0 -1 2 3 0) :end 1) 1))
    (is (= (cl-find 0 '(1 0 -1 2 3 0) :end 1) nil))
    (is (= (cl-find 0 '(1 0 -1 2 3 0) :end 2) 0))
    (is (= (cl-find 0 '(1 -1 2 3 0) :from-end true :test >) -1))
    (is (= (cl-find 0 '(1 -1 2 3 0) :start 2 :from-end true :test >) nil))
    (is (= (cl-find 0 '(1 -1 2 3 0) :start 1 :from-end true :test >) -1))
    ))

(deftest test-index-of
  (is (= (index-of pos? [-1 -2 -3]) nil))
  (is (= (index-of pos? [-1 -2 4]) 2)))

(deftest test-cl-eql
  (is (cl-eql 'a 'a))
  (is (cl-eql :b :b))
  (is (cl-eql \a \a))
  (is (not (cl-eql \a \b)))
  (is (cl-eql 1 1))
  (is (cl-eql 1.0 1.0))
  (is (cl-eql "a" "a")) ; true in clojure/jvm (interned strings), false in lisp, that's okay
  (is (not (cl-eql "a" "b"))))

(deftest test-cl-member
  (is (= (cl-member 2 '(1 2 3)) '(2 3)))
  (is (= (cl-member 2 '()) nil))
  (is (= (cl-member 1 '(1 2 3)) '(1 2 3)))
  (is (= (cl-member 1 '(1 2 3) :key (fn [x] (- x 1))) '(2 3)))
  (is (= (cl-member 'a '(b c d) :test (fn [x y] (not= x y))) '(b c d))))

(deftest test-assoc-if
  (testing "assoc-if"
    (let [l '[:a a :b b :c nil :d false]]
      (is (= (assoc-if {} nil :a nil) {}))
      (is (= (assoc-if {} nil :a false) {}))
      (is (= (assoc-if {} nil :a 'a) {:a 'a}))
      (is (= (apply assoc-if {:g "g"} (fn [k v] (not (nil? v))) l)) {:d false, :b 'b, :a 'a, :g "g"}))))
  
(deftest test-cl-count-if
  (is (= (cl-count-if odd? (range 0 6)) 3))
  (is (= (cl-count-if pos? (range 1 5) :key #(- % 3))  1))
  (is (= (cl-count-if #(> % 2) (range 0 5) :start 3) 2))
  (is (= (cl-count-if #(= (rem % 3) 0) (range 1 7) :end 2) 0))
  (is (= (cl-count-if #(= (rem % 3) 0) (range 1 7) :end 3) 1))
  (is (= (cl-count-if #(= (rem % 3) 0) (range 1 7) :end 4) 1))
  (is (= (cl-count-if #(= (rem % 3) 0) (range 1 7) :end 5) 1))
  (is (= (cl-count-if #(= (rem % 3) 0) (range 1 7) :end 6) 2))
  (is (= (cl-count-if #(= (rem % 3) 0) (range 1 7) :end 7) 2)) ; one past end
  (is (= (cl-count-if #(= (rem % 3) 0) (range 1 7) :end 7) 2))) ; two past end

(deftest test-and-let
  (is (nil? (and-let [x nil y 1] [x y])))
  (is (nil? (and-let [x 1 y nil] [x y])))
  (is (nil? (and-let [x false y nil] [x y])))
  (is (= (and-let [x true y 1] [x y]) [true 1]))
  (is (= (and-let [x true] x) true)))

(deftest test-seqable?
  (testing "seqable?"
    (is (seqable? ()))
    (is (seqable? {}))
    (is (seqable? []))
    (is (seqable? #{}))
    (is (seqable? "abc"))
    (is (seqable? (list)))
    (is (not (seqable? nil)))))
        
(deftest map-matches-1
  (testing "map-matches"
    ;; Use set equality to eliminate dependency on sequence order of keys from map implementation changes.
    (is (= (into #{} (map-matches "abc" (seq {:b-key "b" :a-key "a" :d-key "d"}) contained-in-string?))
           (into #{} '(:b-key :a-key))))))

(def ^:dynamic *lines*
  ["Zzzzzzzzzzzz"
   "The quick brown fox jumps over the lazy dog."
   "Now is the time for all good men to come to the aid of their country."
   "You fool!  Fool you!"
   "Nyuk Nyuk Nyuk."
   "Zzzzz"])

(deftest sms-1
  (testing "select-matching-strings"
    (is (= (select-matching-strings *lines* {:exclaim "!" :boring "." :wacky "Nyuk"} contained-in-string?)
           {:exclaim [(*lines* 3)] :boring [(*lines* 1) (*lines* 2) (*lines* 4)] :wacky [(*lines* 4)]}))
    (is (= (select-matching-strings *lines* {:exclaim #"!" :funny #"HAH"} re-find)
           {:exclaim [(*lines* 3)]}))
    (is (= (select-matching-strings *lines* {:repetitive #"(?i)the\s.*the\s" :distinctive #"\stheir\s"}
                                    re-find)
           {:repetitive [(*lines* 1) (*lines* 2)] :distinctive [(*lines* 2)]}))
    (is (= (select-matching-strings *lines* {:boring #"Zz{1,4}" :sleeping #"Zz{5,}"} re-matches)
           {:boring [(*lines* 5)] :sleeping [(*lines* 0)]}))))
    
    
(deftest test-merge-predicates
  (testing "merge-predicates"
    (let [pred (merge-predicates even? pos?)]
      (is (pred 2))
      (is (not (pred 1)))
      (is (not (pred -2))))
    (let [pred (merge-predicates even? false nil true)]
      (is (pred 2))
      (is (not (pred 1))))
    (is (nil? (merge-predicates :foo nil)))))
          

(deftest test-get-indexes
  (testing "get-indexes"
    ;; Use set equality to avoid map ordering dur to changes in map implementations as clojure/java
    ;; changes (and thus order of sequence output from maps).
    (is (= (into #{} (seq (get-indexes {:a 1 :b ["c" "d"]} [])))
           (into #{} '((:a) (:b 0) (:b 1)))))
    (let [form {:a 1 :b [{:c 2 :d [{:e 3 :f "abc"}]} {:g 1}]}
          indices (get-indexes form [])]
      (is (= (into #{} indices)
             (into #{} '((:a) (:b 0 :c) (:b 0 :d 0 :f) (:b 0 :d 0 :e) (:b 1 :g)))))
      (is (= (get-in form (nth indices 2)) "abc")))
    (let [form [10 11 12 {:a 1} [:b :c :d] {:e {:f :g}}]
          indices (get-indexes form [])]
      (is (= indices '((0) (1) (2) (3 :a) (4 0) (4 1) (4 2) (5 :e :f)))))))
    
(deftest test-with-output
  (testing "with-output"
    (let [file (java.io.File/createTempFile "core_test", "tmp")
          file2 (java.io.File/createTempFile "core_test", "tmp")]
      (is (= (with-output [(.getAbsolutePath file)] (print "hi") 'bye) 'bye))
      (is (= (slurp file) "hi"))
      (is (= (with-output [file] (print "bye") 'hi) 'hi))
      (is (= (slurp file) "bye"))
      (with-output [file]
        ;; The order of these isn't necessarily guaranteed by with-output, but we know the implementation
        ;; flushes *out* first.
        (clojure.pprint/cl-format *out* "h")
        (clojure.pprint/cl-format *err* "i"))
      (is (= (slurp file) "hi"))
      (with-output [file file2]
        (clojure.pprint/cl-format *out* "b")
        (clojure.pprint/cl-format *err* "ye"))
      (is (= (slurp file) "b"))
      (is (= (slurp file2) "ye"))
      (.delete file)
      (.delete file2))))

(deftest test-parse-path
  (is (= (parse-path "abc def") ["abc" " def"]))
  (is (= (parse-path "\"ab \\\"cde\" def") ["ab \"cde" " def"])))

(deftest test-mapply
  (let [foo (fn [a b & {:keys [c d]}]
              [a b c d])
        bar (fn [a b & {:keys [c d] :as options}]
              (mapply foo a b options))]
    (is (= (bar 1 2) [1 2 nil nil]))
    (is (= (bar 1 2 :c 3) [1 2 3 nil]))
    (is (= (bar 1 2 :d 4) [1 2 nil 4]))
    (is (= (bar 1 2 :c 3 :d 4) [1 2 3 4]))))

(deftest test-fn-name-info
  (is (= ["jdt.core" "fn-name-info"] (fn-name-info fn-name-info))))

(deftest test-arglist
  (is (= ['(f)] (arglist fn-name-info))))

(deftest test-contains-value?
  (is (contains-value? [:a :b :c] :c))
  (is (not (contains-value? [:a :b :c] :d))))

(deftest test-map-keys-valid?
  (is (map-keys-valid? {} []))
  (is (map-keys-valid? {:a 1} [:a]))
  (is (not (map-keys-valid? {:a 1 :b 2} [:a]))))

(deftest test-validate-map-keys
  ;; There isn't an (is-not) or (is (not (thrown?))).  So we're just checking fn returns.
  (is (not (validate-map-keys {} [])))
  (is (not (validate-map-keys {:a 1} [:a])))
  (is (thrown? Exception (validate-map-keys {:a 1 :b 2} [:a]))))

;; Function sfor validate-fn-keywords
(defn f1 [a b & {:keys [c d]}] [a b c d])
(defn f2 [a b {:keys [c d]}] [a b c d])
(defn f3 [{:keys [a]} {:keys [b]}] [a b ])
(defn f4 [a b & {c :c d :d}] [a b c d])
(defn f5 [a b & {:keys [c d] :as options}]
  (validate-fn-keywords f5 options)
  [a b c d])

(deftest test-validate-fn-keywords
  ;; Won't throw unless passed map has keywords not declared
  (is (not (validate-fn-keywords f1 {:c 1})))
  (is (not (validate-fn-keywords f1 {:d 2})))
  (is (not (validate-fn-keywords f1 {:c 1 :d 2})))
  (is (thrown? Exception (validate-fn-keywords f1 {:c 1 :d 2 :e 3})))
  (is (not (validate-fn-keywords f2 {:c 1})))
  (is (thrown? Exception (validate-fn-keywords f2 {:e 3})))
  (is (not (validate-fn-keywords f3 {:b 1})))
  (is (thrown? Exception (validate-fn-keywords f3 {:f 3})))
  (is (not (validate-fn-keywords f4 {:c 1 :d 2})))
  (is (thrown? Exception (validate-fn-keywords f4 {:d 2 :e 3})))
  (is (= (f5 1 2 :c 3) [1 2 3 nil]))
  (is (thrown? Exception (f5 1 2 :f 5)))
  (is (= (f5 1 2 :d 4) [1 2 nil 4]))
  (is (thrown? Exception (f5 1 2 :c 3 :d 4 :e 5))))


  
