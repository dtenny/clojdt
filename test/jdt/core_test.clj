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

(deftest test-assoc-if
  (testing "assoc-if"
    (let [l '[:a a :b b :c nil :d false]]
      (is (= (assoc-if {} nil :a nil) {}))
      (is (= (assoc-if {} nil :a false) {}))
      (is (= (assoc-if {} nil :a 'a) {:a 'a}))
      (is (= (apply assoc-if {:g "g"} (fn [k v] (not (nil? v))) l)) {:d false, :b 'b, :a 'a, :g "g"}))))
  
(deftest test-seqable?
  (testing "seqable?"
    (is (seqable? ()))
    (is (seqable? {}))
    (is (seqable? []))
    (is (seqable? #{}))
    (is (seqable? (list)))
    (is (not (seqable? nil)))))
        
        

(deftest map-matches-1
  (testing "map-matches"
    (is (= (map-matches "abc" (seq {:b-key "b" :a-key "a" :d-key "d"}) contained-in-string?)
           '(:b-key :a-key)))))

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
    (is (= (get-indexes {:a 1 :b ["c" "d"]} [])
           '((:a) (:b 0) (:b 1))))
    (let [form {:a 1 :b [{:c 2 :d [{:e 3 :f "abc"}]} {:g 1}]}
          indices (get-indexes form [])]
      (is (= indices '((:a) (:b 0 :c) (:b 0 :d 0 :f) (:b 0 :d 0 :e) (:b 1 :g))))
      (is (= (get-in form (nth indices 2)) "abc")))
    (let [form [10 11 12 {:a 1} [:b :c :d] {:e {:f :g}}]
          indices (get-indexes form [])]
      (is (= indices '((0) (1) (2) (3 :a) (4 0) (4 1) (4 2) (5 :e :f)))))))
    
