(ns jdt.core-test
  (:require [clojure.test :refer :all]
            [jdt.core :refer :all]))

#_
(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

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
          

