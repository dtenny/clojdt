(ns jdt.shell-test
  (:require [clojure.test :refer :all]
            [jdt.shell :refer :all]))

#_
(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(deftest test-bash-tilde-expansion
  (is (= (bash-tilde-expansion "abc") "abc"))
  (is (= (bash-tilde-expansion "~abc") "/home/abc"))
  (is (= (bash-tilde-expansion "~abc/") "/home/abc/"))
  (is (= (bash-tilde-expansion "~abc/def") "/home/abc/def"))
  (is (= (bash-tilde-expansion "~+") (System/getProperty "user.dir")))
  (is (= (bash-tilde-expansion "~+abc") "~+abc"))
  (is (= (bash-tilde-expansion "~+/") (str (System/getProperty "user.dir") "/")))
  (is (= (bash-tilde-expansion "~+/def") (str (System/getProperty "user.dir") "/def")))
  (is (= (bash-tilde-expansion "~") (str (System/getProperty "user.home"))))
  )
