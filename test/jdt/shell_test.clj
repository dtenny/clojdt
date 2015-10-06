(ns jdt.shell-test
  (:require [clojure.test :refer :all]
            [jdt.shell :as sh :exclude [cat]])) ;clojure 1.7 has 'cat' in clojure.core

#_
(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))

(deftest test-bash-tilde-expansion
  (is (= (sh/bash-tilde-expansion "abc") "abc"))
  (is (= (sh/bash-tilde-expansion "~abc") "/home/abc"))
  (is (= (sh/bash-tilde-expansion "~abc/") "/home/abc/"))
  (is (= (sh/bash-tilde-expansion "~abc/def") "/home/abc/def"))
  (is (= (sh/bash-tilde-expansion "~+") (System/getProperty "user.dir")))
  (is (= (sh/bash-tilde-expansion "~+abc") "~+abc"))
  (is (= (sh/bash-tilde-expansion "~+/") (str (System/getProperty "user.dir") "/")))
  (is (= (sh/bash-tilde-expansion "~+/def") (str (System/getProperty "user.dir") "/def")))
  (is (= (sh/bash-tilde-expansion "~") (str (System/getProperty "user.home"))))
  )
