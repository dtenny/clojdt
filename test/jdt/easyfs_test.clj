(ns jdt.easyfs-test
  (:require [clojure.test :refer :all]
            [jdt.easyfs :refer :all]))

#_
(deftest a-test
  (testing "FIXME, I fail."
    (is (= 0 1))))


;;; Figure out how to use fixtures to set up some special files for tesitng
;;; (setup/teardown infrastructure for this type of testing)


(deftest test-exists
  (testing "exists? and not-exists?"
    (assert (exists? "~"))
    (assert (not-exists? "~" {:no-tilde true})))) ;file doesn't exist, much less is it a directory

(deftest test-dir
  (testing "dir?"
    (assert (dir? "~"))
    (assert (not (dir? "~" {:no-tilde true}))))) ;file doesn't exist, much less is it a directory

(deftest test-file
  (testing "file?"
    (assert (file? "~/.bashrc"))
    (assert (not (file? "~")))))
;; *TODO*: test symbolic links with 'file?'

;; *FINISH*: test 'link?'

(deftest test-exe
  (testing "exe?"
    (assert (not (exe? "~/.bashrc")))
    (assert (exe? "/usr/bin/ls"))))
;; *TODO*: test symbolic links and following(?) with exe?

(deftest test-hidden
  (testing "hidden?"
    (assert (hidden? "~/.bashrc"))
    (assert (not (hidden? "/usr/bin/ls")))))

(deftest test-readable
  (testing "readable?"
    (assert (readable? "~/.bashrc"))))
;; *TODO*: test case where file isn't readable, need a chmod or something for that

(deftest test-writable
  (testing "writable?"
    (assert (writable? "~/.bashrc"))))
;; *TODO*: test case where file isn't writable, need a chmod or something for that

(deftest test-same-file
  (testing "same-file?"
    (assert (same-file? "~/.bashrc" "/home/./dave/.bashrc"))
    (assert (not (same-file? "~/.bashrc" "~/clojure")))))
            
(deftest test-to-path
  (testing "to-path"
    (assert (= (jdt.easyfs/to-path "~/foo") (jdt.easyfs/expand "~/foo" nil)))
    (assert (= (.startsWith (jdt.easyfs/to-path "~/foo" {:no-tilde true}) "~")))))
    
(deftest test-children
  (testing "children"
    (assert (instance? java.nio.file.Path (first (children "~"))))
    (assert (nil? (children "~" {:no-tilde true})))))

(deftest test-accept
  (testing ":accept"
    (assert (< (count (children "~" {:accept #(not (hidden? %))}))
               (count (children "~"))))))

;; *TODO*: test these: owner, group, ctime, mtime, atime, file-key, size, file-store,
;; perm-keys, perm-string, content-type, read-bytes, read-lines, read-symlink,
;; children
