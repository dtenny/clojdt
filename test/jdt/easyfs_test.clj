(ns jdt.easyfs-test
  (:require [clojure.test :refer :all]
            [jdt.easyfs :refer :all])
  (:import java.nio.file.NotDirectoryException))

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
  (testing "children and glob"
    (assert (instance? java.nio.file.Path (first (children "~"))))
    (is (= 1 (count (children "~" {:glob "*.bashrc"}))))))

(deftest test-bogus-dirspec
  (testing "bogus dirspec"
    ;; If we disallow non-directory specs to directory stream wrappers
    (is (thrown? NotDirectoryException (children "~/.bashrc")))
    ;; If we allow non-directory specs to directory stream wrappers
    #_(is (empty? (children "~/.bashrc")))
    ))

(deftest test-accept
  (testing "(children x {:accept fn})"
    (assert (< (count (children "~" {:accept #(not (hidden? %))}))
               (count (children "~"))))))

(deftest test-file-children
  (testing "file-children"
    (assert (not (some dir? (file-children "~"))))))

(deftest test-dir-children
  (testing "dir-children"
    (assert (not (some file? (dir-children "~"))))))

;; *TODO*: test these: owner, group, ctime, mtime, atime, file-key, size, file-store,
;; perm-keys, perm-string, content-type, read-bytes, read-lines, read-symlink,

