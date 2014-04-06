(ns jdt.easyfs-test
  (:use jdt.core)
  (:require [clojure.test :refer :all]
            [jdt.easyfs :refer :all])
  (:import java.nio.file.NotDirectoryException))

;;; Apologies, this is only tested on linux, it clearly isn't going to work as-is for Windows systems.

;;; Figure out how to use fixtures to set up some special files for tesitng
;;; (setup/teardown infrastructure for this type of testing)

(deftest test-path-conversions
  (testing "Test x<->Path conversions"
    (is (path? (as-path "abc")))
    (is (path? (as-path (to-file (as-path "abc")))))
    (is (path? (as-path (as-path "abc"))))
    (is (path? (as-path (to-uri (as-path "abc")))))
    (is (path? (as-path '("a" "b" "c"))))
    (is (path? (as-path (seq [1 2 3]))))))

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
    (is (= 1 (count (children "~" {:glob "*.bashrc"}))))
    (is (= 1 (count (children "~" {:regex ".*\\.bashrc$"}))))
    ;; combining two options for filtering, filtering among the .b* files the hard way
    (is (= 1 (count (children "~" {:accept (fn [p] (.startsWith (str (.getFileName p)) ".b"))
                     :regex ".*rc$"}))))))

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
    (assert (not (some dir? (file-children "~"))))
    ;; match .bashrc the hard way, 3 of our own predicates basically, plus the implicit file? predicate
    (is (= 1 (count (file-children "~"
                                   {:accept (fn [p] (.startsWith (str (.getFileName p)) ".b"))
                                    :regex ".*ash.*"
                                    :glob "*rc"}))))
    ))

(deftest test-dir-children
  (testing "dir-children"
    (assert (not (some file? (dir-children "~"))))))

;; *TODO*: test these: owner, group, ctime, mtime, atime, file-key, size, file-store,
;; perm-keys, perm-string, content-type, read-bytes, read-lines, read-symlink,

(deftest test-directory
  (testing "directory creation/deletion"
    (let [tdir (create-temp-directory)]
      (is (exists? tdir))
      (is (dir? tdir))
      (let [cdir (as-path [tdir "cdir"])]
        (delete-if-exists cdir)
        (is (create-directory cdir))
        (is (exists? cdir))
        (is (dir? cdir))
        (is (delete-if-exists cdir)))
      (let [cdir (as-path [tdir "x" "y" "z"])]
        (delete-if-exists cdir)
        (is (create-directories cdir))
        (is (exists? cdir))
        (is (dir? cdir))
        ;; need to delete child dirs first
        (is (thrown? java.io.IOException (delete tdir)))
        ;; Drop /tmp and /tmp/<tdir>
        (doseq [p (reverse (drop 2 (resolve-component-paths cdir)))]
          (is (dir? p))
          (is (delete-if-exists p))
          (is (not (dir? p)))))
      (is (exists? tdir))
      (is (delete-if-exists tdir))
      (is (not (exists? tdir))))))

(deftest test-file
  (testing "file creation/deletion"
    (let [file (create-temp-file)]
      (is (file? file))
      (is (= 0 (size file)))
      (delete file)
      (is (not (delete-if-exists file))))
    (let [file (create-temp-file {:prefix "abc" :suffix "def" :parent "~" :permissions "rwx------"})]
      (is (file? file))
      (is (= (perm-string file) "rwx------"))
      (is (string-contains? (str file) "abc"))
      (is (string-contains? (str file) "def"))
      (is (not (string-contains? (file-name-string (first (seq file))) "tmp")))
      (is (= 0 (size file)))
      (is (delete-if-exists file)))))


(deftest test-directory-perms
  (testing "directory permissions"
    (let [tdir (create-temp-directory
                {:permissions {:owner-write false :group-write false :other-write false :owner-read true}})]
      (is (exists? tdir))
      (is (dir? tdir))
      (is (not (writable? tdir)))
      (is (= (perm-string tdir) "r--------"))
      (delete tdir))
    (let [tdir (create-temp-directory {:permissions "rwx---rwx"})]
      ;; Set<FilePermissions> is as expected, but resulting perms not as expected. 
      ;; Assuming umask is 0002, then :other-write is disabled
      (is (= (perm-string tdir) "rwx---r-x"))
      (delete tdir))
    (let [tdir (create-temp-directory {:permissions [:owner-read :owner-write]})]
      (is (= (perm-string tdir) "rw-------"))
      (delete tdir))
    ))
        
(deftest test-delete-directory
  (testing "delete-directory"
    (let [tdir (create-temp-directory)
          ndir (create-directories (as-path [tdir "x" "y" "z"]))
          file (create-temp-file {:parent ndir})]
      (delete-directory ndir)
      (delete-directory tdir))))

(deftest test-read-attributes
  (testing "read-attributes, attribute-map"
    (is (map? (attribute-map (read-attributes "/tmp"))))))

(deftest test-get-attribute
  (testing "get-attribute"
    (is (= (get-attribute "/tmp" "unix:uid") 0))))

(deftest test-set-attribute
  (testing "set-attribute"
    (let [file (create-temp-file)
          perms "rw-------"]
      (set-attribute file "posix:permissions"
                     (java.nio.file.attribute.PosixFilePermissions/fromString perms))
      (is (= (java.nio.file.attribute.PosixFilePermissions/toString
              (get-attribute file "posix:permissions"))
             perms))
      (delete file))))
