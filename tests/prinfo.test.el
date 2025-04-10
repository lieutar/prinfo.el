;; -*- lexical-binding: t -*-

(require 'prinfo)
(require 'slash-tmp)
(require 'ert)
(require 'f)
(require 's)

(setq $THIS-TEST (or load-file-name buffer-file-name))
(setq $THIS-DIR  (f-dirname $THIS-TEST))
(setq $RSC-DIR   (f-expand "rsc" $THIS-DIR))

(ert-deftest prinfo-test::0000 ()
  "Test `prinfo/git::current-branch-name'."
  (/tmp/with-temp-dir
    (sit-for 0) ;; Strangely this is required ?? Why the Heck ??
    (should (zerop (call-process "tar" nil nil nil
                                 "xzf" (f-expand "p00.tar.gz" $RSC-DIR))))
    (should (f-dir-p "p00"))

    (let ((p02-npl (prinfo-get-nested-project-list "p00/submods/p01/submods/p02"
                                                   :root-dir ".")))
      ;;(message "p02-npl: %S" p02-npl)
      (should p02-npl)
      (should (= 3 (length p02-npl)))
      ;;(message ">>> %S" (s-join " " (-map #'car p02-npl)))
      (should (string= (s-join " " (-map #'f-expand
                                         '("p00/"
                                           "p00/submods/p01/"
                                           "p00/submods/p01/submods/p02/")))
                       (s-join " " (-map #'car p02-npl))))
      )
    )
  )
