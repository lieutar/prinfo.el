;; -*- lexical-binding: t -*-

(require 'prinfo)
(require 'slash-tmp)
(require 'ert)
(require 'f)
(require 's)

(setq $THIS-TEST (or load-file-name buffer-file-name))
(setq $THIS-DIR  (f-dirname $THIS-TEST))
(setq $RSC-DIR   (f-expand "rsc" $THIS-DIR))

(ert-deftest prinfo/git-test::0000 ()
  "Test `prinfo/git::current-branch-name'."
  (/tmp/with-temp-dir
    (sit-for 0) ;; Strangely this is required ?? Why the Heck ??
    (should (zerop (call-process "tar" nil nil nil
                                 "xzf" (f-expand "p00.tar.gz" $RSC-DIR))))
    (should (f-dir-p "p00"))
    (should (prinfo/git::is-git-dir-file-p "p00/submods/p01/submods/p02/.git"))
    (should (prinfo/git::is-git-dir-file-p "p00/submods/p01/.git"))
    (should-not (prinfo/git::is-git-dir-file-p "p00/.git"))
    (should (prinfo/git::is-git-dir-p "p00/.git"))

    (let ((p02-npl (prinfo/git::nested-project-list "p00/submods/p01/submods/p02"))
          (p01-br  (prinfo/git::current-branch-name "p00/submods/p01/submods"))
          (p00-lc  (prinfo/git::last-committed "p00/submods")))
      (should (string= (s-join "\n" p02-npl)
                       (s-join "\n" (--map (f-expand it)
                                           '("p00"
                                             "p00/submods/p01"
                                             "p00/submods/p01/submods/p02")))))
      (should (string= p01-br "main"))
      (should (string= (format "%S" p00-lc) "(2025 3 15 12 33 4)"))
      (should-not (prinfo/git::has-uncommitted-changes-p "p00"))
      (with-temp-file "p00/foo" (insert "foo!"))
      (should (prinfo/git::has-uncommitted-changes-p "p00"))
      )

    ;; Test on out of git repository
    (should-not (or
                 (prinfo/git::project-root ".")
                 (prinfo/git::has-uncommitted-changes-p ".")
                 (prinfo/git::current-branch-name ".")
                 (prinfo/git::last-committed ".")
                 ))
    )
  )

(ert-deftest prinfo/git-test::0010 ()
  "Test `prinfo/git::remote-url'"
  (/tmp/with-temp-dir
    (/tmp/weird-magic-spell)
    (should (zerop (call-process "tar" nil nil nil
                                 "xzf" (f-expand "bar.tar.gz" $RSC-DIR))))
    (should (f-dir-p "bar"))
    (should (string= (prinfo/git::remote-url "bar")
                     "/home/lieutar/work/emacs/prinfo.el/tests/rsc/foo"))
    )
  )
