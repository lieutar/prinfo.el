;; -*- lexical-binding: t -*-

(use-package slash-tmp
  :ensure nil
  :load-path "~/work/emacs/slash-tmp.el")

(dolist (file command-line-args-left)
  (load (expand-file-name file)))
;;(buttercup-run)
(ert-run-tests-batch)

(message "All tests completed.")
