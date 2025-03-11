;; -*- lexical-binding: t -*-
;;; prinfo.el ---

;; Copyright (C) 2025  lieutar <lieutar@gmail.com>

;; Autor:
;; Version: 0.0.0
;; Keywords:
;; URL:

;;; License:

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:
(provide 'prinfo)

(require 'project)
(require 'dash)
(require 's)
(require 'f)


(defun prinfo/git::get-info (path)
  (unless (f-dir-p path) (setq path (f-dirname path)))
  (let ((np (prinfo/git::nested-project-list path)))
    (when np
      (let ((context (car np)))
        (list :path path
              :project-list np
              :shorten-project-path (-map #'f-filename (reverse np))
              :project-contextual context
              :has-uncommited-changes (prinfo/git::has-uncommited-changes-p
                                       context)
              :branch (prinfo/git::current-branch-name context))))))

(defun prinfo/git::nested-project-list (path)
  ""
  (unless (f-dir-p path) (setq path (f-dirname path)))
  (let ((r-result nil))
    (letrec ((recurse (lambda (path)
                        (let ((parent (f-parent path)))
                          (unless (f-root? parent)
                            (let ((dot-git (f-expand ".git" path)))
                              (if (f-exists-p dot-git)
                                  (progn
                                    (setq r-result (cons path r-result))
                                    (unless (f-dir-p dot-git)
                                      (funcall recurse parent)))
                                (funcall recurse parent))))))))
      (funcall recurse path))
    (reverse r-result)))

(defun prinfo/git::project-root (path)
  ""
  (car (prinfo/git::nested-project-list path)))


(defun prinfo/git::has-uncommited-changes-p (path)
  ""
  (unless (f-dir-p path) (setq path (f-dirname path)))
  (not (s-match "^nothing to commit, working tree clean"
                (shell-command-to-string
                 (format
                  "cd %s;LANG=C git status"
                  path
                  )))))


(defun prinfo/git::current-branch-name (path)
  "Get the current branch name of the Git repository at PATH."
  (unless (f-dir-p path) (setq path (f-dirname path)))
  (let ((default-directory path))
    (with-temp-buffer
      (when (zerop (call-process "git" nil "*hoge*" nil "branch"))
        (goto-char (point-min))
        (when (re-search-forward "^\* \\(.*\\)$" nil t)
          (match-string 1))))))
;;(prinfo/git::current-branch-name buffer-file-name)


(defun my/get-current-git-branch ()
  "Get the current Git branch name."
  (when (vc-backend buffer-file-name)
    (let ((branch (vc-git-working-revision)))
      (if branch
          (message "Current branch: %s" branch)
        (message "Not on a branch")))))



(defun prinfo/git::last-commited (path)
  (unless (f-dir-p path) (setq path (f-dirname path)))
  (let ((default-directory path))
    (-map #'string-to-number
          (s-split
           "-"
           (shell-command-to-string
            "git log -1 --format='%cd' --date=format:'%Y-%m-%d-%H-%M-%S'")))))


;;(prinfo/git::last-commited "~/work/emacs/winx.el/")
