;; -*- lexical-binding: t -*-
;;; prinfo.el --- get information from repositories

;; Copyright (C) 2025  lieutar <lieutar@gmail.com>

;; Author:
;; Version: 0.0.0
;; Keywords:
;; URL:
;; Package-Requires: ((emacs "29.3")(dash)(s)(f))

;;; License:

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;

;;; Code:
(provide 'prinfo)

(require 'dash)
(require 's)
(require 'f)

;;;###autoload
(defsubst prinfo/git::is-git-dir-p (path)
  "Return non nil value when PATH is a valid git directory."
  (and (f-dir-p path)
       (f-readable-p path)
       (let ((head (f-expand "HEAD" path)))
         (and (f-file-p head)
              (f-readable-p head)))))

;;;###autoload
(defsubst prinfo/git::is-git-dir-file-p (path)
  "Return non nil value when PATH is a valid .git file."
  (and (f-file-p    path)
       (f-readable-p path)
       (let ((match
              (s-match "\\`gitdir:[[:space:]]*\\([^\r\n]+\\)" (f-read path))))
         (and match
              (prinfo/git::is-git-dir-p (f-expand (nth 1 match)
                                                  (f-parent path)))))))

;;;###autoload
(defsubst prinfo/git::is-valid-dot-git-p (path)
  "Return non nil value when PATH is a valid .git file or directory."
  (and (s-match "\\(\\`\\|[/\\\\]\\)\\.git\\'"  path)
       (or (prinfo/git::is-git-dir-p path)
           (prinfo/git::is-git-dir-file-p path))))

;;;###autoload
(defun prinfo/git::get-info (path)
  "Return information of the repository contains given PATH.
The return value will be a plist having following properties.

:type 'git (FIXED)
:path (string) -  given PATH value.
:project-list (list<string>) - paths of project root directory.
        If the project contains given PATH is a submodule, this value contains
        multiple values.
        This list starts with root-project and last is same as :project-contextual.
:project-contextual (string) - project-root contains given PATH.
:shorten-project-path (list<string>) - base-name list of the :project-list
:has-uncommitted-changes (boolean) -
:branch (string) -
:last-commited (list<integer>) -
"
  (unless (f-dir-p path) (setq path (f-dirname path)))
  (let ((np (prinfo/git::nested-project-list path)))
    (when np
      (let ((context (car (reverse np))))
        (list :type 'git
              :path path
              :project-list np
              :shorten-project-path (-map #'f-filename np)
              :project-contextual context
              :has-uncommitted-changes (prinfo/git::has-uncommitted-changes-p
                                       context)
              :branch (prinfo/git::current-branch-name context)
              :last-commited (prinfo/git::last-committed context))))))

;;;###autoload
(defun prinfo/git::nested-project-list (path)
  ""
  (unless (f-dir-p path) (setq path (f-dirname path)))
  (letrec ((recurse (lambda (path)
                      (if (f-dir-p (f-expand ".git" path))
                          (list path)
                        (cons path (funcall recurse (prinfo/git::project-root
                                                     (f-parent path))))))))
    (let ((pr (prinfo/git::project-root (f-expand path))))
      (and pr (reverse (funcall recurse pr))))))

;;;###autoload
(defun prinfo/git::project-root (path)
  "Return root directory of the project contains PATH."
  (unless (f-dir-p path) (setq path (f-dirname path)))
  (unless (f-root-p path)
    (let ((dot-git (f-expand ".git" path)))
      (if (prinfo/git::is-valid-dot-git-p dot-git)
          path
        (prinfo/git::project-root (f-parent path))))))

(defmacro prinfo/git::with-LANG-C (&rest body)
  "Execute BODY with LANG environment variable set to C,
then restore the original LANG."
  `(let ((old-LANG (getenv "LANG")))
     (unwind-protect
         (progn
           (setenv "LANG" "C")
           ,@body)
       (setenv "LANG" old-LANG))))

;;;###autoload
(defun prinfo/git::current-branch-name (path)
  "Get the current branch name of the Git repository at PATH."
  (unless (f-dir-p path) (setq path (f-dirname path)))
  (prinfo/git::with-LANG-C
   (let ((default-directory (f-expand path)))
     (with-temp-buffer
       (when (zerop (call-process "git" nil t nil "branch"))
         (goto-char (point-min))
         (when (re-search-forward "^\* \\(.*\\)$" nil t)
           (match-string 1)))))))

;;;###autoload
(defun prinfo/git::has-uncommitted-changes-p (path)
  "Returns non-nil when the repository has uncommitted changes"
  (unless (f-dir-p path) (setq path (f-dirname path)))
  (prinfo/git::with-LANG-C
   (let ((default-directory (f-expand path)))
     (with-temp-buffer
       (and
        (zerop (call-process "git" nil t nil "status"))
        (progn
          (goto-char (point-min))
          (not
           (re-search-forward  "^nothing to commit, working tree clean" nil t))
          ))))))

;;;###autoload
(defun prinfo/git::last-committed (path)
  ""
  (unless (f-dir-p path) (setq path (f-dirname path)))
  (let* ((default-directory (f-expand path))
         (result (prinfo/git::with-LANG-C
                  (with-temp-buffer
                    (when (zerop (call-process
                                  "git" nil t nil
                                  "log" "-1" "--format=%cd"
                                  "--date" "format:%Y-%m-%d-%H-%M-%S"))
                      (s-chomp (buffer-substring-no-properties (point-min)
                                                               (point-max))))))))
    (when result
      (-map #'string-to-number
            (s-split
             "-"
             result)))))

;;;###autoload
(defun prinfo/git::remote-url (path &optional name)
  ""
  (unless (f-dir-p path) (setq path (f-dirname path)))
  (setq name (or name "origin"))
  (let* ((default-directory (f-expand path))
         (result (prinfo/git::with-LANG-C
                  (with-temp-buffer
                    (when (zerop (call-process
                                  "git" nil t nil
                                  "remote" "get-url" name)))
                    (s-chomp (buffer-substring-no-properties
                              (point-min)(point-max)))))))
    result))

;;(prinfo/git::remote-url "~/.emacs.d/straight/repos/f.el")


;;; prinfo.el ends here
