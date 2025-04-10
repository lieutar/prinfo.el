;; -*- lexical-binding: t -*-
;;; prinfo.el --- get information from repositories

;; Copyright (C) 2025  lieutar <lieutar@gmail.com>

;; Author:
;; Version: 0.1.0
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
(defun prinfo-get-nested-project-list (path &rest options)
  ""
  (unless (f-dir-p path) (setq path (f-dirname path)))
  (let ((type          (plist-get options :type))
        (root-dir      (plist-get options :root-dir))
        (force-to-root (plist-get options :force-to-root)))
    (letrec ((recurse
              (lambda (pinfo)
                (let ((path   (car pinfo))
                      (palist (cdr pinfo)))
                  (if (and (not force-to-root)
                           (--find (plist-get (cdr it) :root) palist))
                      (list pinfo)
                    (cons pinfo
                          (funcall recurse
                                   (prinfo-project-root
                                    (f-parent path)
                                    :type type
                                    :root-dir root-dir))))))))
      (let ((pinfo (prinfo-project-root path
                                        (f-expand path)
                                        :type type
                                        :root-dir root-dir)))
        (and pinfo (reverse (funcall recurse pinfo)))))))

(defconst prinfo::$vcs-detector-functions ())
;;;###autoload
(defun prinfo-project-root (path &rest options)
  "Return root directory of the project contains PATH."
  (let ((cpath (lambda (path)
                 (let ((sep (f-path-separator)))
                   (s-replace-regexp
                    (format "\\([^%s]\\)\\'" (regexp-quote sep))
                    (concat "\\1" sep)
                    (f-expand path))))))
    (setq path (funcall cpath (if (f-dir-p path) path (f-dirname path))))
    (let ((type     (plist-get options :type))
          (root-dir (funcall cpath (or (plist-get options :root-dir) "/"))))
      (unless (string= path root-dir)
        (let ((detected (->>  prinfo::$vcs-detector-functions
                              (--map (funcall it path))
                              (--filter (and it
                                             (or (not type)
                                                 (eq type (car it))))))))
          (if detected
              (cons path detected)
            (prinfo-project-root (f-parent path))))))))


;;;; prinfo/git

(defun prinfo/git::detect-git (path)
  (let ((dot-git (f-expand ".git" path)))
    (cond ((prinfo/git::is-git-dir-p dot-git)
           (list 'git :root t))
          ((prinfo/git::is-git-dir-file-p dot-git)
           (list 'git :root nil)))))
(add-hook 'prinfo::$vcs-detector-functions #'prinfo/git::detect-git)

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
(defun prinfo/git::nested-project-list (path &rest options)
  ""
  (let ((result (prinfo-get-nested-project-list
                 path
                 :type 'git
                 :root-dir (plist-get options :root-dir)
                 :force-to-root (plist-get options :force-to-root))))
    (-map #'car result)))

;;;###autoload
(defun prinfo/git::project-root (path &rest options)
  "Return root directory of the project contains PATH."
  (car (prinfo-project-root path
                            :type 'git
                            :root-dir (plist-get options :root-dir))))

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
