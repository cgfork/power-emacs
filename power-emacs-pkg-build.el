;;; power-emacs-pkg-build.el --- Package build -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:

(require 'power-emacs-base)

(defcustom power-emacs-pkg-build-archive-dir (expand-file-name "packages/" user-emacs-directory)
  "Directory in which to keep compiled archives"
  :group 'power-emacs
  :type 'string)

(defcustom power-emacs-pkg-build-verbose t
  "When non-nil, then print additional progress information."
  :group 'power-emacs
  :type 'boolean)

(defcustom power-emacs-pkg-build-tar-executable "tar"
  "Path to a (preferably GNU) tar command.
Certain package names (e.g. \"@\") may not work properly with a BSD tar.

On MacOS it is possible to install coreutils using Homebrew or
similar, which will provide the GNU timeout program as
\"gtar\"."
  :group 'power-emacs
  :type '(file :must-match t))

(defcustom power-emacs-pkg-build-version-regexp "^[rRvV]?\\(.*\\)$"
  "Default pattern for matching valid version-strings within repository tags.
The string in the capture group should be parsed as valid by `version-to-list'."
  :group 'power-emacs
  :type 'string)


(defun power-emacs-pkg-build--message (fmt-str &rest args)
  "Behave like `message' if `power-emacs-pkg-build-verbose' is non-nil."
  (when power-emacs-pkg-build-verbose
    (apply #'message fmt-str args)))


;; (power-emacs-pkg-build--parse-time "19911015 13:11:11") => "19911015.1311
(defun power-emacs-pkg-build--parse-time (str &optional regexp)
  "Parse STR as a time, and format as a YYYYMMDD.HHMM string.
Always use Coordinated Universal Time (UTC) for output string.
If REGEXP is provided, it is applied to STR and the function
parses the first match group instead of STR."
  (unless str
    (error "No valid timestamp found"))
  (setq str (substring-no-properties str))
  (when regexp
    (if (string-match regexp str)
        (setq str (match-string 1 str))
      (error "No valid timestamp found")))
  ;; We remove zero-padding the HH portion, as it is lost
  ;; when stored in the archive-contents
  (let ((time (date-to-time
               (if (string-match "\
^\\([0-9]\\{4\\}\\)/\\([0-9]\\{2\\}\\)/\\([0-9]\\{2\\}\\) \
\\([0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\)$" str)
                   (concat (match-string 1 str) "-" (match-string 2 str) "-"
                           (match-string 3 str) " " (match-string 4 str))
                 str))))
    (concat (format-time-string "%Y%m%d." time t)
            (format "%d" (string-to-number (format-time-string "%H%M" time t))))))

;; (power-emacs-pkg-build--find-version-newest '("20200405.2310" "20200405.2311")) => ("20200405.2311" . "20200405.2311")
(defun power-emacs-pkg-build--find-version-newest (tags &optional regexp)
  "Find the newest version in TAGS matching REGEXP.
If optional REGEXP is nil, then `power-emacs-build-version-regexp'
is used instead."
  (let ((ret '(nil 0)))
    (dolist (tag tags)
      (string-match (or regexp power-emacs-pkg-build-version-regexp) tag)
      (let ((version (ignore-errors (version-to-list (match-string 1 tag)))))
        (when (and version (version-list-<= (cdr ret) version))
          (setq ret (cons tag version))))
      ;; Some version tags use "_" as version separator instead of
      ;; the default ".", e.g. "1_4_5".  Check for valid versions
      ;; again, this time using "_" as a `version-separator'.
      ;; Since "_" is otherwise treated as a snapshot separator by
      ;; `version-regexp-alist', we don't have to worry about the
      ;; incorrect version list above `(1 -4 4 -4 5)' since it will
      ;; always be treated as smaller by `version-list-<'.
      (string-match (or regexp power-emacs-pkg-build-version-regexp) tag)
      (let* ((version-separator "_")
             (version (ignore-errors (version-to-list (match-string 1 tag)))))
        (when (and version (version-list-<= (cdr ret) version))
          (setq ret (cons tag version)))))
    (and (car ret)
         (cons (car ret)
               (package-version-join (cdr ret))))))

(provide 'power-emacs-pkg-build)
;;; power-emacs-pkg-build.el ends here
