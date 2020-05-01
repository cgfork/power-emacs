;;; power-emacs-utils.el --- Manage the external elisp packages and simpfy the .emacs fil -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:

(require 'package)
(require 'power-emacs-base)

(defcustom power-emacs-package-archives 'tuna
  "Set package archives from which to fetch the melpa packages."
  :type '(choice
	  (const :tag "Melpa" melpa)
	  (const :tag "Melpa Mirror" melpa-mirror)
	  (const :tag "Emacs-China" emacs-china)
	  (const :tag "Netease" netease)
	  (const :tag "Tuna" tuna))
  :group 'power-emacs)

;;;###autoload
(defun power-emacs-set-package-archives (archives)
  "Set specific package ARCHIVES repository for `package-install'."
  (interactive (list (intern (completing-read
			      "Choose package archives: "
			      '(melpa melpa-mirror emacs-china netease tuna)))))
  (setq package-archives
	(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
			    (and (or (fboundp 'gnutls-available-p)
				     (gnutls-available-p)))))
	       (proto (if no-ssl "http" "https")))
	  (pcase archives
	    ('melpa
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://melpa.org/packages/"))))
            ('melpa-mirror
             `(,(cons "gnu"   (concat proto "://elpa.gnu.org/packages/"))
               ,(cons "melpa" (concat proto "://www.mirrorservice.org/sites/melpa.org/packages/"))))
            ('emacs-china
             `(,(cons "gnu"   (concat proto "://elpa.emacs-china.org/gnu/"))
               ,(cons "melpa" (concat proto "://elpa.emacs-china.org/melpa/"))))
            ('netease
             `(,(cons "gnu"   (concat proto "://mirrors.163.com/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.163.com/elpa/melpa/"))))
            ('tuna
             `(,(cons "gnu"   (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/gnu/"))
               ,(cons "melpa" (concat proto "://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))
            (archives
             (error "Unknown archives: '%s'" archives)))))
  (message "Set package archives to '%s'" archives))

;;;###autoload
(defun power-emacs-apply-themes ()
  "Forcibly load the thems listed in the `custom-enabled-themes'."
  (dolist (theme custom-enabled-themes)
    (unless (custom-theme-p theme)
      (load-theme theme))
    (custom-set-variables `(custom-enabled-themes (quote ,custom-enabled-themes)))))

;;;###autoload
(defun power-emacs-rename-this-file-and-buffer (name)
  "Rename both current buffer and file to the NAME."
  (interactive (list (read-string (format "Rename %s to: " (buffer-name)))))
  (let ((bname (buffer-name))
	(fname (buffer-file-name)))
    (unless fname
      (error "Buffer '%s' is not a visiting file!" bname))
    (progn
      (when (file-exists-p fname)
	(rename-file fname name 1))
      (set-visited-file-name name)
      (rename-buffer name))))

;;;###autoload
(defun power-emacs-delete-this-file ()
  "Delete the current file and kill the current buffer."
  (interactive)
  (let ((file-name (buffer-file-name))
	(buf-name (current-buffer)))
    (unless file-name
      (error "No file is currently being edited!"))
    (when (yes-or-no-p (format "Are you sure to delete '%s'?"
			       (file-name-nondirectory file-name)))
      (delete-file file-name)
      (kill-buffer buf-name))))

;;;###autoload
(defun power-emacs-open-in-browse ()
  "Open the current file using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
	     (tramp-tramp-file-p file-name))
	    (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

;;;###autoload
(defun power-emacs-open-recentf ()
  "Open the recent file."
  (interactive)
  (when (featurep 'recentf)
    (let ((file (completing-read "Open: " recentf-list)))
      (if (file-exists-p file)
	  (find-file file)))))

(provide 'power-emacs-utils)

;;; power-emacs-utils.el ends here
