;;; power-emacs.el --- Enhance the Emacs and provide some useful functions -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1

;;; Commentary:
;;; Code:

;; Define consts
(defconst sys/win32p
  (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp
  (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp
  (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-x-p
  (and (display-graphic-p) sys/macp)
  "Are we running under X on a Mac system?")

(defconst sys/linux-x-p
  (and (display-graphic-p) sys/linuxp)
  "Are we running under X on a GNU/Linux system?")

(defconst sys/cygwinp
  (eq system-type 'cygwin)
  "Are we running on a Cygwin system?")

(defconst sys/rootp
  (string-equal "root" (getenv "USER"))
  "Are you using ROOT user?")

(defconst emacs/>=25p
  (>= emacs-major-version 25)
  "Emacs is 25 or above.")

(defconst emacs/>=26p
  (>= emacs-major-version 26)
  "Emacs is 26 or above.")

(defconst emacs/>=27p
  (>= emacs-major-version 27)
  "Emacs is 27 or above.")

(defconst emacs/>=25.2p
  (or emacs/>=26p
      (and (= emacs-major-version 25) (>= emacs-minor-version 2)))
  "Emacs is 25.2 or above.")

(defgroup power-emacs nil
  "Define the group for the power-emacs package."
  :group 'convenience)

(defcustom power-emacs-package-archives 'tuna
  "Set package archives from which to fetch the melpa packages."
  :type '(choice
	  (const :tag "Melpa" melpa)
	  (const :tag "Melpa Mirror" melpa-mirror)
	  (const :tag "Emacs-China" emacs-china)
	  (const :tag "Netease" netease)
	  (const :tag "Tuna" tuna))
  :group 'power-emacs)

(defcustom power-emacs-shell-executable (getenv "SHELL")
  "Where the environment variables get from."
  :type 'string
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
(defun power-emacs-get-shell-variable (name &optional shell)
  "Return the value associated with the NAME in the default shell environments 
or in the SHELL environments if it exists."
  (let* ((shell (or shell power-emacs-shell-executable))
	 (shell-exec (executable-find shell))
	 (name (cond
		((stringp name) name)
		((symbolp name) (symbol-name name))
		(t (error "Unknown name %S" name))))
	 (printf (or (executable-find "printf") "printf"))
	 (printf-command (concat printf " '%s' " (format "${%s}" name)))
	 (shell-args (cond
		      ((string-match-p "t?csh$" shell)
		       `("-d" "-c" ,(concat "sh -c" printf-command)))
		      ((string-match-p "fish" shell)
		       `("-l" "-c" ,(concat "sh -c" printf-command)))
		      (t
		       `("-l" "-i" "-c" ,printf-command)))))
    (with-temp-buffer
      (let ((exit-code (apply #'call-process shell nil t nil shell-args)))
	(unless (zerop exit-code)
	  (error "Non-zero exit code when execute `%s' with '%S'" shell shell-args)))
      (buffer-string))))

;;;###autoload
(defun power-emacs-set-shell-variable (name value)
  "Set the value of environment variable NAME to VALUE.
If NAME is 'PATH', it will also set corresponding variables
such as `exec-path', `eshell-path-env' and so on."
  (setenv name value)
  (when (and (string-equal "PATH" name)
	     (not (string-empty-p value)))
    (setq eshell-path-env value
	  exec-path (append (parse-colon-path value) (list exec-directory)))))

;;;###autoload
(defun power-emacs-copy-shell-variables (shell &rest vars)
  "Set the environment VARS from the given SHELL.
It will returns the pairs that are set into the environment variables."
  (mapcar (lambda (name)
	    (let ((value (condition-case err
			     (power-emacs-get-shell-variable name shell)
			   (message "get %s variable error, skip it" name))))
	      (if value
		  (progn (power-emacs-set-shell-variable name value)
			 (cons name value))
		(cons name nil))))
	  vars))

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

(provide 'power-emacs)
;;; power-emacs.el ends here
