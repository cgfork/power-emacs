;;; power-emacs-base.el --- Manage the external elisp packages and simpfy the .emacs fil -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:

;;; Code:

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

(defcustom power-emacs-shell-executable (getenv "SHELL")
  "Where the environment variables get from."
  :type 'string
  :group 'power-emacs)

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
      (let ((exit-code (apply #'call-process shell-exec nil t nil shell-args)))
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
			   (message "get %s variable error: %s, skip it" name (error-message-string err)))))
	      (if value
		  (progn (power-emacs-set-shell-variable name value)
			 (cons name value))
		(cons name nil))))
	  vars))

(provide 'power-emacs-base)
;;; power-emacs-base.el ends here
