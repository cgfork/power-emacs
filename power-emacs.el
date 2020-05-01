;;; power-emacs.el --- Enhance the Emacs and provide some useful functions -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; URL: https://github.com/cgfork/power-emacs
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:
;;; Code:

(require 'power-emacs-build)

(defcustom power-emacs-banner-file (expand-file-name "banner" power-emacs-base-dir)
  "Banner file.")

(defcustom power-emacs-melpa-dir (expand-file-name "melpa" user-emacs-directory)
  "Directory to clone the 'melpa' repository."
  :group 'power-emacs
  :type 'string)

(defcustom power-emacs-melpa-repo-url "https://github.com/melpa/melpa.git"
  "The repository for checkouting the melpa sources."
  :group 'power-emacs
  :type 'string)

(defcustom power-emacs-recipe-archives
  `(("melpa" . ,(expand-file-name "recipes" power-emacs-melpa-dir))
    ("power-emacs" . ,(expand-file-name "recipes" power-emacs-base-dir)))
  "Recipe stores where `power-emacs' finds the recipes for packages."
  :group 'power-emacs
  :type 'list)

(defcustom power-emacs-autoremove-enable nil
  "Remove the obsoleted packages automatically if non-nil."
  :group 'power-emacs
  :type 'boolean)

(defvar power-emacs-recipe-alist nil
  "Cache the recipe contents stored in the `power-emacs-recipe-archives'.")

(defun power-emacs--update-recipe-alist (archive rcp)
  "Update `power-emacs-recipe-alist' with the given ARCHIVE and RCP object."
  (when (and (stringp archive)
	     (power-emacs-recipe-p rcp))
    (let* ((name (power-emacs-recipe-name rcp))
	   (exist (assq name power-emacs-recipe-alist)))
      (when exist
	(setq power-emacs-recipe-alist
	      (assq-delete-all name power-emacs-recipe-alist)))
      (setq power-emacs-recipe-alist
	    (cons (list name archive rcp)
		  power-emacs-recipe-alist)))))

(defun power-emacs--read-recipe-alist (archive)
  "Read the recipe contents stored in the ARCHIVE."
  (let ((archive-name (car archive))
	(dir (cdr archive)))
    (when (file-directory-p dir)
      (dolist (file (directory-files dir))
	(unless (string-prefix-p "." file)
	  (when-let ((rcp (power-emacs-build--recipe-lookup file dir)))
	    (power-emacs--update-recipe-alist archive-name rcp)))))))

(defun power-emacs--read-recipe-archives ()
  "Read the recipe contents from `power-emacs-recipe-archives'."
  (dolist (archive power-emacs-recipe-archives)
    (when archive
      (power-emacs--read-recipe-alist archive))))

(defun power-emacs--recipe-lookup (pkg)
  "Return a recipe object for the PKG. PKG is a symbol.
It will resolve the recipe info in the `power-emacs-recipe-contents'.
If no such recipe exists, then raise an error."
  (when-let ((entry (assq pkg power-emacs-recipe-alist)))
    (caddr entry)))

(defun power-emacs--package-recipe (pkg)
  "Parse the given recipe or pakcage PKG.
Return the recipe object if it resolved, otherwise raise an error."
  (pcase pkg
    (`(,a . nil) (power-emacs--recipe-lookup a))
    (`(,a . ,b) (apply #'power-emacs-build--recipe-create a b))
    ((pred symbolp) (power-emacs--recipe-lookup pkg))))

(defun power-emacs--delete-obsoleted-package (name)
  "Delete obsoleted packages with name NAME."
  (mapc (lambda (pkg-desc)
          (with-demoted-errors "Error deleting package: %S"
            (let ((inhibit-message t))
              (package-delete pkg-desc))))
        (cddr (assoc name package-alist))))

(defun power-emacs--package-desc->archive-entry (pkg-desc)
  "Transform the `package-desc' object PKG-DESC to a (archive . archive-entry) pair."
  (when (and pkg-desc (package-desc-p pkg-desc))
    (cons (package-desc-name pkg-desc)
	  (vector (package-desc-version pkg-desc)
		  (package-desc-reqs pkg-desc)
		  (package-desc-summary pkg-desc)
		  (package-desc-kind pkg-desc)
		  (package-desc-extras pkg-desc)))))

(defun power-emacs--archive-entry->package-desc (archive-entry archive)
  "Transform the ARCHIVE-ENTRY vector to `package-desc' object."
  (let* ((name (car archive-entry))
	  (pkg-info (cdr archive-entry))
	  (version (aref pkg-info 0))
	  (requires (aref pkg-info 1))
	  (desc (aref pkg-info 2))
	  (kind (aref pkg-info 3))
	  (extras (aref pkg-info 4)))
    (package-desc-create :name name
			 :version version
			 :summary desc
			 :reqs requires
			 :kind kind
			 :archive archive
			 :dir "builtin"
			 :extras extras
			 :signed nil)))

(defun power-emacs--archive-file (pkg-desc)
  "Return the archive file from the PKG-DESC."
  (when (package-desc-p pkg-desc)
    (let ((name (package-desc-name pkg-desc))
	  (version (package-version-join (package-desc-version pkg-desc)))
	  (kind (package-desc-kind pkg-desc)))
      (expand-file-name
       (format "%s-%s.%s" name version (if (eq kind 'single) "el" "tar"))
       power-emacs-build-archive-dir))))

(defun power-emacs--package-type (file)
  "Determine the package type of FILE.
Return `tar' for tarball packages, `single' for single file
packages, or nil, if FILE is not a package."
  (let ((ext (file-name-extension file)))
    (cond
     ((string= ext "tar") 'tar)
     ((string= ext "el") 'single)
     (:else nil))))

(defun power-emacs--get-package-desc (file)
  "Extract and return the PACKAGE-DESC object from FILE.
On error return nil."
  (let* ((kind (power-emacs--package-type file))
         (desc (with-demoted-errors "Error getting PACKAGE-DESC: %s"
                 (with-temp-buffer
                   (pcase kind
                     (`single (insert-file-contents file)
                              (package-buffer-info))
                     (`tar (insert-file-contents-literally file)
                           (tar-mode)
                           (with-no-warnings
                             (package-tar-file-info))))))))
    (when (package-desc-p desc)
      desc)))

(defvar power-emacs--already-setup nil)

;;;###autoload
(defun power-emacs-setup ()
  "Intialize the directories, checkout melpa repo and load all packages.
PLIST provides additional process before installing package."
  (interactive nil)
  (dolist (dir (list power-emacs-base-dir power-emacs-build-working-dir power-emacs-build-archive-dir))
    (unless (file-exists-p dir)
      (progn
	(power-emacs-build--message "Create directory: %s" dir)
	(make-directory dir t))))
  (unless power-emacs--already-setup
    (power-emacs-build--message "Setup POWER-EMACS")
    (power-emacs--read-recipe-archives)
    (unless (bound-and-true-p package--initialized)
      (package-initialize))
    (setq power-emacs--already-setup t)))

;;;###autoload
(defun power-emacs-refresh-recipe-alist ()
  "Reload the `power-emacs-recipe-alist' from the `power-emacs-recipe-archives'."
  (setq power-emacs-recipe-alist nil)
  (power-emacs--read-recipe-archives))

;;;###autoload
(defun power-emacs-package-build (rcp)
  "Checkout and build package specified by RCP object.
PLIST is a plist that provides additional config
for building the package. Return a `package-desc' object
if it builds successfully, otherwise nil."
  (when-let ((archive-entry (power-emacs-build rcp)))
    (let ((entry (assq (power-emacs-recipe-name rcp) power-emacs-recipe-alist)))
      (if entry
	  (power-emacs--archive-entry->package-desc archive-entry
					    (cadr entry))
	(power-emacs--get-package-desc (power-emacs-build--artifact-file archive-entry))))))

;;;###autoload
(defun power-emacs-package-install (pkg &rest plist)
  "Checkout, build and install a package named PKG. Resolve the recipe from 
the STORE. PLIST is addtional options for building and installing. Return 
the `package-desc' object if it installed successfully."
  (unless power-emacs--already-setup
    (error "Must setup before `power-emacs-package-install'."))
  (let ((power-emacs-autoremove-enable power-emacs-autoremove-enable) ;; shadow
	(power-emacs-build-stable power-emacs-build-stable)) ;; shadow
    (while plist
      (let ((key (car plist))
	    (val (cadr plist)))
	(pcase key
	  (:autoremove (setq power-emacs-autoremove-enable val))
	  (:stable (setq power-emacs-build-stable val))))
      (setq plist (cddr plist))) 
    (let* ((rcp (power-emacs--package-recipe pkg))
	   (pkg-desc (and rcp (power-emacs-package-build rcp))))
      (when pkg-desc
	(when-let ((requires (package-desc-reqs pkg-desc)))
	  (mapc (lambda (r)
		  (unless (or (equal 'emacs (car r))
			      (package-installed-p (car r) (cadr r)))
		    (power-emacs-package-install (car r))))
		requires))
	(when-let ((file (power-emacs--archive-file pkg-desc)))
	  (package-install-file file))
	(when power-emacs-autoremove-enable
	  (power-emacs--delete-obsoleted-package (power-emacs-recipe-name rcp)))
	pkg-desc))))

;;;###autoload
(defun power-emacs-install (pkg &rest plist)
  "Install the PKG if it was not installed. Return t if the PKG has already
installed via `power-emacs'. Install the PKG if it non-installed, otherwise raise
an error. "
  (interactive (list nil))
  (power-emacs-setup)
  (let ((min-version (plist-get plist :min-version))
	(pkg-name (if (symbolp pkg)
		      pkg
		    (car pkg))))
    (unless (or (package-installed-p pkg-name min-version)
		(apply #'power-emacs-package-install pkg plist))
      (error "Failed to install `%s'" pkg-name))))

;;;###autoload
(defun power-emacs-try (pkg &rest plist)
  "Try to install the PKG if it was not installed. Return non-nil if the PKG
installed successfully, otherwise return nil."
  (condition-case err
      (progn
	(apply #'power-emacs-install pkg plist)
	t)
    (error
     (message "%s" (error-message-string err))
     nil)))

;;;###autoload
(defun power-emacs ()
  "Return the banner of power emacs."
  (with-temp-buffer
    (insert-file-contents-literally power-emacs-banner-file)
    (buffer-string)))

(provide 'power-emacs)
;;; power-emacs.el ends here
