;;; power-emacs-build.el --- Manage the external elisp packages and simpfy the .emacs file  -*- lexical-binding: t -*-

;; Copyright (C) 2020-2029, cgfork

;; Author: cgfork
;; Version: 0.0.1
;; Package-Requires: ((emacs "25.3"))

;;; Commentary:

;; A package for building the emacs package based on
;; `package-build'.

;;; Code:

(require 'cl-lib)
(require 'cl-generic)
(require 'package)
(require 'json)
(require 'lisp-mnt)
(require 'subr-x)
(require 'power-emacs-base)

(defcustom power-emacs-base-dir (expand-file-name "power-emacs/" user-emacs-directory)
  "Directory in which to checkout, build and archive packages."
  :group 'power-emacs
  :type 'string)

(defcustom power-emacs-build-working-dir (expand-file-name "build/" power-emacs-base-dir)
  "Directory in whitch to checkout the package source."
  :group 'power-emacs
  :type 'string)

(defcustom power-emacs-build-archive-dir (expand-file-name "packages/" power-emacs-base-dir)
  "Directory in which to keep compiled archives"
  :group 'power-emacs
  :type 'string)

(defcustom power-emacs-build-verbose t
  "When non-nil, then print additional progress information."
  :group 'power-emacs
  :type 'boolean)

(defcustom power-emacs-build-stable t
  "When non-nil, then try to build packages from versions-tagged code."
  :group 'power-emacs
  :type 'boolean)

(defcustom power-emacs-build-timeout-executable "timeout"
  "Path to a GNU coreutils \"timeout\" command if available.
This must be a version which supports the \"-k\" option.

On MacOS it is possible to install coreutils using Homebrew or
similar, which will provide the GNU timeout program as
\"gtimeout\"."
  :group 'power-emacs
  :type '(file :must-match t))

(defcustom power-emacs-build-timeout-secs nil
  "Wait this many seconds for external processes to complete.

If an external process takes longer than specified here to
complete, then it is terminated.  If nil, then no time limit is
applied.  This setting requires
`power-emacs-build-timeout-executable' to be set."
  :group 'power-emacs
  :type 'number)

(defcustom power-emacs-build-tar-executable "tar"
  "Path to a (preferably GNU) tar command.
Certain package names (e.g. \"@\") may not work properly with a BSD tar.

On MacOS it is possible to install coreutils using Homebrew or
similar, which will provide the GNU timeout program as
\"gtar\"."
  :group 'power-emacs
  :type '(file :must-match t))

(defcustom power-emacs-build-write-melpa-badge-images nil
  "When non-nil, write MELPA badge images alongside packages.
These batches can, for example, be used on GitHub pages."
  :group 'power-emacs
  :type 'boolean)

(defcustom power-emacs-build-version-regexp "^[rRvV]?\\(.*\\)$"
  "Default pattern for matching valid version-strings within repository tags.
The string in the capture group should be parsed as valid by `version-to-list'."
  :group 'power-emacs
  :type 'string)

(defun power-emacs-build--message (format-string &rest args)
  "Behave like `message' if `power-emacs-build-verbose' is non-nil."
  (when power-emacs-build-verbose
    (apply #'message format-string args)))

;;; Version Handling

;; (power-emacs-build--parse-time "19911015 13:11:11" ) => "19911015.1311"
(defun power-emacs-build--parse-time (str &optional regexp)
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

;; (power-emacs-build--find-version-newest '("20200405.2310" "20200405.2311")) => ("20200405.2311" . "20200405.2311")
(defun power-emacs-build--find-version-newest (tags &optional regexp)
  "Find the newest version in TAGS matching REGEXP.
If optional REGEXP is nil, then `power-emacs-build-version-regexp'
is used instead."
  (let ((ret '(nil 0)))
    (dolist (tag tags)
      (string-match (or regexp power-emacs-build-version-regexp) tag)
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
      (string-match (or regexp power-emacs-build-version-regexp) tag)
      (let* ((version-separator "_")
             (version (ignore-errors (version-to-list (match-string 1 tag)))))
        (when (and version (version-list-<= (cdr ret) version))
          (setq ret (cons tag version)))))
    (and (car ret)
         (cons (car ret)
               (package-version-join (cdr ret))))))

;;; Run Process

(defun power-emacs-build--run-process (directory destination command &rest args)
  (with-current-buffer
      (if (eq destination t)
          (current-buffer)
        (or destination (get-buffer-create "*power-emacs-build-checkout*")))
    (let ((default-directory
            (file-name-as-directory (or directory default-directory)))
          (argv (nconc (unless (eq system-type 'windows-nt)
                         (list "env" "LC_ALL=C"))
                       (if (and power-emacs-build-timeout-secs power-emacs-build-timeout-executable)
                           (nconc (list power-emacs-build-timeout-executable
                                        "-k" "60" (number-to-string
                                                   power-emacs-build-timeout-secs)
                                        command)
                                  args)
                         (cons command args)))))
      (unless (file-directory-p default-directory)
        (error "Can't run process in non-existent directory: %s" default-directory))
      (let ((exit-code (apply 'process-file
                              (car argv) nil (current-buffer) t
                              (cdr argv))))
        (or (zerop exit-code)
            (error "Command '%s' exited with non-zero status %d: %s"
                   argv exit-code (buffer-string)))))))

(defun power-emacs-build--run-process-match (regexp directory command &rest args)
  (with-temp-buffer
    (apply 'power-emacs-build--run-process directory t command args)
    (goto-char (point-min))
    (re-search-forward regexp)
    (match-string-no-properties 1)))

(defun power-emacs-build--process-lines (directory command &rest args)
  (with-temp-buffer
    (apply 'power-emacs-build--run-process directory t command args)
    (split-string (buffer-string) "\n" t)))

;;; Package Recipes

(cl-defgeneric power-emacs-build--checkout (rcp)
  "Defines a generic function to checkout the sources from the RCP.")

(cl-defgeneric power-emacs-build--upstream-url (rcp)
  "Returns the actual url of the RCP.")

(cl-defstruct power-emacs-recipe
  "Defines a melpa compatible recipe.

The standard melpa recipe plist form:

(<package-name>
 :fetcher [git|github|gitlab|hg|bitbucket]
 [:url \"<repo-url>\"]
 [:commit \"<commit>\"]
 [:branch \"<branch>\"]
 [:version-regexp \"<regexp>\"]
 [:files (\"<file1>\" ...)])

The description of recipe keywords:

package-name is a list symbol that has the same name as the package being specified.

:fetcher specifies the type of repository that :url or :repo points to.

:url specifies the URL of the version control repository. required for git or hg.

:repo specifies the github/gitlab/bitbucket repository and is of the form user/repo-name.

:commit specifies the commit of the git repo to checkout.

:branch specifies the branch of the git repo to use.

:version-regexp is a regular expression for extracing a version-string from the repository tags.

:files optional property specifying the elisp and info files used to build package.
"
  name fetcher url repo commit branch version-regexp files old-names)

(cl-defstruct (power-emacs-git-recipe
	       (:include power-emacs-recipe
			 (fetcher 'git)))
  (tag-regexp "\
\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \
[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\( [+-][0-9]\\{4\\}\\)?\\)"))

(cl-defstruct (power-emacs-hg-recipe
	       (:include power-emacs-recipe
			 (fetcher 'hg)))
  (tag-regexp "\
\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \
[0-9]\\{2\\}:[0-9]\\{2\\}\\( [+-][0-9]\\{4\\}\\)?\\)"))


(cl-defstruct (power-emacs-github-recipe
	       (:include power-emacs-git-recipe
			 (fetcher 'github)))
  (url-format "https://github.com/%s.git"))

(cl-defstruct (power-emacs-gitlab-recipe
	       (:include power-emacs-git-recipe
			 (fetcher 'gitlab)))
  (url-format "https://gitlib.com/%s.git"))

(cl-defstruct (power-emacs-bitbucket-recipe
	       (:include power-emacs-hg-recipe
			 (fetcher 'bitbucket)))
  (url-format "https://bitbucket.org/%s"))

(cl-defmethod power-emacs-build--recipe-list ((rcp power-emacs-recipe))
  "Return a list of the given RCP without fields whose value is nil."
  `(,(power-emacs-recipe-name rcp)
    ,@(when-let ((fetcher (power-emacs-recipe-fetcher rcp)))
	(list :fetcher fetcher))
    ,@(when-let ((url (power-emacs-recipe-url rcp)))
	(list :url url))
    ,@(when-let ((repo (power-emacs-recipe-repo rcp)))
	(list :repo repo))
    ,@(when-let ((commit (power-emacs-recipe-commit rcp)))
	(list :commit commit))
    ,@(when-let ((branch (power-emacs-recipe-branch rcp)))
	(list :branch branch))
    ,@(when-let ((version-regexp (power-emacs-recipe-version-regexp rcp)))
	(list :version-regexp version-regexp))
    ,@(when-let ((files (power-emacs-recipe-files rcp)))
	(list :files files))
    ,@(when-let ((old-names (power-emacs-recipe-old-names rcp)))
	(list :old-names old-names))))

(defun power-emacs-build--recipe-validate (name recipe)
  "Perform some basic checks on the recipe PLIST for the package NAME."
  (pcase-let ((`(,ident . ,plist) recipe))
    (cl-assert ident)
    (cl-assert (symbolp ident))
    (cl-assert (string= (symbol-name ident) name)
	       nil "Recipe '%s' contains mismatched package name '%s'"
	       name ident)
    (cl-assert plist)
    (let* ((symbol-keys '(:fetcher))
	   (string-keys '(:url :repo :commit :branch :version-regexp))
	   (list-keys '(:files :old-names))
	   (all-keys (append symbol-keys string-keys list-keys)))
      (dolist (thing plist)
	(when (keywordp thing)
	  (cl-assert (memq thing all-keys) nil "Unkown keyword %S" thing)))
      (let ((fetcher (plist-get plist :fetcher)))
	(cl-assert fetcher nil ":fetcher is missing")
	(if (memq fetcher '(github gitlab bitbucket))
	    (progn
	      (cl-assert (plist-get plist :repo) ":repo is missing")
	      (cl-assert (not (plist-get plist :url)) ":url is redundant"))
	  (cl-assert (plist-get plist :url) ":url is missing")))
      (dolist (key symbol-keys)
	(let ((val (plist-get plist key)))
	  (when val
	    (cl-assert (symbolp val) nil "%s must be a symbol but is %S" key val))))
      (dolist (key list-keys)
	(let ((val (plist-get plist key)))
	  (when val
	    (cl-assert (listp val) nil "%s must be a list but is %S" key val))))
      (dolist (key string-keys)
	(let ((val (plist-get plist key)))
	  (when val
	    (cl-assert (stringp val) nil "%s must be a string but is %S" key val)))))
    recipe))

(defun power-emacs-build--recipe-create (name &rest plist)
  "Make recipe object with the given NAME and PLIST.
The NAME must be a symbol and the fetcher must be provided."
  (cl-assert name)
  (cl-assert (symbolp name))
  (if-let ((fetcher (plist-get plist :fetcher)))
      (apply (intern (format "make-power-emacs-%s-recipe" fetcher))
	     :name name plist)
    (error "A :fetcher is necessary.")))

(defun power-emacs-build--recipe-lookup (name-str dir)
  "Lookup recipe with NAME from DIR. 
Return the power-emacs-recipe if it exists, otherwise nil."
  (let* ((file (expand-file-name name-str dir)))
    (when (file-exists-p file)
      (let* ((rcp (with-temp-buffer
		    (insert-file-contents-literally file)
		    (read (current-buffer))))
	     (pkg-name (car rcp))
	     (plist (cdr rcp))
	     (fetcher (plist-get plist :fetcher)))
	(power-emacs-build--recipe-validate name-str rcp)
	(apply (intern (format "make-power-emacs-%s-recipe" fetcher))
		 :name pkg-name plist)))))

(cl-defmethod power-emacs-build--package-name ((rcp power-emacs-recipe))
  (let ((name (power-emacs-recipe-name rcp)))
    (cond
     ((stringp name) name)
     ((symbolp name) (symbol-name name))
     (t (error "Package name is not a string or a symbol.")))))

(cl-defmethod power-emacs-build--working-dir ((rcp power-emacs-recipe))
  (file-name-as-directory
     (expand-file-name (power-emacs-build--package-name rcp) power-emacs-build-working-dir)))

(cl-defmethod power-emacs-build--upstream-url ((rcp power-emacs-git-recipe))
  (power-emacs-recipe-url rcp))

(cl-defmethod power-emacs-build--upstream-url ((rcp power-emacs-hg-recipe))
  (power-emacs-recipe-url rcp))

(cl-defmethod power-emacs-build--upstream-url ((rcp power-emacs-github-recipe))
  (or (power-emacs-github-recipe-url rcp)
      (format (power-emacs-github-recipe-url-format rcp) (power-emacs-github-recipe-repo rcp))))

(cl-defmethod power-emacs-build--upstream-url ((rcp power-emacs-gitlab-recipe))
  (or (power-emacs-gitlab-recipe-url rcp)
      (format (power-emacs-gitlab-recipe-url-format rcp) (power-emacs-gitlab-recipe-repo rcp))))

(cl-defmethod power-emacs-build--upstream-url ((rcp power-emacs-bitbucket-recipe))
  (or (power-emacs-bitbucket-recipe-url rcp)
      (format (power-emacs-bitbucket-recipe-url-format rcp) (power-emacs-bitbucket-recipe-repo rcp))))

(cl-defmethod power-emacs-build--checkout :before ((rcp power-emacs-recipe))
  (power-emacs-build--message "Package: %s" (power-emacs-build--package-name rcp))
  (power-emacs-build--message "Fetcher: %s" (power-emacs-recipe-fetcher rcp))
  (power-emacs-build--message "Checkout: %s" (power-emacs-build--upstream-url rcp)))

;;; Git

(cl-defmethod power-emacs-build--checkout ((rcp power-emacs-git-recipe))
  (let ((dir (power-emacs-build--working-dir rcp))
	(url (power-emacs-build--upstream-url rcp)))
    (cond
     ;; Check whether the git repo exists.
     ((and (file-exists-p (expand-file-name ".git" dir))
	   (string-equal (power-emacs-build--used-url rcp) url))
      (power-emacs-build--message "Updating %s" dir)
      (power-emacs-build--run-process dir nil "git" "fetch" "-f" "--all" "--tags"))
     (t
      ;; Delete the existing file firstly. 
      (when (file-exists-p dir)
	(delete-directory dir t))
      (power-emacs-build--message "Cloning %s to %s" url dir)
      (power-emacs-build--run-process nil nil "git" "clone" url dir)))
    (if power-emacs-build-stable
	(cl-destructuring-bind (tag . version)
	    (or (power-emacs-build--find-version-newest
		 (let ((default-directory (power-emacs-build--working-dir rcp)))
		   (process-lines "git" "tag"))
		 (power-emacs-recipe-version-regexp rcp))
		(error "No valid stable versions found for %s" (power-emacs-build--package-name rcp)))
	  (power-emacs-build--checkout-stable rcp (concat "tags/" tag))
	  version)
      (power-emacs-build--checkout-stable rcp)
      (power-emacs-build--parse-time
       (car (apply #'power-emacs-build--process-lines dir
		   "git" "log" "--first-parent" "-n1" "--pretty=format:'\%ci'"
		   (power-emacs-build--expand-source-file-list rcp)))
       (power-emacs-git-recipe-tag-regexp rcp)))))

(cl-defmethod power-emacs-build--checkout-stable ((rcp power-emacs-git-recipe) &optional rev)
  (let ((dir (power-emacs-build--working-dir rcp)))
    (unless rev
      (setq rev (or (power-emacs-recipe-commit rcp)
		    (concat "origin/"
			    (or (power-emacs-recipe-branch rcp)
				(ignore-errors
				  (power-emacs-build--run-process-match
				   "HEAD branch: \\(.*\\)" dir
				   "git" "remote" "show" "origin"))
				"master")))))
    (power-emacs-build--run-process dir nil "git" "reset" "--hard" rev)
    (power-emacs-build--run-process dir nil "git" "submodule" "sync" "--recursive")
    (power-emacs-build--run-process dir nil "git" "submodule" "update" "--init" "--recursive")))

(cl-defmethod power-emacs-build--used-url ((rcp power-emacs-git-recipe))
  (let ((default-directory (power-emacs-build--working-dir rcp)))
    (car (process-lines "git" "config" "remote.origin.url"))))

;; Hg

(cl-defmethod power-emacs-build--checkout ((rcp power-emacs-hg-recipe))
  (let ((dir (power-emacs-build--working-dir rcp))
        (url (power-emacs-build--upstream-url rcp)))
    (cond
     ((and (file-exists-p (expand-file-name ".hg" dir))
           (string-equal (power-emacs-build--used-url rcp) url))
      (power-emacs-build--message "Updating %s" dir)
      (power-emacs-build--run-process dir nil "hg" "pull")
      (power-emacs-build--run-process dir nil "hg" "update"))
     (t
      (when (file-exists-p dir)
        (delete-directory dir t))
      (power-emacs-build--message "Cloning %s to %s" url dir)
      (power-emacs-build--run-process nil nil "hg" "clone" url dir)))
    (if power-emacs-build-stable
        (cl-destructuring-bind (tag . version)
            (or (power-emacs-build--find-version-newest
                 (mapcar (lambda (line)
                           ;; Remove space and rev that follow ref.
                           (string-match "\\`[^ ]+" line)
                           (match-string 0))
                         (process-lines "hg" "tags"))
                 (power-emacs-recipe-version-regexp rcp))
                (error "No valid stable versions found for %s" (power-emacs-build--package-name rcp)))
          (power-emacs-build--run-process dir nil "hg" "update" tag)
          version)
      (power-emacs-build--parse-time
       (car (apply #'power-emacs-build--process-lines dir
                   "hg" "log" "--style" "compact" "-l1"
                   (power-emacs-build--expand-source-file-list rcp)))
       (power-emacs-hg-recipe-tag-regexp rcp)))))

(cl-defmethod power-emacs-build--used-url ((rcp power-emacs-hg-recipe))
  (power-emacs-build--run-process-match "default = \\(.*\\)"
                                    (power-emacs-build--working-dir rcp)
                                    "hg" "paths"))

;;; Various Files

(defun power-emacs-build--write-pkg-file (pkg-file pkg-info)
  "Write PKG-FILE containing PKG-INFO."
  (with-temp-file pkg-file
    (pp
     `(define-package
	,(aref pkg-info 0)
	,(aref pkg-info 3)
	,(aref pkg-info 2)
	',(mapcar
	   (lambda (elt)
	     (list (car elt)
		   (package-version-join (cadr elt))))
	   (aref pkg-info 1))
	;; Append our extra information
	,@(cl-mapcan (lambda (entry)
		       (let ((value (cdr entry)))
			 (when (or (symbolp value) (listp value))
			   (setq value (list 'quote value)))
			 (list (car entry) value)))
		     (when (> (length pkg-info) 4)
		       (aref pkg-info 4))))
     (current-buffer))
    (princ ";; Local Variables:\n;; no-byte-compile: t\n;; End:\n"
	   (current-buffer))))

(defun power-emacs-build--create-tar (file dir &optional files)
  "Create a tar FILE containing the contents of DIR, or just FILES if non-nil"
  (when (eq system-type 'window-nt)
    (setq file (replace-regexp-in-string "^\\([a-z]\\):" "/\\1" file)))
  (apply 'process-file
	 power-emacs-build-tar-executable nil
	 (get-buffer-create "*power-emacs-build-checkout*")
	 nil "-cvf"
	 file
	 "--exclude=.git"
	 "--exclude=.hg"
	 (or (mapcar (lambda (fn) (concat dir "/" fn)) files) (list dir))))

(defun power-emacs-build--find-package-commentary (file-path)
  "Get commentary section from FILE-PATH."
  (when (file-exists-p file-path)
    (with-temp-buffer
      (insert-file-contents file-path)
      (lm-commentary))))

(defun power-emacs-build--write-pkg-readme (target-dir commentary file-name)
  "In TARGET_DIR, write COMMENTARY to a -readme.txt file prefixed with FILE-NAME."
  (when commentary
    (with-temp-buffer
      (insert commentary)
      ;; Adapted from `describe-package-1'
      (goto-char (point-min))
      (save-excursion
	(when (re-search-forward "^;;; Commentary:\n" nil t)
	  (replace-match ""))
	(while (re-search-forward "^\\(;+ ?\\)" nil t)
	  (replace-match ""))
	(goto-char (point-min))
	(when (re-search-forward "\\ `\\( *\n\\)+'" nil t)
	  (replace-match "")))
      (delete-trailing-whitespace)
      (let ((coding-system-for-write buffer-file-coding-system))
	(write-region nil nil
		      (expand-file-name (concat file-name "-readme.txt")
					target-dir))))))

;;; Entries

(defun power-emacs-build--update-or-insert-version (version)
  "Ensure current buffer has a \"Package-Version: VERSION\" header."
  (goto-char (point-min))
  (if (let ((case-fold-search t))
	(re-search-forward "^;+* *Package-Version *: *" nil t))
      (progn
	(move-beginning-of-line nil)
	(search-forward "V" nil t)
	(backward-char)
	(insert "X-Original-")
	(move-beginning-of-line nil))
    (re-search-forward "^;+* *\\(Version\\|Package-Requires\\|Keywords\\|URL\\) *:"
		       nil t)
    (forward-line))
  (insert (format ";; Package-Version: %s" version))
  (newline))

(defun power-emacs-build--ensure-ends-here-line (file-path)
  "Add a 'FILE-PATH ends here' trailing line if missing."
  (save-excursion
    (goto-char (point-min))
    (let ((trailer (concat ";;; "
			   (file-name-nondirectory file-path)
			   " ends here")))
      (unless (search-forward trailer nil t)
	(goto-char (point-max))
	(newline)
	(insert trailer)
	(newline)))))

(defun power-emacs-build--get-package-info (file-path)
  "Get a vector of package info from the docstring in FILE-PATH."
  (when (file-exists-p file-path)
    (ignore-errors
      (with-temp-buffer
	(insert-file-contents file-path)
	;; next few lines are a hack for some packages that aren't
	;; commented properly.
	(power-emacs-build--update-or-insert-version "0")
	(power-emacs-build--ensure-ends-here-line file-path)
	(cl-flet ((package-strip-rcs-id (_) "0"))
	  (power-emacs-build--package-buffer-info-vec))))))

(defun power-emacs-build--package-buffer-info-vec ()
  "Return a vector of package info.
`package-buffer-info' returns a vector in older Emacs versions,
and a cl struct in Emacs HEAD. This wrapper normalises the results."
  (let ((desc (package-buffer-info))
	(keywords (lm-keywords-list)))
    (if (fboundp 'package-desc-create)
	(let ((extras (package-desc-extras desc)))
	  (when (and keywords (not (assq :keywords extras)))
	    (push (cons :keywords keywords) extras))
	  (vector (package-desc-name desc)
		  (package-desc-reqs desc)
		  (package-desc-summary desc)
		  (package-desc-version desc)
		  extras))
      (let ((homepage (power-emacs-build--lm-homepage))
	    extras)
	(when keywords (push (cons :keywords keywords) extras))
	(when homepage (push (cons :url homepage) extras))
	(vector (aref desc 0)
		(aref desc 1)
		(aref desc 2)
		(aref desc 3)
		extras)))))

(defun power-emacs-build--get-pkg-file-info (file-path)
  "Get a vector of package info from \"-pkg.el\" file FILE-PATH."
  (when (file-exists-p file-path)
    (let ((package-def (with-temp-buffer
                         (insert-file-contents file-path)
                         (read (current-buffer)))))
      (if (eq 'define-package (car package-def))
          (let* ((pkgfile-info (cdr package-def))
                 (descr (nth 2 pkgfile-info))
                 (rest-plist (cl-subseq pkgfile-info (min 4 (length pkgfile-info))))
                 (extras (let (alist)
                           (while rest-plist
                             (unless (memq (car rest-plist) '(:kind :archive))
                               (let ((value (cadr rest-plist)))
                                 (when value
                                   (push (cons (car rest-plist)
                                               (if (eq (car-safe value) 'quote)
                                                   (cadr value)
                                                 value))
                                         alist))))
                             (setq rest-plist (cddr rest-plist)))
                           alist)))
            (when (string-match "[\r\n]" descr)
              (error "Illegal multi-line package description in %s" file-path))
            (vector
             (nth 0 pkgfile-info)
             (mapcar
              (lambda (elt)
                (unless (symbolp (car elt))
                  (error "Invalid package name in dependency: %S" (car elt)))
                (list (car elt) (version-to-list (cadr elt))))
              (eval (nth 3 pkgfile-info)))
             descr
             (nth 1 pkgfile-info)
             extras))
        (error "No define-package found in %s" file-path)))))

(defun power-emacs-build--merge-package-info (pkg-info name version)
  "Return a version of PKG-INFO updated with NAME, VERSION and info from CONFIG.
If PKG-INFO is nil, an empty one is created."
  (let ((merged (or (copy-sequence pkg-info)
                    (vector name nil "No description available." version))))
    (aset merged 0 name)
    (aset merged 3 version)
    merged))

(defun power-emacs-build--write-archive-entry (rcp pkg-info type)
  (let ((entry (power-emacs-build--archive-entry rcp pkg-info type)))
    (with-temp-file (power-emacs-build--archive-entry-file entry)
      (print entry (current-buffer)))))

(cl-defmethod power-emacs-build--get-commit ((rcp power-emacs-git-recipe))
  (ignore-errors
    (power-emacs-build--run-process-match
     "\\(.*\\)"
     (power-emacs-build--working-dir rcp)
     "git" "rev-parse" "HEAD")))

(cl-defmethod power-emacs-build--get-commit ((rcp power-emacs-hg-recipe))
  (ignore-errors
    (power-emacs-build--run-process-match
     "changeset:[[:space:]]+[[:digit:]]+:\\([[:xdigit:]]+\\)"
     (power-emacs-build--working-dir rcp)
     "hg" "log" "--debug" "--limit=1")))

(defun power-emacs-build--archive-entry (rcp pkg-info type)
  (let ((name (intern (aref pkg-info 0)))
        (requires (aref pkg-info 1))
        (desc (or (aref pkg-info 2) "No description available."))
        (version (aref pkg-info 3))
        (extras (and (> (length pkg-info) 4)
                     (aref pkg-info 4)))
        (commit (power-emacs-build--get-commit rcp)))
    (when commit
      (push (cons :commit commit) extras))
    (cons name
          (vector (version-to-list version)
                  requires
                  desc
                  type
                  extras))))

(defun power-emacs-build--artifact-file (archive-entry)
  "Return the path of the file in which the package for ARCHIVE-ENTRY is stored."
  (let* ((name (car archive-entry))
         (pkg-info (cdr archive-entry))
         (version (package-version-join (aref pkg-info 0)))
         (flavour (aref pkg-info 3)))
    (expand-file-name
     (format "%s-%s.%s" name version (if (eq flavour 'single) "el" "tar"))
     power-emacs-build-archive-dir)))

(defun power-emacs-build--archive-entry-file (archive-entry)
  "Return the path of the file in which the package for ARCHIVE-ENTRY is stored."
  (let* ((name (car archive-entry))
         (pkg-info (cdr archive-entry))
         (version (package-version-join (aref pkg-info 0))))
    (expand-file-name
     (format "%s-%s.entry" name version)
     power-emacs-build-archive-dir)))

;;; File Specs

(defconst power-emacs-build-default-files-spec
  '("*.el" "*.el.in" "dir"
    "*.info" "*.texi" "*.texinfo"
    "doc/dir" "doc/*.info" "doc/*.texi" "doc/*.texinfo"
    (:exclude ".dir-locals.el" "test.el" "tests.el" "*-test.el" "*-tests.el"))
  "Default value for :files attribute in recipes.")


(defun power-emacs-build-expand-file-specs (dir specs &optional subdir allow-empty)
  "In DIR, expand SPECS, optionally under SUBDIR.
The result is a list of (SOURCE . DEST), where SOURCE is a source
file path and DEST is the relative path to which it should be copied.

If the resulting list is empty, an error will be reported.  Pass t
for ALLOW-EMPTY to prevent this error."
  (let ((default-directory dir)
        (prefix (if subdir (format "%s/" subdir) ""))
        (lst))
    (dolist (entry specs lst)
      (setq lst
            (if (consp entry)
                (if (eq :exclude (car entry))
                    (cl-nset-difference lst
                                        (power-emacs-build-expand-file-specs
                                         dir (cdr entry) nil t)
                                        :key 'car
                                        :test 'equal)
                  (nconc lst
                         (power-emacs-build-expand-file-specs
                          dir
                          (cdr entry)
                          (concat prefix (car entry))
                          t)))
              (nconc
               lst (mapcar (lambda (f)
                             ; (let ((destname)))
                             (cons f
                                   (concat prefix
                                           (replace-regexp-in-string
                                            "\\.el\\.in\\'"
                                            ".el"
                                            (file-name-nondirectory f)))))
                           (file-expand-wildcards entry))))))
    (when (and (null lst) (not allow-empty))
      (error "No matching file(s) found in %s: %s" dir specs))
    lst))

(defun power-emacs-build--config-file-list (rcp)
  (let ((file-list (power-emacs-recipe-files rcp)))
    (cond
     ((null file-list)
      power-emacs-build-default-files-spec)
     ((eq :defaults (car file-list))
      (append power-emacs-build-default-files-spec (cdr file-list)))
     (t
      file-list))))

(defun power-emacs-build--expand-source-file-list (rcp)
  (mapcar 'car
          (power-emacs-build-expand-file-specs
           (power-emacs-build--working-dir rcp)
           (power-emacs-build--config-file-list rcp))))

;;; Info Manuals

(defun power-emacs-build--generate-info-files (files source-dir target-dir)
  "Create .info files from any .texi files listed in FILES.

The source and destination file paths are expanded in SOURCE-DIR
and TARGET-DIR respectively.

Any of the original .texi(nfo) files found in TARGET-DIR are
deleted."
  (dolist (spec files)
    (let* ((source-file (car spec))
           (source-path (expand-file-name source-file source-dir))
           (dest-file (cdr spec))
           (info-path (expand-file-name
                       (concat (file-name-sans-extension dest-file) ".info")
                       target-dir)))
      (when (string-match ".texi\\(nfo\\)?$" source-file)
        (unless (file-exists-p info-path)
          (ignore-errors
            (power-emacs-build--run-process
             (file-name-directory source-path) nil
             "makeinfo" source-path "-o" info-path)
            (power-emacs-build--message "Created %s" info-path)))
        (power-emacs-build--message "Removing %s"
                                (expand-file-name dest-file target-dir))
        (delete-file (expand-file-name dest-file target-dir))))))

(defun power-emacs-build--generate-dir-file (files target-dir)
  "Create dir file from any .info files listed in FILES in TARGET-DIR."
  (dolist (spec files)
    (let* ((source-file (car spec))
           (dest-file (cdr spec))
           (info-path (expand-file-name
                       (concat (file-name-sans-extension dest-file) ".info")
                       target-dir)))
      (when (and (or (string-match ".info$" source-file)
                     (string-match ".texi\\(nfo\\)?$" source-file))
                 (file-exists-p info-path))
        (ignore-errors
          (power-emacs-build--run-process
           nil nil
           "install-info"
           (concat "--dir=" (expand-file-name "dir" target-dir))
           info-path))))))


;;; Building Utilities

(defun power-emacs-build--copy-package-files (files source-dir target-dir)
  "Copy FILES from SOURCE-DIR to TARGET-DIR.
FILES is a list of (SOURCE . DEST) relative filepath pairs."
  (power-emacs-build--message
   "Copying files (->) and directories (=>)\n  from %s\n  to %s"
   source-dir target-dir)
  (dolist (elt files)
    (let* ((src  (car elt))
           (dst  (cdr elt))
           (src* (expand-file-name src source-dir))
           (dst* (expand-file-name dst target-dir)))
      (make-directory (file-name-directory dst*) t)
      (cond ((file-regular-p src*)
             (power-emacs-build--message
              "  %s %s -> %s" (if (equal src dst) " " "!") src dst)
             (copy-file src* dst*))
            ((file-directory-p src*)
             (power-emacs-build--message
              "  %s %s => %s" (if (equal src dst) " " "!") src dst)
             (copy-directory src* dst*))))))

(defconst power-emacs-build--this-file load-file-name)

;;; Building

(defun power-emacs-build--build-multi-file-package (rcp version files source-dir)
  (let* ((name (power-emacs-build--package-name rcp))
	 (tmp-dir (file-name-as-directory (make-temp-file name t))))
    (unwind-protect
	(let* ((pkg-dir-name (concat name "-" version))
	       (pkg-tmp-dir (expand-file-name pkg-dir-name tmp-dir))
	       (pkg-file (concat name "-pkg.el"))
	       (pkg-file-source (or (car (rassoc pkg-file files))
				    pkg-file))
	       (file-source (concat name ".el"))
	       (pkg-source (or (car (rassoc file-source files))
			       file-source))
	       (pkg-info (power-emacs-build--merge-package-info
			  (let ((default-directory source-dir))
			    (or (power-emacs-build--get-pkg-file-info pkg-file-source)
				(power-emacs-build--get-pkg-file-info
				 (expand-file-name (concat pkg-file ".in")
						   (file-name-directory pkg-source)))
				(power-emacs-build--get-package-info pkg-source)))
			  name version)))
	  (power-emacs-build--copy-package-files files source-dir pkg-tmp-dir)
	  (power-emacs-build--write-pkg-file (expand-file-name
				      pkg-file
				      (file-name-as-directory pkg-tmp-dir))
				     pkg-info)
	  (power-emacs-build--generate-info-files files source-dir pkg-tmp-dir)
	  (power-emacs-build--generate-dir-file files pkg-tmp-dir)

	  (let ((default-directory tmp-dir))
	    (power-emacs-build--create-tar
	     (expand-file-name (concat name "-" version ".tar")
			       power-emacs-build-archive-dir)
	     pkg-dir-name))

	  (let ((default-directory source-dir))
	    (power-emacs-build--write-pkg-readme
	     power-emacs-build-archive-dir
	     (power-emacs-build--find-package-commentary pkg-source)
	     name))
	  (power-emacs-build--write-archive-entry rcp pkg-info 'tar))
      (delete-directory tmp-dir t nil))))

(defun power-emacs-build--build-single-file-package (rcp version file source-dir)
  (let* ((name (power-emacs-build--package-name rcp))
	 (pkg-source (expand-file-name file source-dir))
	 (pkg-target (expand-file-name
		      (concat name "-" version ".el")
		      power-emacs-build-archive-dir))
	 (pkg-info (power-emacs-build--merge-package-info
		    (power-emacs-build--get-package-info pkg-source)
		    name version)))
    (unless (string-equal (downcase (concat name ".el"))
			  (downcase (file-name-nondirectory pkg-source)))
      (error "Single file %s does not match package name %s"
	     (file-name-nondirectory pkg-source) name))
    (copy-file pkg-source pkg-target t)
    (let ((enable-local-variables nil)
	  (make-backup-files nil))
      (with-current-buffer (find-file pkg-target)
	(power-emacs-build--update-or-insert-version version)
	(power-emacs-build--ensure-ends-here-line pkg-source)
	(write-file pkg-target nil)
	(condition-case err
	    (power-emacs-build--package-buffer-info-vec)
	  (error
	   (power-emacs-build--message "Warning: %S" err)))
	(kill-buffer)))
    (power-emacs-build--write-pkg-readme
     power-emacs-build-archive-dir
     (power-emacs-build--find-package-commentary pkg-source)
     name)
    (power-emacs-build--write-archive-entry rcp pkg-info 'single)))

;;;###autoload
(defun power-emacs-build-package (rcp version)
    "Create version VERSION of the package specified by RCP.
Return the archive entry for the package and store the package
in `power-emacs-build-archive-dir'."
    (let* ((source-dir (power-emacs-build--working-dir rcp))
	   (file-specs (power-emacs-build--config-file-list rcp))
	   (files (power-emacs-build-expand-file-specs source-dir file-specs))
	   (name (power-emacs-build--package-name rcp)))
      (unless (equal file-specs power-emacs-build-default-files-spec)
	(when (equal files (power-emacs-build-expand-file-specs
			    source-dir power-emacs-build-default-files-spec nil t))
	  (power-emacs-build--message
	   "Note: %s :files spec is equivalent to the default." name)))
      (cond
       ((not version)
	(error "Unable to check out repository for %s" name))
       ((= 1 (length files))
	(power-emacs-build--build-single-file-package
	 rcp version (caar files) source-dir))
       ((< 1 (length files))
	(power-emacs-build--build-multi-file-package
	 rcp version files source-dir))
       (t (error "Unable to find files matching recipe patterns")))))

;;;###autoload
(defun power-emacs-build (rcp)
  "Checkout and build a package from the given recipe object RCP.
Return the archive entry or nil if no action is necessary."
  (when-let ((version (power-emacs-build--checkout rcp)))
    (power-emacs-build-package rcp version)))

;;; Backports

(defun power-emacs-build--lm-homepage (&optional file)
  "Return the homepage in file FILE, or current buffer if FILE is nil.
This is a copy of `lm-homepage', which first appeared in Emacs 24.4."
  (let ((page (lm-with-file file
                (lm-header "\\(?:x-\\)?\\(?:homepage\\|url\\)"))))
    (if (and page (string-match "^<.+>$" page))
        (substring page 1 -1)
      page)))


(provide 'power-emacs-build)

;;; power-emacs-build.el ends here
