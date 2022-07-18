;;; link.el --- link or dotfiles into home directory -*- lexical-binding: t -*-

;;; Commentary:

;; Links all files into Home using symlinks, creates folders if
;; needed. Also stores a list of created files for later clearnup via
;; 'teardown'

;;; Code:

(add-to-list 'load-path "~/PersonalConfigs/emacs/scripts")
(require '+scripty)

(use-package projectile)
(require 'projectile)

(defvar link-teardown-file "~/elisp_teardown"
  "The location to store a list of created symlinks.
These can then be removed cleanly between runs.")

(defun link-setup ()
  "Do it."
  (link-teardown)
  (--> (link--get-all-linkable-files)
       (--map (s-replace "/PersonalConfigs" "" it) it)
       (reverse it) ; reverse to keep directories after files for cleanup later
       (+write-data link-teardown-file it)
       (-each it #'link--symlink-file))
  (message "link-setup complete"))

(defun link-teardown ()
  "Remove all previous made symlinks."
  (if (f-exists? link-teardown-file)
      (progn
	(--> (+read-data link-teardown-file)
	     (-each it #'link--remove-symlink))
	(f-delete link-teardown-file))
    (message "No teardown-file, not doing anything")))

;;; Helpers ----------------------------------------------------------------

(defun +write-data (filename data)
  "Write elisp data to FILENAME where DATA is any Lisp object."
  (with-temp-file filename
    (point-min)
    (insert ";;; -*- lisp-data -*-\n")
    (prin1 data (current-buffer))
    (point-min))
  data)

(defun +read-data (filename)
  "Read elisp data from FILENAME."
  (with-temp-buffer
    (insert-file-contents filename)
    (point-min)
    (read (current-buffer))))

(defun link--symlink-file (filename)
  "Attempt to symlink FILENAME if possible."
  (let ((source (link--path-in-configs filename)))
    (if (f-file? filename)
	(message "file %s already exists, remove manually" filename)
      (if (f-file? source)
	  (progn
	    (f-mkdir-full-path (f-dirname filename))
	    (f-symlink source filename)
	    (message "linked %s" filename))))))

(defun link--remove-symlink (filename)
  "Foo FILENAME."
  (cond ((and (f-dir? filename) (f-empty? filename)) (link--remove-and-notify filename))
	((f-dir? filename) nil)		; contents in folder so ignore
	((f-symlink? filename) (link--remove-and-notify filename))
	((f-exists? filename) (print "file exists! move yourself"))
	(t nil)))

(defun link--remove-and-notify (filename)
  "Remove FILENAME and log it."
  (f-delete filename)
  (message "link %s removed" filename))

(defun link--path-in-configs (filename)
  "Recieve a FILENAME in HOME and normalise it to contain dotfiles."
  (s-replace (getenv "HOME") (f-join (getenv "HOME") "PersonalConfigs") filename))

(defun link--get-all-linkable-files ()
  "Do it."
  (let ((dir "~/PersonalConfigs")
	(ignored-files '("^_" "README" ".gitignore$"))
	(ignored-dirs '("^_" "^.git$")))
    (-intersection
     (link--get-all-files dir ignored-files ignored-dirs)
     (link--get-absolute-project-paths))))

(defun link--get-absolute-project-paths ()
  "Get files in project with absolute paths."
  (--map (f-join (projectile-project-root) it)
	 (-concat
	  (projectile-project-dirs (projectile-project-root))
	  (projectile-project-files (projectile-project-root)))))

(defun link--get-all-files (&optional start ignored-files ignored-dirs)
  "Get all files recursively in START.
While ignoring IGNORED-FILES and not recursing through
IGNORED-DIRS."
  (let* ((dir (or start "~/PersonalConfigs"))
	 (files (f-files dir (-partial #'link--basename-not-contains ignored-files)))
	 (dirs (f-directories dir (-partial #'link--basename-not-contains ignored-dirs)))
	 (nested (--map (link--get-all-files it ignored-files ignored-dirs) dirs)))
    (-concat files (--map (s-append "/" it) dirs ) (-flatten nested))))

(defun link--basename-not-contains (ignored-files entry)
  "Check if the basename of ENTRY is included in IGNORED-FILES."
  (--> entry
       (f-base it)
       (link--not-contains ignored-files it)))

(defun link--contains (ignored e)
  "Check if any item in IGNORED list `s-matches?' against E."
  (--any? (s-matches? it e) ignored))

(defun link--not-contains (ignored e)
  "Check if no items in IGNORED list `s-matches?' against E."
  (not (link--contains ignored e)))

(provide 'link)

;;; link.el ends here
