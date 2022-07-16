;;; +bootstrap.el --- Get straight set up -*- no-byte-compile: t; lexical-binding: t -*-

;;; Commentary:
;; 
;; Package Manager setup --- inital setup of straight.el and use-package (plus complimentary packages)
;; 
;; Provided as a standalone package to allow scripts to run and load various packages like `dash'
;; 
;;; Code:

(require '+constants)

(setq straight-check-for-modifications nil) ; speeds up startup, but requires manual recompile, see 'emacs straight' notes

(defvar bootstrap-version)
(let ((bootstrap-file (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

(setq straight-use-package-by-default t)

(provide '+bootstrap)

;;; +bootstrap.el ends here
