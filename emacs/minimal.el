;;; init.el --- emacs config for learning the holy editor -*- no-byte-compile: t; lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Package Manager setup --- inital setup of straight.el and use-package (plus complimentary packages)

;; speeds up startup, but requires manual recompile, see 'emacs straight' notes
(setq straight-check-for-modifications nil)

(defvar bootstrap-version)
(let ((bootstrap-file
      (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
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
(setq use-package-verbose t)
(setq use-package-always-defer t)

(use-package use-package-ensure-system-package
  :straight t)

;; Elisp libraries
(use-package dash :demand :hook (emacs-lisp-mode . dash-fontify-mode))
(use-package s :demand)
(use-package f :demand)

(use-package cus-edit
  :demand
  :straight nil
  :custom (custom-file "~/.emacs.d/minimal-custom.el")
  :config
  (unless (file-exists-p custom-file)
    (with-temp-buffer (write-file custom-file)))
  (load-file custom-file))
