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

(use-package emacs
  :demand
  :straight nil
  :hook
  (prog-mode . (lambda ()
		 (visual-line-mode)
		 (setq line-spacing 0.1)))
  :init
  ;; font strings "FiraCode Nerd Font"
  (set-face-attribute 'default nil :font "IBM Plex Mono" :height (when-monitor-size :small 130 :large 140))
  (set-face-attribute 'variable-pitch nil :font "IBM Plex Serif")
  (set-face-attribute 'fixed-pitch nil :font "IBM Plex Mono")
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)


  (setq indent-tabs-mode nil
	read-process-output-max (* 4 1024 1024) ; increase performance: https://emacs-lsp.github.io/lsp-mode/page/performance/

	ring-bell-function #'ignore
	visible-bell t

	window-resize-pixelwise t
	frame-resize-pixelwise t
	;; frame-title-format "%b"
	frame-title-format nil

	create-lockfiles nil
	kill-buffer-query-functions nil
	delete-by-moving-to-trash t
	native-comp-async-report-warnings-errors nil)

  ;; don't wrap lines
  (custom-set-variables '(truncate-lines t))

  ;; prevent unsafe dir-locals being pestered for
  ;; https://emacs.stackexchange.com/questions/10983/remember-permission-to-execute-risky-local-variables
  (advice-add 'risky-local-variable-p :override #'ignore))

(use-package modus-themes
  ;; docs are hidden away a bit https://protesilaos.com/emacs/modus-themes#h:51ba3547-b8c8-40d6-ba5a-4586477fd4ae
  :init
  (modus-themes-load-themes)
  :hook
  (after-init . (lambda ()
		  (modus-themes-load-operandi)
					;(solaire-global-mode)
		  ))
  :custom
  (modus-themes-italic-constructs t)
  (modus-themes-bold-constructs t)
  (modus-themes-variable-pitch-headings t)
  (modus-themes-variable-pitch-ui t)
  ;; (modus-themes-mode-line '(accented borderless (padding . 4) (height . 1.2)))
  (modus-themes-mode-line '(borderless))
  (modus-themes-paren-match '(underline))
  (modus-themes-org-blocks 'tinted-background)
  (modus-themes--markup '(background))
  (modus-themes-headings '((1 . (1.3))
			   (2 . (1.2))))
  (modus-themes-syntax '(alt-syntax yellow-comments)) ; more colors
  (modus-themes-prompts '(bold))
  (modus-themes-lang-checkers '(background straight-underline))
  ;; (modus-themes-hl-line '(underline accented)) ;underline clashes with brackets
  (modus-themes-hl-line '(accented))
  (modus-themes-operandi-color-overrides '(;; (bg-main . "#EFEFEF") ; readable, bit a little too grey
					   (bg-main . "#ffffff")
					   (fg-main . "#1b1b1b")))
  (modus-themes-vivendi-color-overrides '((bg-main . "#24292f")))
  :config
  (set-face-attribute 'default nil :font "IBM Plex Mono")
  (set-face-attribute 'variable-pitch nil :font "IBM Plex Serif")
  (modus-themes-load-operandi)

  (defun my/apply-theme (appearance)
    "Load theme, taking current system APPEARANCE into consideration."
    (mapc #'disable-theme custom-enabled-themes)
    (pcase appearance
      ('light (load-theme 'modus-operandi t))
      ('dark (load-theme 'modus-vivendi t))))

  (add-hook 'ns-system-appearance-change-functions #'my/apply-theme))

(use-package restart-emacs
  :commands restart-emacs)

(use-package helpful
  :bind
  ([remap describe-function] . helpful-callable)
  ([remap describe-command] . helpful-command)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-key] . helpful-key))

(use-package pulsar
  :straight
  (pulsar :host github :repo "protesilaos/pulsar")
  :hook
  (emacs-startup . pulsar-global-mode)
  :custom
  (pulsar-face 'pulsar-yellow)
  (pulsar-delay 0.08)
  (pulsar-pulse-functions '(recenter-top-bottom
			    move-to-window-line-top-bottom
			    reposition-window
			    forward-page
			    backward-page
			    scroll-up-command
			    scroll-down-command
			    org-next-visible-heading
			    org-previous-visible-heading
			    org-forward-heading-same-level
			    org-backward-heading-same-level
			    outline-backward-same-level
			    outline-forward-same-level
			    outline-next-visible-heading
			    outline-previous-visible-heading
			    outline-up-heading

			    ;; undo/redo
			    undo-tree-undo
			    undo-tree-redo
			    
			    ;; evil
			    evil-scroll-line-to-center
			    evil-scroll-line-to-bottom
			    evil-scroll-line-to-top
			    evil-search-next
			    evil-search-previous

			    evil-window-vsplit
			    evil-window-split

			    evil-goto-definition
			    evil-jump-backward
			    evil-jump-forward

			    goto-last-change
			    goto-last-change-reverse)))

(use-package hl-line
  :straight nil
  :hook ((prog-mode text-mode messages-buffer-mode Info-mode helpful-mode) . hl-line-mode))

(use-package lin
  :hook (emacs-startup . lin-global-mode))
