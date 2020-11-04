;;; Startup Performance
;; -----------------------------------------------------

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold (* 50 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))

(setq user-emacs-directory "~/.emacs.d"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      url-history-file (expand-file-name "url/history" user-emacs-directory)
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory)
      projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" user-emacs-directory))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
	  (expand-file-name "custom.el" server-socket-dir)
	(expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))
(load custom-file t)

;;; Per System Settings
;; -----------------------------------------------------

;; not sure if needed but can take inspiration from https://github.com/daviwil/dotfiles/blob/master/Emacs.org#system-settings

;; -----------------------------------------------------
;; Package Manager
;; -----------------------------------------------------

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; -----------------------------------------------------
;; Load Paths
;; -----------------------------------------------------

;; Add custom elisp files to load-path - used by require
(push "~/.emacs.d/elisp" load-path)

;; -----------------------------------------------------
;; OSX specific code
;; -----------------------------------------------------

(declare-function exec-path-from-shell-initialize "exec-path-from-shell.el")

(when (eq system-type 'darwin)
  ;; On OS X Emacs doesn't use the shell PATH if it's not started from
  ;; the shell. Let's fix that:
  (use-package exec-path-from-shell
    :custom
    (exec-path-from-shell-arguments '("-l"))
    (exec-path-from-shell-variables '("PATH" "MANPATH" "SPOTIFY_TOKEN" "SLACK_SKY_EMACS_TOKEN"))
    :config
    (exec-path-from-shell-initialize))

  ;; fix alt as meta key
  (setq ns-function-modifier 'hyper) 

  ;; Enable emoji, and stop the UI from freezing when trying to display them.
  (if (fboundp 'set-fontset-font)
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))

;; -----------------------------------------------------
;; Emacs Settings
;; -----------------------------------------------------

(setq inhibit-startup-message t)	; straight to scratch

;; Backup and Autosave Directories
(setq temporary-file-directory "~/tmp/")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq vc-follow-symlinks t
      compilation-scroll-output t
      indent-tabs-mode nil
      show-paren-mode t ; highlight parens

      large-file-warning-threshold 100000000 ; warn when opening files bigger than 100MB

      help-window-select t
      ;; split-width-threshold 170 ; always split vertically if there's room
      ;; split-height-threshold nil ; Split horizontally when opening a new window from a command
      ring-bell-function #'ignore
      visible-bell t
      window-resize-pixelwise t
      save-abbrevs 'silently
      frame-resize-pixelwise t
      ;; dired-listing-switches "-lat" ; list, all, alphabetical
      )

;; control amount of fontification, can be done per mode if slow
(setq font-lock-maximum-decoration t)

;; backup file config
(setq backup-by-copying t ; slow but sure way of saving
      ;; auto-save-file-name-transforms `((".*" . "~/.emacs-file-saves")) ; store all backup files in home directory

      ;; If that's too slow for some reason you might also
      ;; have a look at backup-by-copying-when-linked
      ;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
      version-control t                 ; version numbers for backup files

      delete-old-versions t ; delete excess backup files silently
      )

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;; (setq ns-use-proxy-icon nil) ;; not sure why undefined

;; Set symbol for the border
(set-display-table-slot standard-display-table
			'vertical-border
			(make-glyph-code ?┃))

(setq frame-title-format nil)

(load-theme 'wombat)

(set-face-attribute 'default nil :font "Hack Nerd Font" :height 140)

;; -----------------------------------------------------
;; Packages
;; -----------------------------------------------------

(use-package delight)

(use-package which-key
  :delight
  :config
  (which-key-mode t)
  (setq which-key-idle-delay 0.3))

(use-package eldoc
  :delight)

;; -----------------------------------------------------
;; Evil
;; -----------------------------------------------------

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  :bind
  (:map evil-normal-state-map
	("C-e"		. move-end-of-line) ; reset
	)
  :config
  (evil-mode t))

(use-package evil-commentary
  :delight
  :config
  (evil-commentary-mode t))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :config
  (evil-collection-init))

;; -----------------------------------------------------
;; Projects / Navigation
;; -----------------------------------------------------

;; quickly navigate to init.el
(global-set-key (kbd "C-x C-v")
		(lambda ()
		  (interactive)
		  (find-file "~/.emacs.d/init.el")))

(use-package projectile
  :delight '(:eval (concat " " (projectile-project-name)))
  :custom
  ((projectile-completion-system 'default))
  :config (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired))

;; -----------------------------------------------------
;; Narrowing / Searching / Lists
;; -----------------------------------------------------

;; Tried helm/ivy/ido/selectrum, went with ivy default sorting/filtering
;;
;; ivy seems most performant. Ido can be faster if fzy matching is turned off but then it's matching must be exact.
;; ido + flx was most precise but lag was considerable in a large work project, in a smaller project its benifits
;; probably wouldn't be noticable. Helm was just slow out of the box, maybe it can be tweaked to be faster but
;; that would probably turn it into Ivy
;; ivy + flx

;; Improves sorting for fuzzy-matched results
(use-package flx 
  :init
  (setq ivy-flx-limit 10000))

(use-package ivy
  :delight
  :bind (("C-s" . swiper)
         :map ivy-minibuffer-map
         ("TAB" . ivy-alt-done)
         ("C-l" . ivy-alt-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         ("C-l" . ivy-done)
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-k" . ivy-previous-line)
         ("C-d" . ivy-reverse-i-search-kill))
  :init
  (ivy-mode 1)
  :config
  (setq ivy-re-builders-alist
	'((projectile-find-file . ivy--regex-plus) ; turn off fzy for these larger lists as its slow
	  (t . ivy--regex-fuzzy))) ; use fzy for everything else
  (setq ivy-use-virtual-buffers t)
  (setq ivy-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-initial-inputs-alist nil)
  (setq enable-recursive-minibuffers t)) ;; Don't start searches with ^

(use-package counsel
  :bind (("M-x" . counsel-M-x)
         ("C-x b" . counsel-ibuffer)
         ("C-x C-f" . counsel-find-file)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history)))

;; (use-package smex ;; Adds M-x recent command sorting for counsel-M-x
;;   :defer 1
;;   :after counsel)

;; -----------------------------------------------------
;; Languages
;; -----------------------------------------------------

(use-package lsp-mode
  :commands lsp
  :hook ((typescript-mode js2-mode web-mode) . lsp)
  :bind (:map lsp-mode-map
         ("TAB" . completion-at-point)))

(use-package add-node-modules-path
  :init
  :hook
  (web-mode)
  (js-mode)
  (js2-mode)
  (rjsx-mode))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2))

(defun dw/set-js-indentation ()
  (setq js-indent-level 2)
  (setq evil-shift-width js-indent-level)
  (setq-default tab-width 2))

(use-package js2-mode
  :mode "\\.jsx?\\'"
	:hook
	('my|web-checkers)
  :config
	(defun my|web-mode-checkers ()
		"Use eslint despite lsp's enthusiasm"
		(interactive)
		(setq-local flycheck-checker 'javascript-eslint))

	(add-hook 'js2-mode-hook 'my|web-mode-checkers)

	;; Use js2-mode for Node scripts
  (add-to-list 'magic-mode-alist '("#!/usr/bin/env node" . js2-mode))

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)

  ;; Set up proper indentation in JavaScript and JSON files
  (add-hook 'js2-mode-hook #'dw/set-js-indentation)
  (add-hook 'json-mode-hook #'dw/set-js-indentation))

(use-package prettier-js
  :hook ((js2-mode . prettier-js-mode)
         (typescript-mode . prettier-js-mode))
  :config
  (setq prettier-js-show-errors nil))

(use-package flycheck
  :defer t
  :hook (prog-mode . flycheck-mode))
