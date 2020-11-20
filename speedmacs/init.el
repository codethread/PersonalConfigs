;;; init.el --- emacs config focusing on a lagless experience -*- no-byte-compile: t -*-

;;; Commentary:

;; I hate lag, so some features around fuzzy searching and UI enhancements
;; are disabled in favour of snappiness.
;; Startup speed is less of an issue so not fussed about that, I only
;; need to restart in the event of an error.

;;; Code:

;; -----------------------------------------------------
;;;; Startup Performance
;; -----------------------------------------------------

;; reduce the frequency of garbage collection by making it happen on
;; each 100MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold (* 100 1000 1000))

;; Profile emacs startup
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "*** Emacs loaded in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time)))
		     gcs-done)))
(defmacro my/with-advice (adlist &rest body)
  "Execute BODY with temporary advice in ADLIST.

Each element of ADLIST should be a list of the form
  (SYMBOL WHERE FUNCTION [PROPS])
suitable for passing to `advice-add'.  The BODY is wrapped in an
`unwind-protect' form, so the advice will be removed even in the
event of an error or nonlocal exit."
  (declare (debug ((&rest (&rest form)) body))
           (indent 1))
  `(progn
     ,@(mapcar (lambda (adform)
                 (cons 'advice-add adform))
               adlist)
     (unwind-protect (progn ,@body)
       ,@(mapcar (lambda (adform)
                   `(advice-remove ,(car adform) ,(nth 2 adform)))
                 adlist))))

(defun my/call-logging-hooks (command &optional verbose)
  "Call COMMAND, reporting every hook run in the process.
Interactively, prompt for a command to execute.

Return a list of the hooks run, in the order they were run.
Interactively, or with optional argument VERBOSE, also print a
message listing the hooks."
  (interactive "CCommand to log hooks: \np")
  (let* ((log     nil)
         (logger (lambda (&rest hooks) 
                   (setq log (append log hooks nil)))))
    (my/with-advice
        ((#'run-hooks :before logger))
      (call-interactively command))
    (when verbose
      (message
       (if log "Hooks run during execution of %s:"
         "No hooks run during execution of %s.")
       command)
      (dolist (hook log)
        (message "> %s" hook)))
    log))

;; (setq package-enable-at-startup nil)
;; (package-initialize)

;; control some file locations, which could be shared across configs, rather in .emacs.d
(setq user-emacs-directory "~/.emacs.d"
      backup-directory-alist `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" user-emacs-directory))

;; Keep customization settings in a temporary file (thanks Ambrevar!)
;; (setq custom-file
;;       (if (boundp 'server-socket-dir)
;; 	  (expand-file-name "custom.el" server-socket-dir)
;; 	(expand-file-name (format "emacs-custom-%s.el" (user-uid)) temporary-file-directory)))

;; (load custom-file t)

(server-start)


;; -----------------------------------------------------
;;; Per System Settings
;; -----------------------------------------------------

;; not sure if needed but can take inspiration from
;; https://github.com/daviwil/dotfiles/blob/master/Emacs.org#system-settings


;; -----------------------------------------------------
;;; Set up package archives
;; -----------------------------------------------------
(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")))

;; -----------------------------------------------------
;;; Keep everything smooth and up-to-date
;; -----------------------------------------------------

;; (byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0)
(unless (package-installed-p 'auto-compile)
  (package-refresh-contents)
  (package-install 'auto-compile))

(require 'auto-compile)
(auto-compile-on-load-mode)
(auto-compile-on-save-mode)

;; -----------------------------------------------------
;; Package Manager
;; -----------------------------------------------------

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; all packages will be installed if not already present
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; controls minor mode descriptions in modeline
(use-package delight)

(use-package cus-edit
  :ensure nil
  :defer t
  :custom
  (custom-file null-device "Don't store customizations"))

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
)
  ;; Enable emoji, and stop the UI from freezing when trying to display them.
  ;; (if (fboundp 'set-fontset-font)
  ;;     (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))

;; -----------------------------------------------------
;; Emacs Settings
;; -----------------------------------------------------

(defalias 'yes-or-no-p 'y-or-n-p)

;; Backup and Autosave Directories
;; TODO: look into this later as it keeps throwing errors
;; (setq temporary-file-directory "~/tmp/")
;; (setq backup-directory-alist '((".*" . temporary-file-directory)))
;; (use-package files
;;   :ensure nil
;;   :custom
;;   (;; This should prevent backup files appearing in the same directory
;;    ;; instead they should end up in temporary-file-directory
;;    ()))

(when window-system
  (setq browse-url-browser-function 'xwidget-webkit-browse-url))

;; recomended to increase performance from https://emacs-lsp.github.io/lsp-mode/page/performance/
(setq read-process-output-max (* 1024 1024)) ;; 1mb

(setq indent-tabs-mode nil
      inhibit-startup-screen t

      large-file-warning-threshold 100000000 ; warn when opening files bigger than 100MB
      line-number-display-limit 1 ; no line numbers in modeline

      delete-by-moving-to-trash t
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
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)

(setq frame-title-format nil)

;; Set symbol for the border
(unless window-system
 (set-display-table-slot standard-display-table
			'vertical-border
			(make-glyph-code ?┃)))

(setq kill-buffer-query-functions nil)
(electric-pair-mode)

;; -----------------------------------------------------
;; Vanila Improvements
;; -----------------------------------------------------

(use-package vc-hooks
  :ensure nil
  :custom
  ((vc-follow-symlinks t)))

(use-package paren
  :ensure nil
  :init
  (setq show-paren-mode t)
  :custom
  ((show-paren-when-point-inside-paren t)
   (show-paren-when-point-in-periphery t))
  :hook (prog-mode . show-paren-mode))


(use-package url-history
  :ensure nil
  :custom
  ((url-history-file (expand-file-name "url/history" user-emacs-directory))))

(use-package compile
  :ensure nil
  :custom
  ((compilation-scroll-output t)))

(use-package eldoc
  :delight)

(use-package dired-single)

;; (use-package all-the-icons-dired
;;   :hook (dired-mode . all-the-icons-dired-mode))

(use-package dired
  :after (evil evil-collection)
  :ensure nil
  :commands (dired dired-jump)
  :bind
  (:map evil-normal-state-map
	("-" . dired-jump))
  :custom
  ((dired-listing-switches "-Algho --group-directories-first"))
  :config
  (evil-collection-define-key 'normal 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer
    "L" 'dired-display-file))

;; (use-package elec-pair
;;   :ensure nil
;;   :hook
;;   (prog-mode . electric-pair-local-mode))

;; -----------------------------------------------------
;; Hydras
;; -----------------------------------------------------

(use-package hydra
  :demand
  :commands
  ;; remove flycheck/byte-compiler warnings about missing commands
  hydra-default-pre
  hydra-keyboard-quit
  hydra--call-interactively-remap-maybe
  hydra-show-hint
  hydra-set-transient-map)

;; -----------------------------------------------------
;; Lore friendly improvements
;; -----------------------------------------------------
(use-package restart-emacs)

(use-package which-key
  :delight
  :config
  (which-key-mode t)
  (which-key-add-key-based-replacements
    "<SPC> b" "Buffers"
    "<SPC> e" "Errors"
    "<SPC> E" "Spelling"
    "<SPC> f" "Files"
    "<SPC> F" "Fold"
    "<SPC> g" "Global"
    "<SPC> n" "Notes"
    "<SPC> p" "Projects"
    "<SPC> s" "Search"
    "<SPC> S" "Web"
    "<SPC> t" "Term"
    "<SPC> T" "Toggle"
    "<SPC> w" "Window"
    "<SPC> W" "Layouts"
    )
  (which-key-add-major-mode-key-based-replacements 'org-mode
  ", d" "Delete"
  ", h" "Heading"
  ", i" "Insert"
  ", l" "List"
  ))

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package expand-region
  :demand
  :commands er/contract-region
  :config
  (defhydra hydra-expand-region (global-map "C-=")
    "expand-region"
    ("k" er/expand-region "expand")
    ("j" er/contract-region "shrink")))

(use-package undo-tree
  :delight
  :custom
  ((undo-tree-auto-save-history t)
   (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))
  :config
  (global-undo-tree-mode))

;; adds highlights to TODO and FIXME.
(use-package fic-mode
  :hook
  (prog-mode)
  :config
  (custom-set-faces
   '(fic-face ((t (:inherit warning :weight bold))))))

;; jump to char/word/line
(use-package avy
  :custom
  ((avy-background t)))

(use-package ace-window
  :bind ("M-o" . ace-window)
  :commands
  (ace-win-swap ace-win-delete)
  :config
  (setq aw-ignore-current t)
  (setq aw-minibuffer-flag t)
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))

  (defun ace-win-delete ()
    (interactive)
    (ace-window 16))

  (defun ace-win-swap ()
    (interactive)
    (ace-window 4))

  ;; turn off grey background
  ;; (setq aw-background nil)
  (custom-set-faces '(aw-leading-char-face ((t (:inherit warning :weight bold :height 2.0))))))

(use-package yasnippet
  :bind
  ("C-c y s" . yas-insert-snippet)
  ("C-c y v" . yas-visit-snippet-file)
  :config
  ;; http://andreacrotti.github.io/yasnippet-snippets/snippets.html
  (use-package yasnippet-snippets)
  (yas-global-mode 1))


;; Completion will start automatically after you type a few letters.
;; Use M-n and M-p to select, <return> to complete or <tab> to complete the common part.
;; Search through the completions with C-s, C-r and C-o. Press M-(digit) to quickly
;; complete with one of the first 10 candidates.
(use-package company
  ;; :hook (lsp-mode . company-mode)
  :config
  (global-company-mode)
  :bind (:map company-active-map
         ("C-l" . company-complete-selection))
        (:map lsp-mode-map
         ("<tab>" . company-indent-or-complete-common))
  :custom
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 100)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; -----------------------------------------------------
;; Terminal
;; -----------------------------------------------------

(use-package vterm
  :config
  (defun terminal ()
    "Switch to terminal. Launch if nonexistent."
    (interactive)
    (if (get-buffer "terminal")
        (switch-to-buffer "terminal")
      (vterm "terminal"))
    (get-buffer-process "terminal")))

(use-package multi-vterm
  :after (vterm evil)
  :config
  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (setq-local evil-insert-state-cursor 'box)
	      (evil-insert-state)))
  (define-key vterm-mode-map [return] #'vterm-send-return)

  (setq vterm-keymap-exceptions nil))

;; -----------------------------------------------------
;; Themes
;; -----------------------------------------------------

;; pretty sure this keeps causing flickering
(use-package solaire-mode
  :config
  (setq solaire-mode-remap-modeline nil))

(use-package doom-themes
  :init
  (solaire-global-mode +1)
  :custom
  ((doom-themes-enable-bold t)
   (doom-themes-enable-italic t))
  :config
  (load-theme 'doom-one t)
  ;; (load-theme 'doom-nord t)
   ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package nord-theme
  :disabled
  :config
  (load-theme 'nord t))

(use-package kaolin-themes
  :disabled
  :config
  (load-theme 'kaolin-dark t))

;; set these after theme load
(set-face-attribute 'default nil :font "Hack Nerd Font")

(set-face-attribute 'font-lock-comment-face nil :slant 'italic)

(set-face-attribute 'font-lock-function-name-face nil :slant 'italic)
(set-face-attribute 'font-lock-variable-name-face nil :weight 'semi-bold)

;; Set the fixed pitch face
;; (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina")

;; Set the variable pitch face
;; (set-face-attribute 'variable-pitch nil :font "Avenir Next" :weight 'regular)

(use-package all-the-icons)

(use-package doom-modeline
  ;; :unless window-system
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 20
	doom-modeline-env-version t
	doom-modeline-modal-icon t
	doom-modeline-vcs-max-length 24
	doom-modeline-buffer-file-name-style 'truncate-except-project
	;; Whether display buffer encoding.
	doom-modeline-buffer-encoding nil))

(use-package smart-mode-line-atom-one-dark-theme
  :disabled)

;; smart-mode-line
(use-package smart-mode-line
  :disabled
  :custom
  (sml/no-confirm-load-theme t)
  :config
  (setq sml/theme 'atom-one-dark)
  (sml/setup))

;; change mode-line color by evil state
;;(lexical-let ((default-color (cons (face-background 'mode-line)
;;				   (face-foreground 'mode-line))))
;;  (add-hook 'post-command-hook
;;	    (lambda ()
;;	      (let ((color (cond ((minibufferp) default-color)
;;				 ((evil-insert-state-p) '("#e80000" . "#ffffff"))
;;				 ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
;;				 ((buffer-modified-p)   '("#006fa0" . "#ffffff"))
;;				 (t default-color))))
;;		(set-face-background 'mode-line (car color))
;;		(set-face-foreground 'mode-line (cdr color))))))

;; -----------------------------------------------------
;; Evil -- start
;; -----------------------------------------------------

(use-package evil-leader
  :delight
  :init
  (setq evil-want-keybinding 'nil)
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<SPC>" 'counsel-projectile-find-file
    ";" 'counsel-M-x
    ":" 'counsel-command-history
    "." 'ace-window

    ;; b --- buffers
    "bK" 'kill-buffer
    "bL" 'counsel-ibuffer
    "bN" 'evil-split-next-buffer
    "bP" 'evil-split-prev-buffer

    "bb" 'my|split-last-buffer
    "be" 'projectile-ibuffer
    "bj" 'evil-show-jumps
    "bk" 'my|kill-this-buffer
    "bl" 'counsel-projectile-switch-to-buffer
    "bn" 'evil-next-buffer
    "bp" 'evil-prev-buffer
    "br" 'rename-buffer
    "bx" 'font-lock-fontify-buffer ;; repaint the buffer

    ;; e -- error
    "ef" 'my|eslint-fix-file-and-revert
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error
    "el" 'flycheck-list-errors
    "ee" 'flycheck-buffer
    ;; "ee" 'flycheck-display-error-at-point ;; not sure?
    "eh" 'flycheck-explain-error-at-point ;; not sure?
    "ei" 'flycheck-verify-setup

    ;; E - flyspell
    "EE" 'flyspell-correct-wrapper
    "ES" 'my|my-save-word
    "EB" 'flyspell-buffer
    "EP" 'evil-prev-flyspell-error
    "EN" 'evil-next-flyspell-error

    ;; f --- File
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "fv" 'my|open-init-file
    "fk" 'my|delete-file-and-buffer
    "fs" 'save-buffer
    "fS" 'projectile-save-project-buffers

    ;; F --- fold
    "FF"  'hydra-hs/body
    "FO"  'hydra-folding/body

    ;; g -- global
    "gs" 'my|reload-init-file ;; TODO make more glorious
    "gg" 'magit-status
    "gp" 'my|pomo
    "gP" 'my|pomo-stop
    "gl" 'counsel-find-library

    ;; w -- window
    "wK" 'elscreen-kill
    "we" 'hydra-tabs/body
    "wN" 'elscreen-create
    "wd" 'ace-win-delete
    "wk" 'delete-window
    "wl" 'elscreen-toggle
    "wo" 'delete-other-windows
    "wr" 'elscreen-screen-nickname
    "ws" 'ace-win-swap
    "wt" 'elscreen-toggle-display-tab
    "ww" 'evil-window-vsplit

    ;; W -- window configurations
    "WW" 'ivy-push-view
    "WD" 'ivy-pop-view
    "Wl" 'ivy-switch-view

    ;; s -- search
    "sf" 'swiper			;; great when you know what you need
    "si" 'counsel-imenu			;; jump to def or explore
    "sI" 'helm-imenu-in-all-buffers	;; ideal when don't know
    "sp" 'deadgrep			;; also ag or grep
    "ss" 'ripgrep-regexp		;; search from current dir out
    "sl" 'xref-find-references		;; also ag or grep

    ;;
    "Sn" 'xwidget-webkit-browse-url
    "SS" '(lambda () (interactive) (xwidget-webkit-new-session "https://google.com"))
    "Sg" 'my|browse-at-point
    ;; "SS" 'xwidget-webkit-cx3

    ;; n --- notes
    "na" 'org-agenda
    "nb" 'org-switchb
    "nc" 'org-capture
    "nh" 'helm-org-agenda-files-headings ;; search through headings
    "nl" 'org-store-link
    "nn" 'my|open-my-notes-file
    "nN" 'my|open-work-notes-file

    ;; p --- project
    "p!" 'projectile-run-shell-command-in-root		;;  "Run cmd in project root"
    "pa" 'projectile-add-known-project			;;  "Add new project"
    "pb" 'projectile-switch-to-buffer			;;  "Switch to project buffer"
    "pc" 'projectile-compile-project			;;  "Compile in project"
    "pd" 'projectile-find-dir				;;  "Remove known project"
    "pe" 'projectile-edit-dir-locals			;;  "Edit project .dir-locals"
    "pf" 'counsel-projectile-find-file			;;  "Find file in project"
    "pF" 'projectile-find-file-in-known-projects	;;  "Find file in project"
    "pg" 'projectile-find-file-dwim			;;  "Find file in project at point better ffap"
    "pi" 'projectile-invalidate-cache			;;  "Invalidate project cache"
    "pk" 'projectile-kill-buffers			;;  "Kill project buffers"
    "po" 'projectile-toggle-between-implementation-and-test
    "pO" 'projectile-find-other-file			;;  "Find other file"
    "pp" 'counsel-projectile-switch-project		;;  "Switch project"
    "pr" 'projectile-recentf				;;  "Find recent project files"
    "pR" 'projectile-regenerate-tags			;;  "Find recent project files"
    "ps" 'deadgrep					;;  "Search with rg"
    "pt" 'my|test-file					;; test file in project

    ;; t --- terminal
    "tn" 'multi-vterm
    "tt" 'multi-vterm-project

    ;; T --- Toggle
    "TT" 'toggle-truncate-lines
    "Ti" 'lsp-ui-imenu
    "Tp" 'electric-pair-local-mode
    "Tu" 'undo-tree-visualize
    "Tv" 'visual-line-mode
    "Tw" 'toggle-word-wrap

    ;; r --- run
    "r" 'hydra-window/body
    ;; "r" 'my|run-ruby

    ))

(use-package evil
  :after (evil-leader avy)
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-want-C-i-jump nil)
  (setq evil-respect-visual-line-mode t)
  :bind
  (:map evil-insert-state-map
	;; ("C-@"   . completion-at-point)
	;; ("C-SPC" . completion-at-point))
	("C-@"   . company-complete)
	("C-SPC" . company-complete)
	("C-l"   . (lambda ()
		     (interactive)
		     (right-char 1))))
	;; ("C-h"   . char-left))
  (:map evil-normal-state-map
	("C-\\" . counsel-projectile-rg)
	("u"    . undo-tree-undo)
	("C-r"  . undo-tree-redo)
	("C-e"	. move-end-of-line) ; replace scroll up
	("C-u"	. evil-scroll-up) ; get scroll up back and replace with C-m as it's just return
	("C-y"	. universal-argument)
	("s"	. avy-goto-word-1)
	("S"	. avy-goto-char)
	("gf"	. projectile-find-file-dwim)
	("gh"	. lsp-describe-thing-at-point))
  :config
  ;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (defadvice evil-inner-word (around underscore-as-word activate)
    (let ((table (copy-syntax-table (syntax-table))))
      (modify-syntax-entry ?_ "w" table)
      (with-syntax-table table
	ad-do-it)))

  (evil-mode t)
  (define-key universal-argument-map (kbd "C-y") 'universal-argument-more)
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  (evil-define-key 'normal org-mode-map ",c" 'org-toggle-checkbox)
  ;; - thing => - [ ] thing => - thing
  (evil-define-key 'normal org-mode-map ",lt" 'my|org-toggle-list-checkbox)
  (evil-define-key 'normal org-mode-map ",ls" 'org-sort-list)

  (evil-define-key 'normal org-mode-map ",g" 'org-open-at-point)
  (evil-define-key 'normal org-mode-map ",hh" 'org-toggle-heading)
  (evil-define-key 'normal org-mode-map ",ho" 'evil-org-insert-heading-below)
  (evil-define-key 'normal org-mode-map ",hn" 'org-insert-heading-respect-content)
  (evil-define-key 'normal org-mode-map ",hs" 'org-insert-subheading)
  (evil-define-key 'normal org-mode-map ",dr" 'org-table-kill-row)
  (evil-define-key 'normal org-mode-map ",dc" 'org-table-delete-column)
  (evil-define-key 'normal org-mode-map ",ic" 'org-table-insert-column)
  (evil-define-key 'normal org-mode-map ",i-" 'org-table-insert-hline)
  (evil-define-key 'normal org-mode-map ">" 'org-shiftmetaright)
  (evil-define-key 'normal org-mode-map "<" 'org-shiftmetaleft)
  (evil-define-key 'normal org-mode-map ",s" 'org-sort)
  (evil-define-key 'normal org-mode-map ",I" 'org-toggle-inline-images)
  ;; move over wrapped lines
  (evil-define-key 'normal org-mode-map "j" 'evil-next-visual-line)
  (evil-define-key 'normal org-mode-map "k" 'evil-previous-visual-line)

  (evil-define-key 'normal markdown-mode-map ",c" 'markdown-toggle-markup-hiding)

  ;; move over wrapped lines
  (evil-define-key 'normal markdown-mode-map "j" 'evil-next-visual-line)
  (evil-define-key 'normal markdown-mode-map "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; https://github.com/Somelauw/evil-org-mode
(use-package evil-org
  :delight
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package evil-magit
  :after magit)

(use-package evil-mc
  :delight
  :demand t
  :bind ("M-d" . hydra-mc/body)
  :init
  (defhydra hydra-mc (:color red :hint nil)
    "
Add:		 Jump:
------------------------------
_a_ll		 _N_ext
_n_ext		 _P_revious
_p_reivous
_s_kip
"
    ("a" evil-mc-make-all-cursors)
    ("q" evil-mc-undo-all-cursors "quit" :color blue)
    ("N" evil-mc-make-and-goto-next-cursor)
    ("P" evil-mc-make-and-goto-prev-cursor)
    ("s" evil-mc-skip-and-goto-next-match)
    ("n" evil-mc-make-and-goto-next-match)
    ("p" evil-mc-make-and-goto-prev-match))
  :config
  (global-evil-mc-mode  1))

(use-package evil-commentary
  :delight
  :config
  (evil-commentary-mode t))

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-escape
  :delight
  :custom
  ((evil-escape-key-sequence "jk"))
  :config
  (evil-escape-mode t))

(use-package evil-matchit
    :config
  (global-evil-matchit-mode t))

(use-package evil-args
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))

(use-package elscreen
  :after (evil)
  :demand t
  :custom
  ((elscreen-display-screen-number nil)
   (elscreen-default-buffer-initial-message nil)
   (elscreen-display-tab nil)
   (elscreen-tab-display-kill-screen nil)
   (elscreen-tab-display-control nil))
  :config
  (defhydra hydra-tabs
    (:color red :hint nil
	    :pre (setq elscreen-display-tab t)
	    :post (setq elscreen-display-tab nil))
    "tabs"
    ("l" elscreen-next "next")
    ("h" elscreen-previous "previous")
    ("x" elscreen-kill "close window")
    ("j" nil "quit" :color blue)
    ("n" elscreen-create "new" :color blue))
  (elscreen-start)

  ;; light theme
  ;; :custom-face
  ;; (elscreen-tab-background-face ((t (:background "#dfdfdf" :height 1.3))))
  ;; (elscreen-tab-current-screen-face ((t (:background "#fafafa" :foreground "#a626a4"))))
  ;; (elscreen-tab-other-screen-face ((t (:background "#dfdfdf" :foreground "#a190a7"))))

  :custom-face
  (elscreen-tab-background-face ((t (:background "#1c1f24" :height 1.3))))
  (elscreen-tab-current-screen-face ((t (:background "#282c34" :foreground "#c678dd"))))
  (elscreen-tab-other-screen-face ((t (:background "#1c1f24" :foreground "#a190a7")))))

;; -----------------------------------------------------
;; Evil -- end - do not remove this line
;; -----------------------------------------------------
;; Projects / Navigation
;; -----------------------------------------------------

;; quickly navigate to init.el
(global-set-key (kbd "C-x C-v")
		(lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(use-package projectile
  :delight '(:eval (concat " [" (projectile-project-name) "]"))
  :custom
  ((projectile-completion-system 'default)
   (projectile-known-projects-file
    (expand-file-name "projectile-bookmarks.eld" user-emacs-directory)))
  :config
  (projectile-mode)
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (if (file-directory-p "~/sky")
    (setq projectile-project-search-path '("~/dev" "~/sky" "~/identity"))
    (setq projectile-project-search-path '("~/dev"))))

(use-package counsel-projectile
  :after (projectile counsel)
  :config (counsel-projectile-mode))

(use-package magit
  :bind ("C-x g" . magit-status))

;; NOTE: Make sure to configure a GitHub token before using this package!
;; - https://magit.vc/manual/forge/Token-Creation.html#Token-Creation
;; - https://magit.vc/manual/ghub/Getting-Started.html#Getting-Started
;; most of the time we just need forge-pull
(use-package forge
  :after magit
  :config
  (setq auth-sources '("~/.authinfo")))


(use-package wakatime-mode
  :delight
  :config
  (setq wakatime-cli-path "/usr/local/bin/wakatime")
  (global-wakatime-mode))

;; -----------------------------------------------------
;; Narrowing / Searching / Lists
;; -----------------------------------------------------

;; Tried helm/ivy/ido/selectrum, went with ivy with flx
;;
;; Ido can be faster if fzy matching is turned off but then it's
;; matching must be exact.
;; Ido is harder to configure so it loses to Ivy once all things on
;; performance are made equal.
;; Helm was just slow out of the box, maybe it can be tweaked to be
;; faster but that would probably turn it into Ivy

;; Improves sorting for fuzzy-matched results
(use-package flx
  :defer 1)

(use-package ivy
  :delight
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("C-c C-c" . ivy-immediate-done)
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
  :custom
  ((ivy-use-virtual-buffers t)
   (ivy-wrap t)
   (ivy-count-format "(%d/%d) ")
   (ivy-initial-inputs-alist nil) ; Don't start searches with ^
   (ivy-extra-directories ())) ; hide . and .. from file lists
  :config
  (setq ivy-re-builders-alist
	'((counsel-projectile-rg . ivy--regex-plus)
	  (swiper . ivy--regex-plus) ; fzy search in file is clumsy
	  (t . ivy--regex-fuzzy))) ; use fzy for everything else
  (setq enable-recursive-minibuffers t))

(use-package ivy-rich
  :init
  (ivy-rich-mode 1))

(use-package ivy-posframe
  :custom
  (ivy-posframe-width      115)
  (ivy-posframe-min-width  115)
  (ivy-posframe-height     10)
  (ivy-posframe-min-height 10)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters '((parent-frame . nil)
                                  (left-fringe . 8)
                                  (right-fringe . 8)))
  (ivy-posframe-mode 1))

(use-package counsel
  :bind (("M-x" . counsel-M-x)
	 ("C-x b" . counsel-ibuffer)
	 ("C-x C-f" . counsel-find-file)
	 :map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :custom
  ;; ((counsel-find-file-ignore-regexp "(.|..)"))
  ((counsel-find-file-ignore-regexp ".."))
  :config
  (defun my|yank-pop-replace-selection (&optional arg)
          "Delete the region before inserting poped string."
          (when (and evil-mode (eq 'visual evil-state))
            (kill-region (region-beginning) (region-end))))

  (advice-add 'counsel-yank-pop :before #'moon-override-yank-pop))

;; Adds M-x recent command sorting for counsel-M-x
(use-package smex
  :defer 1
  :after counsel)

;; might want deadgrep-kill-all-buffers in a function
(use-package deadgrep
  :config
  (evil-define-key 'normal deadgrep-mode-map (kbd "C-j") 'deadgrep-forward-filename)
  (evil-define-key 'normal deadgrep-mode-map (kbd "C-k") 'deadgrep-backward-filename))

;; -----------------------------------------------------
;; Languages / General
;; -----------------------------------------------------

(use-package lsp-mode
  :commands lsp
  :hook
  ((typescript-mode js-mode js2-mode web-mode scala-mode) . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :bind (:map lsp-mode-map
	      ("TAB" . completion-at-point))
  :custom
  (lsp-disabled-clients '((json-mode . eslint)))
  (lsp-enable-file-watchers 'nil)
  (lsp-eslint-run "onType")
  (lsp-auto-execute-action 'nil)
  :config
  (setq lsp-eslint-server-command (if (file-directory-p "~/sky")
         '("node"
      "/Users/adh23/.vscode/extensions/dbaeumer.vscode-eslint-2.1.8/server/out/eslintServer.js"
      "--stdio")
       '("node"
      "/Users/adam/.vscode/extensions/dbaeumer.vscode-eslint-2.1.8/server/out/eslintServer.js"
        "--stdio"))))
  ;; (setq lsp-diagnostic-package :none))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind ("C-c C-k" . my|lsp-ui-doc-glance) ; just a test binding
  :config
  (defun my|lsp-ui-doc--glance-hide-frame ()
    "Hook to hide hover information popup for lsp-ui-doc-glance."
    (lsp-ui-doc-hide)
    (remove-hook 'pre-command-hook 'lsp-ui-doc--glance-hide-frame))

  (defun my|lsp-ui-doc-glance ()
    "Trigger display hover information popup and hide it on next typing."
    (interactive)
    (lsp-ui-doc-show)
    (add-hook 'pre-command-hook 'lsp-ui-doc--glance-hide-frame))
  :custom
  (lsp-ui-doc-header 'nil) ; looks shit
  (lsp-ui-sideline-enable 'nil) ; bloody overwhelming
  (lsp-ui-doc-use-webkit 'nil) ; don't be tempted by this, unless you really want to configure it
  (lsp-ui-doc-include-signature 'nil) ; eldoc does a better job of this
  (lsp-ui-doc-show-with-cursor 'nil) ; use keybinding instead or mouse
  (lsp-ui-imenu--custom-mode-line-format "lsp-ui-menu")
  (lsp-ui-doc-border "brightblack")
  (lsp-ui-doc-position 'at-point)) 

(use-package flycheck
  :defer t
  ;; :custom
  ;; ((flycheck-check-syntax-automatically '(idle-buffer-switch save mode-enabled)))
  :hook (prog-mode . flycheck-mode))

(use-package editorconfig
  :delight
  :config
  (editorconfig-mode 1))

;; -----------------------------------------------------
;; Javascript / Typescript / Web
;; -----------------------------------------------------

(use-package add-node-modules-path
  :init
  :hook
  (web-mode)
  (js-mode)
  (js2-mode)
  (typescript-mode)
  (css-mode)
  (rjsx-mode))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :custom
  ((js-indent-level 2))
  :config
  (setq typescript-indent-level 2))

(use-package rjsx-mode
  :custom-face
  (rjsx-tag ((t (:slant italic :foreground "#c678dd"))))
  (rjsx-attr ((t (:foreground "#ECBE7B"))))
  :config
  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-to-list 'interpreter-mode-alist '("node" . rjsx-mode)))

(use-package web-mode
  :mode "\\.tsx\\'"
  :config
  (setq-default web-mode-comment-formats
              '(("javascript" . "//")
                ("typescript" . "//")))

  (add-hook 'web-mode-hook 'my|web-mode-settings)

  (defun my|web-mode-settings ()
    "Hooks for Web mode."
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-enable-auto-quoting nil)
    (setq web-mode-markup-indent-offset 2)))

(use-package jest)
  ;; :after (js2-mode)
  ;; :hook
  ;; (js2-mode . jest-minor-mode))

;; -----------------------------------------------------
;; Scala
;; -----------------------------------------------------
(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package lsp-metals
  :disabled
  :config (setq lsp-metals-treeview-show-when-views-received t))

;; -----------------------------------------------------
;; Others
;; -----------------------------------------------------

(use-package json-mode)

(use-package yaml-mode)

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package graphql-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.gql\\'" . graphql-mode)))

(use-package dotenv-mode
  :config
  (add-hook 'dotenv-mode-hook
            (lambda ()
              (set (make-local-variable 'comment-start) "# ")
              (set (make-local-variable 'comment-end) "")))
  ;; for optionally supporting additional file extensions such as `.env.test' with this major mode
  (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)))

;; -----------------------------------------------------
;; Writing
;; -----------------------------------------------------

(use-package flyspell
  :ensure nil
  :config
  (setq ispell-program-name "/usr/local/bin/aspell"))

(use-package flyspell-correct-ivy
  :after (flyspell ivy))

(use-package ob-typescript)

(use-package org
  :delight
  :init
  (defvar org-directory "~/org-notes/")

  (defvar org-work-file "~/org-notes/org-sky-notes/work.org")
  (defvar org-personal-file "~/org-notes/org-me-notes/notes.org")

  (defvar org-default-notes-file (if (file-directory-p "~/sky")
				     (expand-file-name org-work-file)
				   (expand-file-name org-personal-file)))
  :commands
  (my|open-work-notes-file
   my|open-my-notes-file)
  :hook
  (org-mode . visual-line-mode)
  (org-mode . flyspell-mode)
  (org-mode . abbrev-mode)
  :custom
  ((org-agenda-files (if (file-directory-p "~/sky")
			 '("~/org-notes/org-sky-notes/work.org"
			   "~/org-notes/org-me-notes/notes.org")
			 '("~/org-notes/org-me-notes/notes.org"))))
  :config
  (require 'org-tempo) ;; needed to add this to get template expansion to work again
  ;; set scratch buffer to org mode
  ;; (setq initial-major-mode 'org-mode)

  (setq org-startup-indented t
	org-fontify-done-headline t
	org-fontify-whole-heading-line t
        org-hide-leading-stars t
        org-hide-block-startup t
	org-hide-emphasis-markers t
	org-startup-folded t
	org-log-done 'time
	org-ellipsis " ▾"
        org-image-actual-width nil ; allows images to be resized with #+ATTR_ORG: :width 100
        org-indirect-buffer-display 'current-window
        org-eldoc-breadcrumb-separator " → "
        org-enforce-todo-dependencies t
        org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a."))
	org-agenda-span 8)

  (setq org-todo-keywords
      '((sequence "TODO(t)" "PROGRESS(p)" "|" "DONE(d)")))

  ;; seems broken
  (defun my|org-toggle-list-checkbox ()
    (interactive)
    (org-toggle-checkbox 4))

  (defun my|open-my-notes-file ()
    "Open rough notes."
    (interactive)
    (find-file org-personal-file))

  (defun my|open-work-notes-file ()
    "Open work notes."
    (interactive)
    (find-file org-work-file))

  (defun my|list-to-checkbox (arg)
    (interactive "P")
    (let ((n (or arg 1)))
      (when (region-active-p)
	(setq n (count-lines (region-beginning)
			     (region-end)))
	(goto-char (region-beginning)))
      (dotimes (i n)
	(beginning-of-line)
	(re-search-forward "- " nil t)
	(replace-match "- [ ] ")
	(forward-line))
      (beginning-of-line)))

  ;; (defun org-babel-execute:typescript (body params)
  ;;   (let ((org-babel-js-cmd "npx ts-node < "))
  ;;     (org-babel-execute:js body params)))

  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t) ; allow bash
			       (js . t)
			       (typescript . t)
			       (haskell . t)
			       (ruby . t)
			       (io . t)))

  ;; Highlight done todos with different colors.
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
   'append)

  (setq org-capture-templates
	'(("t" "TODO" entry (file+headline org-default-notes-file "Capture:Collect")
	   "* TODO %? %^G \n  %U" :empty-lines 1)
	  ("s" "Scheduled TODO" entry (file+headline org-default-notes-file "Capture:Collect")
	   "* TODO %? %^G \nSCHEDULED: %^t\n  %U" :empty-lines 1)
	  ("d" "Deadline" entry (file+headline org-default-notes-file "Capture:Collect")
	   "* TODO %? %^G \n  DEADLINE: %^t" :empty-lines 1)
	  ("p" "Priority" entry (file+headline org-default-notes-file "Capture:Collect")
	   "* TODO [#A] %? %^G \n  SCHEDULED: %^t")
	  ("a" "Appointment" entry (file+headline org-default-notes-file "Capture:Collect")
	   "* %? %^G \n  %^t")
	  ("l" "Link" entry (file+headline org-default-notes-file "Capture:Tasks")
	   "* TODO LINK %?\n  %u\n  %a")
	  ("n" "Note" entry (file+headline org-default-notes-file "Capture:Notes")
	   "* %? %^G\n%U" :empty-lines 1)
	  ("j" "Journal" entry (file+datetree org-default-notes-file)
	   "* %? %^G\nEntered on %U\n")))
  (setq org-agenda-custom-commands
	'(("p" todo "PROGRESS")
	  ("w" . "SKYPORT+Name tags searches") ; description for "w" prefix
	  ("ws" tags "+skyport+graphql")
	  ("u" "Unscheduled TODO"
	   ((todo ""
		  ((org-agenda-overriding-header "\nUnscheduled TODO")
		   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'timestamp)))))
	   nil
	   nil)
	  ("a" "adenda and things"
	   ((agenda)
	    (todo "PROGRESS")
	    (tags-todo "work")
	    (tags-todo "skyport")))))
  )

;; only show bullets in gui
(use-package org-bullets
  :if window-system
  :commands org-bullets-mode
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-download
  :config
  ;; Drag-and-drop to `dired`
  (add-hook 'dired-mode-hook 'org-download-enable))

(use-package org-alert
  :custom (alert-default-style 'osx-notifier)
  :config
  (setq org-alert-interval 300)
  (org-alert-enable))

(use-package markdown-mode
  :hook
  (markdown-mode . flyspell-mode)
  (markdown-mode . visual-line-mode))

(use-package markdown-toc)

(require 'my-fns)

(global-set-key (kbd "C-s-<f8>") 'my|close-notifications-mac)
;; Put this somewhere better
(global-set-key (kbd "C-M-<left>") 'frame-half-size-left)
(global-set-key (kbd "C-M-<right>") 'frame-half-size-right)
(global-set-key (kbd "C-M-<return>") 'toggle-frame-maximized)

;; -----------------------------------------------------
;;; init.el ends here
