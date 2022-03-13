;;; init.el --- emacs config focusing on a lagless experience -*- no-byte-compile: t; lexical-binding: t -*-
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


;;; Initial packages --- these must load before everything

;; Elisp libraries
(use-package dash)
(use-package s)
(use-package f)

(use-package cus-edit
  :demand
  :straight nil
  :custom (custom-file "~/.emacs.d/custom.el")
  :config
  (unless (file-exists-p custom-file)
    (with-temp-buffer (write-file custom-file)))
  (load-file custom-file))

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :demand
  :custom
  (exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-variables '("PATH" "MANPATH" "SPOTIFY_TOKEN" "SLACK_SKY_EMACS_TOKEN"))
  :config
  (exec-path-from-shell-initialize))

(use-package hydra
  :demand t)

(use-package use-package-hydra
  :demand t)

(use-package general
  :demand
  :config
  (general-evil-setup t)

  (defun my/open-scratch-buffer ()
    "Open the *scratch* buffer."
    (interactive)
    (switch-to-buffer "*scratch*"))

  (defun my/open-messages-buffer ()
    "Open the *Messages* buffer."
    (interactive)
    (switch-to-buffer "*Messages*"))

  (defun my/open-init-file ()
    "Open my init.el buffer."
    (interactive)
    (find-file (concat user-emacs-directory "/init.el")))

  (general-create-definer my-leader-def
    :prefix "SPC"
    :keymaps 'override
    :states '(normal visual))
  
  (general-create-definer my-local-leader-def
    :prefix ","
    :keymaps 'override
    :states '(normal visual))

  (my-leader-def
    "b" '(:ignore t :wk "Buffers")
    "bK" 'kill-buffer
    "br" 'rename-buffer

    "e" '(:ignore t :wk "Errors")
    "E" '(:ignore t :wk "Spelling")
    "f" '(:ignore t :wk "Files")
    "j" '(:ignore t :wk "Test")
    "g" '(:ignore t :wk "Global")
    "n" '(:ignore t :wk "Notes")

    "o" '(:ignore t :wk "Open")
    "os" 'my/open-scratch-buffer
    "om" 'my/open-messages-buffer
    "oi" 'my/open-init-file
    "ot" 'vterm

    "p" '(:ignore t :wk "Projects")
    "s" '(:ignore t :wk "Search")
    "t" '(:ignore t :wk "Term")

    "T" '(:ignore t :wk "Toggle")
    "TT" 'visual-line-mode ; when true, lines are not broken over multiple, instead they clip (useful for long tables in org)
    "Tc" (general-predicate-dispatch 'centered-window-mode
	   (string-equal major-mode "org-mode") 'olivetti-mode)
    "Th" 'my/tree-sitter-hl
    "Ti" 'lsp-ui-imenu
    "Tl" 'global-display-line-numbers-mode
    "Tp" 'electric-pair-local-mode
    "Tu" 'undo-tree-visualize
    "Tw" 'toggle-word-wrap
    "Tz" 'global-ligature-mode

    "w" '(:ignore t :wk "Window")
    "wl" '(:ignore t :wk "Layout")
    "p" '(:ignore t :wk "Projectile")))

(use-package evil
  :demand
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)	; evil-colleciton expects this
  :custom
  ;; (evil-goto-definition-functions) ;; Might be useful
  (evil-want-C-u-scroll t)
  (evil-want-C-w-delete nil) 		; shift windows in insert
  (evil-want-C-w-in-emacs-state t) 	; I don't yank in emacs
  (evil-respect-visual-line-mode t)
  (evil-auto-balance-windows t)	; default but leaving here for reference
  (evil-want-Y-yank-to-eol t)
  :general
  (general-def 'override
    :states '(normal insert visual emacs motion)
    "C-e" 'move-end-of-line
    "C-y" 'universal-argument)
  (my-leader-def
    "ww" 'evil-window-vsplit

    "bN" 'evil-split-next-buffer
    "bP" 'evil-split-prev-buffer
    "bj" 'evil-show-jumps
    "bn" 'evil-next-buffer
    "bp" 'evil-prev-buffer)
  :config
  (define-key universal-argument-map (kbd "C-y") 'universal-argument-more)

  (setq evil-insert-state-cursor '((bar . 2) "LightSkyBlue")
	evil-normal-state-cursor '(box "SlateGrey"))

  ;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (defadvice evil-inner-word (around underscore-as-word activate)
    (let ((table (copy-syntax-table (syntax-table))))
      (modify-syntax-entry ?_ "w" table)
      (with-syntax-table table
	ad-do-it)))

  (evil-mode t))

(use-package evil-collection
  :demand
  :after evil
  :config
  (setq my/evil-collection-disabled-modes '(lispy company))
  (evil-collection-init
   (seq-difference evil-collection--supported-modes my/evil-collection-disabled-modes)))

(use-package evil-surround
  :demand
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-escape
  :demand
  :after evil
  :custom
  (evil-escape-key-sequence "jk")
  :config
  (evil-escape-mode t))

(use-package evil-args
  :demand
  :after evil
  :config
  ;; bind evil-args text objects
  (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))


;;; Builtin configurations

(use-package emacs
  :demand
  :straight nil
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

  ;; (global-visual-line-mode -1)
  ;; don't wrap lines
  (custom-set-variables '(truncate-lines t))

  ;; prevent unsafe dir-locals being pestered for
  ;; https://emacs.stackexchange.com/questions/10983/remember-permission-to-execute-risky-local-variables
  (advice-add 'risky-local-variable-p :override #'ignore)


  (unless (window-system)
    (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?┃))))

;; (use-package fringe
;;   :if (window-system)
;;   :straight nil
;;   :config
;;   (fringe-mode '(4 . 4)))

(use-package font-lock
  :demand
  :straight nil
  :custom
  (font-lock-maximum-decoration t)) ; control amount of fontification, can be done per mode if slow

(use-package help
  :demand
  :straight nil
  :custom
  (help-window-select t))

(use-package window
  :demand
  :straight nil
  :general
  (my-leader-def
    "wk" 'delete-window
    "wo" 'delete-other-windows)
  :config
  ;; Split horizontally when opening a new window from a command
  ;; split-height-threshold nil
  ;; split-width-threshold 170 ; always split vertically if there's room

  (defadvice delete-window (after restore-balance activate)
    "Balance deleted windows."
    (balance-windows))

  ;; recaculate split-width-threshold with every change
  ;; (add-hook 'window-configuration-change-hook
  ;;           'frontside-windowing-adjust-split-width-threshold)

  (defun frontside-windowing-adjust-split-width-threshold ()
    "Change the value of `split-width-threshold' to split once.
For example, if the frame is 360 columns wide, then we want the
`split-width-threshold' to be 181. That way, when you split horizontally,
the two new windows will each be 180 columns wide, and sit just below the threshold."
    (setq split-width-threshold (+ 1 (/ (frame-width) 2)))))


(use-package files
  :demand
  :straight nil
  :general
  (my-leader-def
    "bk" '(my/kill-this-buffer :wk "kill buffer")
    "fk" '(my/delete-file-and-buffer :wk "delete file")
    "fs" 'save-buffer)
  :custom
  (backup-by-copying nil)		    ; slow but sure way of saving if set to t
  (version-control t)		    ; version numbers for backup files
  (save-abbrevs 'silently)
  (backup-directory-alist `(("." . ,user-temporary-file-directory)))
  (delete-old-versions t)

  :config
  ;; https://www.reddit.com/r/emacs/comments/64xb3q/killthisbuffer_sometimes_just_stops_working/
  (defun my/kill-this-buffer ()
    "Kill the current buffer."
    (interactive)
    (kill-buffer (current-buffer)))

  (defun my/delete-file-and-buffer ()
    "Kill the current buffer and deletes the file it is visiting."
    (interactive)
    (let ((filename (buffer-file-name)))
      (when filename
	(if (vc-backend filename)
	    (vc-delete-file filename)
	  (progn
	    (delete-file filename)
	    (message "Deleted file %s" filename)
	    (kill-buffer)))))))

(use-package "startup"
  :demand
  :straight nil
  :custom
  (auto-save-list-file-prefix (concat user-temporary-file-directory ".auto-saves-"))
  (auto-save-file-name-transforms `((".*" ,user-temporary-file-directory t)))
  (inhibit-startup-screen t)
  (initial-buffer-choice (lambda () (switch-to-buffer "*Messages*"))))

(use-package vc-hooks
  :demand
  :straight nil
  :custom
  (vc-follow-symlinks t))

(use-package paren
  :straight nil
  :init
  (setq show-paren-mode t)
  :custom
  ((show-paren-when-point-inside-paren t)
   (show-paren-when-point-in-periphery t))
  :hook (prog-mode . show-paren-mode))

(use-package compile
  :demand
  :straight nil
  :custom
  (compilation-scroll-output t)
  (scroll-conservatively 101)
  :config
  (defun my/postprocess-compilation-buffer ()
    (goto-char compilation-filter-start)
    (when (looking-at "\033c")
      (delete-region (point-min) (match-end 0)))
    (ansi-color-apply-on-region (point) (point-max)))

  (add-hook 'compilation-filter-hook 'my/postprocess-compilation-buffer))

(use-package eldoc
  :hook (prog-mode org-mode)
  :straight nil)

(use-package dired
  :straight nil
  :custom (dired-listing-switches "-Algho --group-directories-first")

  :general
  (my-leader-def
    "od" 'dired-jump)
  (nmap
    :keymaps 'dired-mode-map
    "h" 'dired-single-up-directory
    "l" 'dired-single-buffer
    "L" 'dired-display-file)

  :config
  (use-package dired-single
    :hook (dired-mode . all-the-icons-dired-mode))

  (use-package all-the-icons-dired
    :hook (dired-mode . all-the-icons-dired-mode)))

(use-package "paragraphs"
  :demand
  :straight nil
  :custom
  ;; don't use double spaces in org files to indicate sentence ending - this may clash with other '.'s in words.
  (sentence-end-double-space nil))

(use-package winner
  :straight nil
  :demand
  :config
  (winner-mode 1))

(use-package electric-pair-mode
  :straight nil
  :hook ((prog-mode) . electric-pair-mode))

(use-package xwidget
  :straight nil
  :if (window-system)
  :commands (xwidget-webkit-new-session)
  :config
  (defun my/google-browse ()
    (interactive)
    (browse-url (my/google--get-search-as-url)))

  (defun my/google-xwidget ()
    (interactive)
    (xwidget-webkit-browse-url (my/google--get-search-as-url)))

  (defun my/google--get-search-as-url ()
    (format "https://www.google.co.uk/search?q=%s"
	    (url-hexify-string
	     (s-replace-all '(("  " . " ")) (my/google--get-search)))))

  (defun my/google--get-search ()
    (if (evil-visual-state-p)
	(buffer-substring-no-properties (region-beginning) (region-end))
      (read-string "Search: ")))

  :general
  (nmap
    "gX" 'browse-url-at-point)
  (nmap :keymaps 'org-mode-map
    "gx" 'org-open-at-point
    "gX" 'xwidget-webkit-browse-url)
  (my-leader-def
    "gs" 'my/google-browse
    "gS" 'my/google-xwidget))

(use-package epg-config
  :demand
  ;; a.k.a: gpg epa
  :straight nil
  :custom
  ;; needed this on macos to get private key to be picked up
  (epg-pinentry-mode 'loopback))

(use-package password-cache
  :demand
  :straight nil
  :custom
  ((password-cache-expiry (* 60 60 12))))


;;; Lore friendly improvements

(use-package restart-emacs
  :commands restart-emacs)

(use-package esup
  :disabled) ; doesn't work on v28 yet

(use-package which-key
  :custom
  (which-key-prefix-prefix "")
  :custom-face
  (which-key-group-description-face ((t (:foreground "#8FBCBB"))))
  (which-key-separator-face ((t (:inherit font-lock-comment-face :slant normal))))
  (which-key-note-face ((t (:inherit font-lock-comment-face))))
  :general
  (general-def :states '(normal visual)
    "C-h M" 'which-key-show-major-mode)
  :config
  (which-key-mode t))

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
  :disabled
  :hook
  ((web-mode typescript-mode scala-mode java-mode) . rainbow-delimiters-mode))

(use-package expand-region
  :general
  (general-def :states '(normal visual insert)
    "C-=" 'hydra-expand-region/body)
  :hydra
  (hydra-expand-region (:hint nil)
   "expand-region"
   ("k" er/expand-region "expand")
   ("j" er/contract-region "shrink")))

(use-package undo-tree
  :hook ((prog-mode org-mode) . global-undo-tree-mode)
  :general
  (general-def :states 'normal
    "u" 'undo-tree-undo
    "C-r" 'undo-tree-redo)
  (general-def :states '(normal insert)
    "C-x u" 'hydra-undo/body)
  :hydra
  (hydra-undo
   (:hint nil)
   ("h" undo-tree-undo "undo")
   ("l" undo-tree-redo "redo")
   ("u" undo-tree-visualize "visualise" :color blue)
   ("q" nil "quit" :color blue))
  :custom
  ((undo-tree-auto-save-history t)
   (undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))))

(use-package fic-mode
  :hook (prog-mode)
  :custom
  (fic-highlighted-words '("FIXME" "TODO" "BUG" "XXX" "HACK"))
  :custom-face
  (fic-face ((t (:inherit warning :weight bold)))))

(use-package avy
  :general
  (general-def
    :keymaps 'override
    :states '(normal visual)
    "s" 'avy-goto-word-1
    "S" 'avy-goto-char)
  :custom
  ((avy-background t)))

(use-package ace-window
  :custom
  (aw-ignore-current t)
  (aw-minibuffer-flag t)
  (aw-keys '(?w ?e ?f ?j ?k ?l))
  :general
  (general-def :states '(normal visual insert emacs)
    "C-x o" 'ace-window)
  (my-leader-def
    "W" 'ace-window)
  (my-leader-def
    "wd" 'ace-win-delete
    "ws" 'ace-win-swap)
  :commands
  (ace-win-swap ace-win-delete)
  :custom-face
  ;; (aw-leading-char-face ((t (:inherit error :weight bold))))
  (aw-leading-char-face ((t (:inherit error :weight bold :height 2.0))))
  :config

  (defun ace-win-delete ()
    (interactive)
    (ace-window 16))

  (defun ace-win-swap ()
    (interactive)
    (ace-window 4)))

;; (use-package auto-package-update
;;   :defer 30
;;   :custom
;;   (auto-package-update-delete-old-versions t)
;;   (auto-package-update-interval 1)
;;   (auto-package-update-prompt-before-update t)
;;   :config
;;   (auto-package-update-at-time "09:18"))

(use-package page-break-lines
  :hook (emacs-lisp-mode . page-break-lines-mode))

(use-package reveal-in-osx-finder
  :if (memq window-system '(mac ns))
  :commands (reveal-in-osx-finder))

(use-package nameless
  :hook (emacs-lisp-mode . nameless-mode))

;; (use-package super-save
;;   :custom
;;   (super-save-idle-duration 300) 	; in seconds
;;   (super-save-auto-save-when-idle t)
;;   (super-save-hook-triggers nil)
;;   :config
;;   (setq auto-save-default nil)
;;   (super-save-mode +1))

(use-package my-auto-save
  :disabled 				; this is clashing with
					; prettier, not sure why but
					; it's like it save too many
					; files, or tries to format
					; the wrong thing. Popwin
					; exagerates the issue but
					; it's mainly between prettier
					; and auto-save
  :straight nil
  :load-path "~/emacs/elisp/my-auto-save"
  :after projectile
  :config
  (my-auto-save-mode +1))

(use-package popwin
  :disabled
  :custom
  (popwin:special-display-config
   '(;; Emacs
    ("*Miniedit Help*" :noselect t)
    help-mode
    (completion-list-mode :noselect t)
    (compilation-mode :noselect t)
    (grep-mode :noselect t)
    (occur-mode :noselect t)
    ("*Pp Macroexpand Output*" :noselect t)
    "*Shell Command Output*"
    vterm-mode
    ;; lsp and flycheck
    "*Flycheck errors*"
    " *LV*"
    ;; VC
    ;; "*vc-diff*"
    ;; "*vc-change-log*"
    ;; Undo-Tree
    (" *undo-tree*" :width 60 :position right)
    ;; Anything
    ("^\\*anything.*\\*$" :regexp t)
    ;; SLIME
    "*slime-apropos*"
    "*slime-macroexpansion*"
    "*slime-description*"
    ("*slime-compilation*" :noselect t)
    "*slime-xref*"
    (sldb-mode :stick t)
    slime-repl-mode
    slime-connection-list-mode))
  :config
  (popwin-mode 1))


;;; Evil helpers

(use-package evil-commentary
  :requires evil
  :general
  (general-def :states '(normal visual)
    "gc" 'evil-commentary)
  :config
  (evil-commentary-mode t))

(use-package evil-matchit
  :requires evil
  :general
  (nvmap "%" 'evilmi-jump-items)
  :config
  (global-evil-matchit-mode t))

(use-package elscreen
  :custom
  (elscreen-display-screen-number nil)
  (elscreen-default-buffer-initial-message nil)
  (elscreen-display-tab nil)
  (elscreen-tab-display-kill-screen nil)
  (elscreen-tab-display-control nil)
  (elscreen-prfix-key nil)
  :general
  (my-leader-def
    "we" 'hydra-tabs/body
    "wK" 'elscreen-kill
    "wN" 'elscreen-create
    "wL" 'elscreen-toggle
    "wr" 'elscreen-screen-nickname
    "wt" 'elscreen-toggle-display-tab
    )
  :hydra
  (hydra-tabs
   (:color red :hint nil
	   :pre (setq elscreen-display-tab t)
	   :post (setq elscreen-display-tab nil))
   "tabs"
   ("l" elscreen-next "next")
   ("h" elscreen-previous "previous")
   ("x" elscreen-kill "close window")
   ("j" nil "quit" :color blue)
   ("n" elscreen-create "new" :color blue))
  :config
  (elscreen-start)
  (custom-set-faces
   `(elscreen-tab-background-face ((t (:background ,(doom-color 'bg) :height 1.5))))
   `(elscreen-tab-current-screen-face ((t (:background ,(doom-color 'bg) :foreground ,(doom-color 'yellow) :underline (:color ,(doom-color 'yellow))))))
   `(elscreen-tab-other-screen-face ((t (:background ,(doom-color 'bg) :foreground ,(doom-color 'blue)))))))

(use-package evil-mc
  :general
  (general-def :keymaps 'override
    "C-M-d" 'hydra-mc/body)
  :config
  (global-evil-mc-mode 1)
  :hydra
  (hydra-mc
   (:color red :hint nil)
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
   ("p" evil-mc-make-and-goto-prev-match)))


;;; Terminals

(use-package vterm
  :custom
  (vterm-timer-delay 0.01)
  :hook
  (vterm-mode-hook . (lambda ()
		       (setq-local evil-insert-state-cursor 'box) (evil-insert-state))))

(use-package multi-vterm
  :general
  (my-leader-def
    "tt" 'multi-vterm-project
    "tn" 'multi-vterm)
  :config
  ;; (setq vterm-keymap-exceptions nil)
  (define-key vterm-mode-map [return] #'vterm-send-return))


;;; Themes and Fonts

(use-package solaire-mode
  :config
  (setq solaire-mode-remap-modeline nil))

(use-package doom-themes
  :demand
  :init
  (solaire-global-mode 1)
  :config
  ;; (load-theme 'doom-one t)
  (load-theme 'doom-nord t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  (custom-set-faces
   ;; `(default ((t (:font "FiraCode Nerd Font" :height 160))))
   `(default ((t (:font "FiraCode Nerd Font" :height 120))))
   ;; :height should be a float to adjust the relative size to the normal default font
   `(fixed-pitch ((t (:inherit default :font "FiraCode Nerd Font Mono" :height 0.9))))
   `(variable-pitch ((t (:inherit default :font "Georgia" :height 1.1 :foreground ,(doom-color 'base7)))))

   `(font-lock-function-name-face ((t (:slant italic))))
   `(font-lock-variable-name-face ((t (:weight semi-bold))))
   `(font-lock-comment-face ((t (:slant italic))))))

(use-package all-the-icons)

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 20)
  (doom-modeline-env-version t)
  (doom-modeline-modal-icon t)
  (doom-modeline-vcs-max-length 24)
  ;; doom-modeline-buffer-file-name-style 'truncate-except-project
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  ;; Whether display buffer encoding.
  (doom-modeline-buffer-encoding nil)
  :config
  (setq line-number-mode nil
	size-indication-mode nil))

(use-package ligature
  :straight (ligature :host github :repo "mickeynp/ligature.el")
  :hook (after-init . global-ligature-mode)
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "||=" "||>"
                ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                "?=" "?." "??" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                "\\\\" "://")))

(use-package tree-sitter
  :disabled
  :hook
  ((typescript-mode rustic-mode) . tree-sitter-mode)
  ((typescript-mode rustic-mode) . tree-sitter-hl-mode)
  :commands (my/tree-sitter-hl)
  :config
  (defun my/tree-sitter-hl ()
    "turn on syntax highlighting via tree-sitter"
    (interactive)
    (if (bound-and-true-p tree-sitter-mode)
	(progn
	  (message "turning syntax highlight off")
	  (tree-sitter-hl-mode -1)
	  (tree-sitter-mode -1))
      (progn
	(message "turning syntax highlight on")
	(tree-sitter-mode 't)
	(tree-sitter-hl-mode 't)))))

(use-package tree-sitter-langs
  :after tree-sitter)

;; (use-package tree-sitter-indent
;;   :straight (tree-sitter-indent :host git
;; 				:repo "https://codeberg.org/FelipeLema/tree-sitter-indent.el.git"
;; 				:branch "main"
;; 				:files ("tree-sitter-indent.el"))
;;   :after tree-sitter)

(use-package evil-textobj-tree-sitter
  :disabled   ; some fun stuff in here, will look into this more later
  :after tree-sitter
  :straight (evil-textobj-tree-sitter :type git
				      :host github
				      :repo "meain/evil-textobj-tree-sitter"
				      :files (:defaults "queries"))
  :config
  ;; bind `function.outer`(entire function block) to `f` for use in things like `vaf`, `yaf`
  (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))

  (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "param.outer"
						'((typescript-mode . [(required_parameter) @param.outer]))))
  ;; bind `function.inner`(function block without name and args) to `f` for use in things like `vif`, `yif`
  (define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "param.inner"
						'((typescript-mode . [(required_parameter) @param.inner])))))


;;; Projects / Navigation

(use-package projectile
  :custom
  (projectile-completion-system 'ivy)
  (projectile-indexing-method 'hybrid) ; use git whilst honoring .projectile ignores
  (projectile-globally-ignored-directories '(".idea" ".vscode" ".ensime_cache" ".eunit"
					     ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr"
					     "_darcs" ".tox" ".svn" ".stack-work"
					     ".ccls-cache" ".cache" ".clangd"
					     ".yarn/cache"))
  :init
  (if (file-directory-p "~/sky")
      (setq projectile-project-search-path '("~/dev" "~/sky"))
    (setq projectile-project-search-path '("~/dev")))

  :general
  (general-def :states '(normal visual)
    "gf" 'projectile-find-file-dwim)
  (my-leader-def
    "SPC" '(projectile-find-file :wk "find-file")
    "be" '(projectile-ibuffer :wk "edit buffers")
    "fS" '(projectile-save-project-buffers :wk "save buffers")

    "p!" '(projectile-run-shell-command-in-root :wk "run-command-in-root")
    "pa" '(projectile-add-known-project :wk "add-known-project")
    "pb" '(projectile-switch-to-buffer :wk "switch-to-buffer")
    "pc" '(projectile-compile-project :wk "compile")
    "pd" '(projectile-find-dir :wk "find-dir")
    "pe" '(projectile-edit-dir-locals :wk "edit-dir-locals")
    "pf" '(projectile-find-file :wk "find-file")
    "pF" '(projectile-find-file-in-known-projects :wk "find-file-in-known-projects")
    "pi" '(projectile-invalidate-cache :wk "invalidate-cache")
    "pk" '(projectile-kill-buffers :wk "kill-buffers")
    "po" '(projectile-toggle-between-implementation-and-test :wk "toggle src / test")
    "pO" '(projectile-find-other-file :wk "find-other-file")
    "pp" '(projectile-switch-project :wk "switch")
    ;; "pt" 'my/test-file					;; test file in project
    "pr" '(projectile-recentf :wk "recentf")
    "pR" '(projectile-regenerate-tags :wk "regenerate-tags"))

  :config
  (projectile-mode)

  (projectile-register-project-type 'yarn '("yarn.lock")
				    :project-file "package.json"
				    :install "yarn install"
				    :compile "yarn build"
				    :test "yarn test"
				    :run "yarn start"
				    :test-suffix ".spec")

  (projectile-register-project-type 'npm '("package-lock.json")
				    :compile "npm i"
				    :test "npm test"
				    :run "npm start"
				    :test-suffix "_test")

  (projectile-register-project-type 'idris '("build.gradle" "yarn.lock")
				    :project-file "package.json"
				    :install "yarn install"
				    :compile "yarn build"
				    :test "yarn test"
				    :run "yarn start"
				    :test-suffix ".test"))

;; (use-package my-projectile-fns
;;   :after projectile
;;   :load-path "~/.emacs.d/elisp"
;;   :general
;;   (my-leader-def
;;     "pg" 'my/goto-github
;;     "bb" '(my/split-last-buffer :wk "split last buffer")))

(use-package counsel-projectile
  :after (projectile counsel)
  :custom ((counsel-projectile-org-capture-templates
	    '(("t" "[${name}] Task" entry
	       (file+headline org-default-notes-file "Tasks")
	       "* TODO %?\n  %u\n  %a"))))
  :general
  (general-def :keymaps 'override
    "C-\\" '(counsel-projectile-rg :wk "ripgrep"))

  (my-leader-def
    "nc" '(counsel-projectile-org-capture :wk "capture")
    "np" '(counsel-projectile-org-agenda :wk "agenda")
    "bl" '(counsel-projectile-switch-to-buffer :wk "project buffers")
    "pn" '(counsel-projectile-org-agenda :wk "agenda"))
  :config
  (counsel-projectile-mode)
  (setcar counsel-projectile-switch-project-action 2))

(use-package magit
  :commands (magit-blame magit-log-buffer-file)
  :general
  (my-leader-def
    "gg" 'magit-status))

(use-package git-gutter
  :custom
  (git-gutter:visual-line t)
  (git-gutter:hide-gutter t)
  :general
  (my-leader-def
    "G" '(hydra-git/body :wk "git-gutter"))
  :hydra
  (hydra-git
   (:hint nil :post my/gutter-close-git-diff)
   "git-gutter"
   ("q" nil "quit" :color blue)
   ("r" git-gutter:update-all-windows "refresh")
   ("j" git-gutter:next-hunk "next")
   ("k" git-gutter:previous-hunk "previous")
   ("d" git-gutter:popup-hunk "diff")
   ("x" git-gutter:revert-hunk "revert hunk"))
  :config
  (global-git-gutter-mode 1)
  (defun my/gutter-close-git-diff ()
    "Close git-gutter diff if open"
    (interactive)
    (when (-contains? (window-list) (git-gutter:popup-buffer-window))
      (delete-window (git-gutter:popup-buffer-window)))))

(use-package git-gutter-fringe
  :if window-system
  :after git-gutter
  :demand
  :init
  ;; turn off git-gutter
  (global-git-gutter-mode 0)
  :config
  ;; now enable the git-gutter from fringe mode
  (global-git-gutter-mode 1)

  (dolist (icon '(git-gutter-fr:added git-gutter-fr:deleted git-gutter-fr:modified))
    (fringe-helper-define icon nil
      "XX....."
      "XX....."
      "XX....."
      "XX....."
      "XX....."
      "XX....."
      "XX....."
      "XX....."
      "XX....."
      "XX....."
      "XX....."
      "XX....."
      "XX....."
      "XX....."
      "XX....."))

  (custom-set-faces
   `(git-gutter-fr:added	((t (:foreground ,(doom-color 'teal)))))
   `(git-gutter-fr:deleted	((t (:foreground ,(doom-color 'orange)))))
   `(git-gutter-fr:modified	((t (:foreground ,(doom-color 'base6)))))))

(use-package forge
  :after magit
  :config
  (setq auth-sources '("~/.authinfo")))

(use-package wakatime-mode
  :demand
  :after projectile
  :config
  (setq wakatime-cli-path
	(s-concat (s-trim (shell-command-to-string "brew --prefix")) "/bin/wakatime-cli"))
  (global-wakatime-mode))


;;; Narrowing / Searching / Lists

(use-package ivy ;; TODO do i need these bindings and evil collection?
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-wrap t)
  (ivy-count-format "(%d/%d) ")
  (ivy-initial-inputs-alist nil)      ; Don't start searches with ^
  (ivy-extra-directories ())	       ; hide . and .. from file lists
  :config
  (ivy-mode 1)

  (setq ivy-re-builders-alist
	'((counsel-projectile-rg . ivy--regex-plus)
	  (swiper . ivy--regex-plus)	; fzy search in file is clumsy
	  (swiper-all . ivy--regex-plus)	; fzy search in file is clumsy
	  (t . ivy--regex-fuzzy)))

  (setq enable-recursive-minibuffers t)

  :general
  (my-leader-def
    "sf" 'swiper

    "wlw" 'ivy-push-view
    "wld" 'ivy-pop-view
    "wll" 'ivy-switch-view)
  (general-def :keymaps 'override
    "C-s" 'swiper)
  (general-def :keymaps '(ivy-minibuffer-map)
    "C-SPC" 'ivy-restrict-to-matches
    "C-l" 'ivy-alt-done
    "C-j" 'ivy-next-line
    "C-k" 'ivy-previous-line
    "C-u" 'ivy-scroll-down-command
    "C-d" 'ivy-scroll-up-command))

(use-package ivy-rich
  :after ivy
  :config
  (ivy-rich-mode 1))

(use-package ivy-xref
  :after ivy)

(use-package flx
  :after ivy)

(use-package ivy-xref
  :after ivy
  :init
  (setq xref-show-definitions-function #'ivy-xref-show-defs))

(use-package counsel
  ;; :bind (:map minibuffer-local-map
  ;; 	 ("C-r" . 'counsel-minibuffer-history))
  :general
  (general-def :keymaps 'override
    "M-x" 'counsel-M-x
    "C-x b" 'counsel-ibuffer
    "C-x B" 'counsel-ibuffer
    "C-x C-f" 'counsel-find-file)

  (my-leader-def
    ";" '(counsel-M-x :wk "M-x")
    ;; ":" '(counsel-command-history :wk "command history")
    ":" 'eval-expression
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "fo" 'counsel-outline

    "bL" '(counsel-switch-buffer :wk "global buffers")
    "si" 'counsel-imenu
    "gl" 'counsel-find-library)

  ;; (my-local-leader-def :keymaps 'org-mode-map
  ;;   "gh" '(counsel-org-goto-all :wk "find heading")
  ;;   "gH" '(counsel-org-goto :wk "find heading in all"))
  :custom
  (counsel-find-file-ignore-regexp "(.|..|.elc)"))

(use-package smex
  :after counsel)

(use-package deadgrep
  :commands deadgrep
  :general
  ;; might want deadgrep-kill-all-buffers in a function
  (general-def :keymaps 'deadgrep-mode-map
    "C-j" 'deadgrep-forward-filename
    "C-k" 'deadgrep-backward-filename)
  (my-leader-def
    "sp" 'deadgrep
    "ps" 'deadgrep))


;;; LSP and Completion

(use-package yasnippet
  :bind
  ("C-c y s" . yas-insert-snippet)
  ("C-c y v" . yas-visit-snippet-file)
  :config
  ;; http://andreacrotti.github.io/yasnippet-snippets/snippets.html
  (use-package yasnippet-snippets)
  (yas-global-mode 1))

(use-package company
  :hook
  (prog-mode . company-mode)
  (org-mode . company-mode)
  :general
  (imap 'override
    "C-@"   'company-complete
    "C-SPC" 'company-complete)
  (general-def :keymaps 'company-active-map
    "C-SPC" 'company-complete-common
    "C-l" 'company-complete-selection
    "C-j" 'company-select-next
    "C-k" 'company-select-previous
    "C-u" 'company-previous-page
    "C-d" 'company-next-page)
  :custom
  (company-tooltip-align-annotations t)
  (company-minimum-prefix-length 3)
  ;; (company-minimum-prefix-length 10000)
  ;; (company-idle-delay 0.0)
  (company-idle-delay 0.2))

(use-package company-box
  :hook (company-mode . company-box-mode))

(general-nmap
  "gh" (general-predicate-dispatch 'helpful-at-point
	 ;; if lsp is active
	 (bound-and-true-p lsp-mode) 'lsp-describe-thing-at-point))


(use-package lsp-mode
  :commands lsp
  :init
  ;; get from https://github.com/elixir-lsp/elixir-ls/releases
  (add-to-list 'exec-path "~/.tooling/elixir-ls-1.11/")
  :hook
  ((web-mode typescript-mode typescript-tsx-mode scala-mode java-mode elixir-mode) . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :custom
  ;; general
  (lsp-auto-execute-action '())
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-disabled-clients '((json-mode . eslint)))

  ;; optimisations that may improve as lsp matures

  (lsp-log-io nil)
  (lsp-clients-typescript-log-verbosity nil)

  ;; (lsp-completion-enable-additional-text-edit nil)
  ;; (lsp-completion-show-detail nil) 	; i think this is help
  ;; (lsp-completion-show-kind nil) ; variable or function stuff in autocomplete
  (lsp-enable-file-watchers '())

  ;; rust
  (lsp-rust-server 'rust-analyzer)
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-parameter-hints t)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-cargo-watch-command "clippy")

  :config
  ;; (add-hook 'typescript-mode #'my/lsp-limit-lsp)
  ;; (add-hook 'web-mode #'my/lsp-limit-lsp)

  ;; this gets a nice speed boost, but makes the experience a bit worse
  (defun my/lsp-limit-lsp ()
    "Limit some of the steps lsp will do with typescript lsp responses, as they are huge"
    (setq lsp-completion-enable-additional-text-edit nil)
    (setq lsp-completion-show-detail nil)	; i think this is help
    (setq lsp-completion-show-kind nil) ; variable or function stuff in autocomplete
    )

  
  (defun lsp-js-ts-rename-file ()
    "Rename current file and all it's references in other files."
    (interactive)
    (let* ((name (buffer-name))
           (old (buffer-file-name))
           (basename (file-name-nondirectory old)))
      (unless (and old (file-exists-p old))
	(error "Buffer '%s' is not visiting a file." name))
      (let ((new (read-file-name "New name: " (file-name-directory old) basename nil basename)))
	(when (get-file-buffer new)
          (error "A buffer named '%s' already exists." new))
	(when (file-exists-p new)
          (error "A file named '%s' already exists." new))
	(lsp--send-execute-command
	 "_typescript.applyRenameFile"
	 (vector (list :sourceUri (lsp--buffer-uri)
                       :targetUri (lsp--path-to-uri new))))
	(mkdir (file-name-directory new) t)
	(rename-file old new)
	(rename-buffer new)
	(set-visited-file-name new)
	(set-buffer-modified-p nil)
	(lsp-disconnect)
	(setq-local lsp-buffer-uri nil)
	(lsp)
	(lsp--info "Renamed '%s' to '%s'." name (file-name-nondirectory new)))))
  )

(use-package lsp-ui
  :commands (lsp-ui-imenu)
  :hook ((rustic-mode lsp-mode) . lsp-ui-mode)
  :bind ("C-c C-k" . my/lsp-ui-doc-glance) ; just a test binding
  :config
  (defun my/lsp-ui-doc--glance-hide-frame ()
    "Hook to hide hover information popup for lsp-ui-doc-glance."
    (lsp-ui-doc-hide)
    (remove-hook 'pre-command-hook 'lsp-ui-doc--glance-hide-frame))

  (defun my/lsp-ui-doc-glance ()
    "Trigger display hover information popup and hide it on next typing."
    (interactive)
    (lsp-ui-doc-show)
    (add-hook 'pre-command-hook 'lsp-ui-doc--glance-hide-frame))

  ;; temp while trying out rust
  (add-hook 'rustic-mode-hook
	    (lambda ()
	      (make-local-variable 'lsp-ui-sideline-enable)
	      (setq lsp-ui-sideline-enable t)))
  :custom
  (lsp-ui-doc-header '())		; looks shit
  (lsp-ui-sideline-enable '())		; bloody overwhelming
  (lsp-ui-doc-use-webkit '()) ; don't be tempted by this, unless you really want to configure it
  (lsp-ui-doc-include-signature '()) ; eldoc does a better job of this
  (lsp-ui-doc-show-with-cursor '())  ; use keybinding instead or mouse
  (lsp-ui-imenu--custom-mode-line-format "lsp-ui-menu")
  (lsp-ui-doc-border "brightblack")
  (lsp-ui-doc-position 'at-point))

(use-package dap-mode
  :disabled
  :commands dap-debug 			; might not need this, wory about this when setting up
  :after lsp-mode
  :config (dap-auto-configure-mode))


;;; Programming Languages

;;;; Javascript / Typescript / Web

(use-package add-node-modules-path
  :hook
  (web-mode)
  (typescript-tsx-mode)
  (typescript-mode)
  (css-mode))

(use-package js2-mode
  :general
  (general-def :states 'insert :keymaps 'js2-minor-mode-map
    "RET" 'js2-line-break)
  :custom
  (js-chain-indent t)
  (js-indent-level 2)
  ;(js2-highlight-level 3)
  ;; Don't use built-in syntax checking
  (js2-mode-show-strict-warnings nil)
  (js2-mode-show-parse-errors nil)
  (js2-strict-trailing-comma-warning nil)
  (js2-strict-missing-semi-warning nil)
  (js2-strict-inconsistent-return-warning nil)
  (js2-strict-cond-assign-warning nil)
  (js2-strict-var-redeclaration-warning nil)
  :hook
  ((web-mode typescript-mode) . js2-minor-mode))

(use-package js2-refactor
  :disabled
  :hook ((web-mode typescript-mode) . js2-refactor-mode))

(use-package typescript-mode
  :interpreter "node"
  :mode "\\.ts\\'"
  :mode "\\.js\\'"
  :hook ((typescript-mode) . my/ts-mode-settings)
  :custom
  (typescript-indent-level 2)
  :config
  (defun my/ts-mode-settings ()
    "Hook for ts mode."
    ;; (setq flycheck-checker 'javascript-eslint)
    (flycheck-add-next-checker 'lsp 'javascript-eslint)))

;; (use-package typescript-tsx-mode
;;   :load-path "~/emacs/elisp/typescript-tsx-mode"
;;   :disabled
;;   :straight nil
;;   :hook ((typescript-tsx-mode) . my/tsx-mode-settings)
;;   :config
;;   (defun my/tsx-mode-settings ()
;;     "Hook for ts mode."
;;     (flycheck-add-mode 'javascript-eslint 'typescript-tsx-mode)
;;     (flycheck-add-next-checker 'lsp 'javascript-eslint)))

(use-package web-mode
  ;; still need web-mode stuff as typescript-tsx-mode is actually derived from it
  :mode "\\.ejs\\'"
  :mode "\\.tsx\\'"
  :mode "\\.jsx\\'"
  :hook ((web-mode-hook) . my/web-mode-settings)
  :config
  (setq-default web-mode-comment-formats
		'(("javascript" . "//")))

  (defun my/web-mode-settings ()
    "Hooks for Web mode."
    (interactive)
    (flycheck-add-mode 'javascript-eslint 'web-mode)
    (flycheck-add-next-checker 'lsp 'javascript-eslint)
    ;; (flycheck-add-mode 'css-stylelint 'web-mode)
    ;; (flycheck-add-next-checker 'javascript-eslint 'css-stylelint)

    (setq web-mode-code-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-enable-auto-quoting nil)))

(use-package jest
  :hook ((web-mode typescript-mode typescript-tsx-mode) . jest-minor-mode)
  :custom
  (jest-executable "yarn test --no-coverage --color --maxWorkers=1")
  (jest-unsaved-buffers-behavior 'save-current)
  :general
  (general-define-key
   :prefix "SPC"
   :states 'normal
   :keymaps '(typescript-mode-map web-mode-map)
   "jj" 'jest-function
   "jl" 'jest-repeat
   ;; file test needs creating as a lambda
   "jf" 'jest-file-dwim
   "jp" 'jest-popup))


(use-package prettier-js
  :custom
  ;; this is cool but can't get it to respect .prettierrc, and it's pretty fast
  ;; (prettier-js-command "prettier_d_slim")
  (prettier-js-show-errors "None")
  :config
  :hook ((web-mode typescript-mode typescript-tsx-mode json-mode) . prettier-js-mode))

;;;; Scala

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package lsp-metals
  :disabled
  :after scala-mode
  :custom
  (lsp-metals-treeview-show-when-views-received nil)
  :config
  ;; (lsp-metals-sbt-script)
  )

(use-package sbt-mode
  :commands sbt-start sbt-command
  :after scala-mode
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

;;;; Java

(use-package lsp-java
  :disabled
  :after (java-mode))

(use-package dap-java
  :disabled
  :straight nil)

(use-package groovy-mode)

;;;; lisp

(use-package elisp-mode
  :straight nil
  :hook (emacs-lisp-mode . (lambda () (when (fboundp 'electric-pair-mode)
					(electric-pair-mode -1))))
  :config
  (defun my/elisp-fold ()
    (interactive)
    (set-selective-display 2))

  (defun my/elisp-unfold ()
    (interactive)
    (set-selective-display 0))

  (defun my/insert-page-break ()
    (interactive)
    (insert-and-inherit "\C-\l"))

  :general
  (my-local-leader-def :keymaps 'emacs-lisp-mode-map
    "p" '(:wk "page-break" :def my/insert-page-break)
    "f" '(:wk "fold" :def my/elisp-fold)
    "F" '(:wk "unFold" :def my/elisp-unfold)))

(use-package lispy
  :hook ((emacs-lisp-mode lisp-data-mode) . lispy-mode))

(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(commentary ; comments on gc
     c-w	; C-w deletes backword word, so I should get used that
     ;; mark		 ; look into this another day
     prettify	 ; make == and the like work
     atom-motions ; make w and e respect atoms (e.g foo-bar is an atom)
     slurp/barf-cp
     additional ; binds a bunch of things to alt https://github.com/noctuid/lispyville#additional-key-theme
     additional-motions ; I mainly like this for so [ ] can act like 'd'
     additional-insert	; make M-[oO] M-[iI] smarter
     operators))

  (defun endless/sharp ()
    "Insert #' unless in a string or comment."
    (interactive)
    (call-interactively #'self-insert-command)
    (let ((ppss (syntax-ppss)))
      (unless (or (elt ppss 3)
		  (elt ppss 4)
		  (eq (char-after) ?'))
	(insert "'"))))

  (define-key emacs-lisp-mode-map "#" #'endless/sharp))

(use-package paren-face
  :hook (lispy-mode . paren-face-mode))

;;;; Others

(use-package json-mode
  :mode "\\.json\\'"
  :config
  ;; TODO tidy this, as I imagine it runs every init
  (defconst json-mode-comments-re (rx (group "//" (zero-or-more nonl) line-end)))
  (push (list json-mode-comments-re 1 font-lock-comment-face) json-font-lock-keywords-1))

(use-package yaml-mode)

(use-package graphql-mode)

(use-package dotenv-mode
  :mode "\\.env\\..*\\'"
  :hook (dotenv-mode . (lambda ()
              (set (make-local-variable 'comment-start) "# ")
              (set (make-local-variable 'comment-end) ""))))

(use-package rustic
  :config
  ;; (add-hook 'rustic-mode-hook 'my/rustic-mode-hook)
  ;; (remove-hook 'rustic-mode-hook 'my/rustic-mode-hook)
  (defun my/rustic-mode-hook ()
    ;; so that run C-c C-c C-r (lsp-rename) works without having to confirm, but don't try to
    ;; save rust buffers that are not file visiting. Once
    ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
    ;; no longer be necessary.
    (when buffer-file-name
      (setq-local buffer-save-without-query t)))

  ;; (defun my/rustic-expand-macro ()
  ;;   (interactive)
  ;;   (let ((current rustic-format-trigger))
  ;;     (setq rustic-format-trigger 'nil)
  ;;     (lsp-rust-analyzer-expand-macro)
  ;;     (setq rustic-format-trigger current)))
  :custom (rustic-indent-offset 4)
  :general
  ;; (my-leader-def
  ;;   "jj" 'rustic-cargo-current-test
  ;;   "jl" 'rustic-cargo-test-rerun
  ;;   ;; file test needs creating as a lambda
  ;;   "jf" 'rustic-cargo-current-test
  ;;   "jp" 'rustic-cargo-test

  ;;   "pt" 'rustic-cargo-test
  ;;   "pc" 'rustic-cargo-build)

  ;; might be better as a hydra
  (general-define-key
   :prefix "SPC"
   :states 'normal
   :keymaps 'rustic-mode-map
   "jj" 'rustic-cargo-current-test
   "jl" 'rustic-cargo-test-rerun
   ;; file test needs creating as a lambda
   "jf" 'rustic-cargo-current-test
   "jp" 'rustic-cargo-test

   "eN" 'next-error
   "eP" 'previous-error

   "pt" 'rustic-cargo-test
   "pc" 'rustic-cargo-build)
  

  :custom
  (rustic-format-trigger 'on-save))

(use-package just-mode)
(use-package lua-mode)
(use-package vimrc-mode)

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package docker
  :general
  (my-leader-def
    "gd" 'docker))

(use-package elixir-mode
  :mode ("\\.ex\\'" "\\.exs\\'" "\\.elixir\\'")
  :config
  ;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
  (add-hook 'elixir-mode-hook
            (lambda () (add-hook 'before-save-hook 'elixir-format nil t))))

(use-package mix
  :hook (elixir-mode . mix-minor-mode)
  ;; :general
  ;; (my-leader-def
  ;;   "pt" 'my/elixir-test-file)
  :config
  (defun my/elixir-test-file ()
    "save buffer and run mix test file"
    (interactive)
    (progn
      (save-buffer)
      (mix-test-current-buffer))))

(use-package csharp-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.cs\\" . csharp-tree-sitter-mode)))

;; (use-package lsp-python-ms
;;   :disabled
;;   :init (setq lsp-python-ms-auto-install-server t)
;;   :hook (python-mode . (lambda ()
;;                          (require 'lsp-python-ms)
;;                          (lsp))))

(use-package flycheck
  :commands (my|eslint-fix-file-and-revert)
  :custom
  (flycheck-indication-mode 'right-fringe)
  ;; (flycheck-check-syntax-automatically '(idle-buffer-switch save mode-enabled))
  (flycheck-check-syntax-automatically '(idle-buffer-switch new-line mode-enabled idle-change))
  (flycheck-javascript-eslint-executable "eslint_d")
  :hook (prog-mode . flycheck-mode)
  :config
  ;; simple clean line for flycheck errors in fringe
  ;; arrows all look pixelated, and I haven't worked out how to use hidpi verison
  (define-fringe-bitmap 'flycheck-fringe-bitmap-double-arrow
    [#b000000000
     #b111111111
     #b111111111
     #b111111111
     #b111111111
     #b000000000])

  (defun my|eslint-fix-file ()
    "Run eslint --fix on current file."
    (interactive)
    (message (concat "eslint --fixing " (buffer-file-name) " using"))
    (save-buffer)
    (shell-command
     (concat "cd " (projectile-project-root) " && node_modules/eslint/bin/eslint.js"
	     (cond ((file-exists-p "./.eslintrc.js") " --config ./.eslintrc.js")
		   ((file-exists-p "./.eslintrc.yml") " --config ./.eslintrc.yml"))
	     " --fix " (buffer-file-name))))

  (defun my|eslint-fix-file-and-revert ()
    (interactive)
    (my|eslint-fix-file)
    (revert-buffer t t))

  :general
  (my-leader-def
    "ef" 'my/eslint-fix-file
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error
    "el" 'flycheck-list-errors
    "ee" 'flycheck-buffer
    "eh" 'flycheck-explain-error-at-point ;; not sure?
    "ei" 'flycheck-verify-setup))

(use-package editorconfig
  :hook (prog-mode . editorconfig-mode))


;;; Writing

(use-package flyspell
  :straight nil
  :ensure-system-package aspell
  :hook ((markdown-mode org-mode) . flyspell-mode)
  :general
  (my-leader-def
    "EE" 'flyspell-correct-wrapper
    "ES" 'my/my-save-word
    "EB" 'flyspell-buffer
    "EP" 'evil-prev-flyspell-error
    "EN" 'evil-next-flyspell-error)

  :config
  (setq ispell-program-name
	(concat (string-trim (shell-command-to-string "brew --prefix")) "/bin/aspell"))

  (defun my/my-save-word ()
    (interactive)
    (let ((current-location (point))
	  (word (flyspell-get-word)))
      (when (consp word)
	(flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location)))))

(use-package flyspell-correct-ivy
  :after (:all (flyspell ivy)))

(use-package markdown-mode
  :hook
  ;; (markdown-mode . visual-line-mode) ; might need this, will see
  (markdown-mode . flyspell-mode)
  (markdown-mode . abbrev-mode)
  (markdown-mode . variable-pitch-mode)
  (markdown-mode . (lambda () (setq line-spacing 0.2))) ; bit more breathing room for notes
  :custom
  (markdown-hide-markup t)
  :general
  (nmap :keymaps 'markdown-mode-map
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)

  (my-local-leader-def :keymaps 'markdown-mode-map
    "c" 'markdown-toggle-markup-hiding))

(use-package markdown-toc
  :after markdown-mode)

(use-package hide-mode-line
  :hook
  ((org-mode vterm-mode term-mode org-roam-mode) . hide-mode-line-mode))

(use-package olivetti
  :custom
  (olivetti-body-width 80)
  (olivetti-mode-on-hook (visual-line-mode -1))
  :hook ((markdown-mode org-mode) . olivetti-mode))


;;; Org mode and related

(use-package org
  :init
  (setq org-directory "~/Dropbox/roam")
  (setq org-default-notes-file (expand-file-name "~/Dropbox/roam/20210614152805-dump.org"))
  :hook
  (org-mode . auto-fill-mode)
  (org-mode . flyspell-mode)
  (org-mode . abbrev-mode)
  (org-mode . variable-pitch-mode)
  (org-mode . (lambda () (setq line-spacing 0.2))) ; bit more breathing room for notes

  :custom
  (org-src-lang-modes '(("C" . c)
			("cpp" . c++)
			("bash" . sh)
			("sh" . sh)
			("elisp" . emacs-lisp)))

  ;; adding roam files does have the consequence of loading all buffers into memory, which could get out of hand
  (org-agenda-files (-non-nil (-list "~/Dropbox/roam"
				     "~/Dropbox/org-me-notes/notes.org"
				     (when (file-directory-p "~/OneDrive - Sky")
				       (expand-file-name "~/OneDrive - Sky/dev/org-sky-notes/work.org")))))

  ;; Not sure I fully understand what I've configured here but intention was to remove the filenames from agenda view
  (org-agenda-prefix-format '((agenda . "  %?-12t% s")
			      (timeline . " % s")
			      (todo . " %i")
			      (tags . " % s")
			      (search . " % s")))
  (org-startup-indented t)
  (org-fontify-done-headline t)
  (org-fontify-todo-headline t)
  (org-fontify-whole-heading-line t)
  (org-src-fontify-natively t)
  (org-fontify-quote-and-verse-blocks t)

  (org-confirm-babel-evaluate nil)

  (org-hide-leading-stars t)
  (org-hide-emphasis-markers t)

  (org-hide-block-startup nil)
  (org-startup-folded nil)

  (org-log-done 'time)
  (org-ellipsis " ▾")
  (org-image-actual-width nil) ; allows images to be resized with #+ATTR_ORG: :width 100
  (org-indirect-buffer-display 'current-window)
  (org-enforce-todo-dependencies t)
  (org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))
  (org-agenda-span 8)
  (org-todo-keywords '((sequence "TODO(t)" "PROGRESS(p)" "BLOCKED(b)" "|" "DONE(d)" "ARCHIVED(a)")))

  (org-capture-templates
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

  (org-agenda-custom-commands
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

  :general
  (nmap :keymaps 'org-mode-map
    ">" 'org-shiftmetaright
    "<" 'org-shiftmetaleft
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)
  (my-local-leader-def :keymaps 'org-mode-map
    "c" 'org-toggle-checkbox
    "l" '(:ignore t :wk "List")
    "lt" 'my/org-toggle-list-checkbox
    "ls" 'org-sort-list
    "g" 'org-open-at-point
    "h" '(:ignore t :wk "Heading")
    "hh" 'org-toggle-heading
    "ho" 'evil-org-insert-heading-below
    "hn" 'org-insert-heading-respect-content
    "hs" 'org-insert-subheading
    "d" '(:ignore t :wk "Delete")
    "dr" 'org-table-kill-row
    "dc" 'org-table-delete-column
    "i" '(:ignore t :wk "Insert")
    "ii" 'org-roam-insert
    "ic" 'org-table-insert-column
    "i-" 'org-table-insert-hline
    "s" 'org-sort
    "t" '(:ignore t :wk "Toggle")
    "ti" 'org-toggle-inline-images
    "tl" 'org-toggle-link-display)
  (my-leader-def
    "na" 'org-agenda
    "nb" 'org-switchb
    "nl" 'org-store-link)

  :config
  ;; read gpg encrypted files
  (unless (string-match-p "\\.gpg" org-agenda-file-regexp)
    (setq org-agenda-file-regexp
	  (replace-regexp-in-string "\\\\\\.org" "\\\\.org\\\\(\\\\.gpg\\\\)?"
				    org-agenda-file-regexp)))

  (use-package ob-typescript)
  (use-package ob-restclient)

  (org-babel-do-load-languages 'org-babel-load-languages '((shell . t) ; allow bash
							   (js . t)
							   (typescript . t)
							   (restclient . t)
							   (haskell . t)
							   (ruby . t)))
  
  ;; TODO add pretty bullets
  ;; (font-lock-add-keywords 'org-mode
  ;;                         '(("^ *\\([-]\\) "
  ;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  (require 'org-tempo) ;; needed to add this to get template expansion to work again

  ;; can't seem to get this to behave without nesting here with demand on
  (use-package org-download
    ;; drag and drop to dired
    :demand
    :custom (org-download-image-org-width 500)
    :hook (dired-mode-hook . org-download-enable))

  ;; Turn off elisp's flycheck checkdoc in src blocks.
  (add-hook 'org-src-mode-hook (lambda () (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))))

(use-package org-bullets
  :if window-system
  :commands org-bullets-mode
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package org-roam
  :if (file-directory-p "~/Dropbox/roam")
  :init
  (setq org-roam-v2-ack t)
  ;; :after org
  :custom
  (org-roam-directory (file-truename "~/Dropbox/roam/"))
  (org-roam-db-update-method 'idle-timer)
  (org-roam-encrypt-files t)
  (org-roam-completion-everywhere t) 	; allow completion for inserting node links
  (org-roam-capture-templates
   '(("d" "default" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org" "#+title: ${title}\n\n")
      :unnarrowed t)
     ("s" "secure" plain
      "%?"
      :if-new (file+head "%<%Y%m%d%H%M%S>-${slug}.org.gpg" "#+title: ${title}\n\n")
      :unnarrowed t)))
  :config
  (org-roam-setup)
  :general
  (my-leader-def
    "nl" 'org-roam-buffer-toggle
    "nf" 'org-roam-node-find
    "ng" 'org-roam-graph
    "NN" 'org-roam-capture
    "ni" 'org-roam-node-insert)
  :bind (:map org-roam-mode-map
	      (("C-c n l" . org-roam-buffer-toggle)
	       ("C-c n f" . org-roam-node-find)
	       ("C-c n g" . org-roam-graph))
	      :map org-mode-map
	      (("C-c n i" . org-roam-node-insert))))

(use-package ob-js
  :straight nil
  :after org
  :config
  ;; overwrite default wrapper function which doesn't work
  (setq org-babel-js-function-wrapper "(function() {
    require('util').inspect((function(){\n%s\n})())
  })()"))

(use-package evil-org
  :after org
  :demand
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package org-alert
  :disabled
  :straight (org-alert :local-repo "~/emacs/elisp/org-alert")
  :defer 30
  :config
  (org-alert-enable))

(use-package my-org-helpers
  :straight (my-org-helpers :local-repo "~/emacs/elisp/my-org-helpers")
  :after (:any org org-roam)
  :demand t
  :general
  (my-leader-def
    "nn" 'my/open-my-notes-file
    "nN" 'my/open-work-notes-file)
  :config
  (my/org-theme))

(use-package my-markdown-helpers
  :straight (my-markdown-helpers :local-repo "~/emacs/elisp/my-markdown-helpers")
  :after (markdown-mode)
  :config
  (my/markdown-theme))

;; watch out for performance issues here
(use-package emojify
  :disabled
  :hook ((markdown-mode org-mode) . emojify-mode))


;;; Own scripts and packages

;; (use-package my-gui-controls
;;   :if (window-system)
;;   :straight (my-gui-controls :local-repo "~/emacs/elisp/my-gui-controls")
;;   :general
;;   (general-def :keymaps 'override
;;     "C-s-<f8>" 'my/close-notifications-mac
;;     "C-M-<left>" 'frame-half-size-left
;;     "C-M-<right>" 'frame-half-size-right
;;     "C-M-<return>" 'toggle-frame-maximized))

;; reset gc to something sensible for normal operation
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
