;;; init.el --- emacs config focusing on a lagless experience -*- no-byte-compile: t; lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Package Manager setup --- inital setup of package.el and use-package (plus complimentary packages)

(require 'package)

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
	("org" . "https://orgmode.org/elpa/")
	("elpa" . "https://elpa.gnu.org/packages/")))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-verbose t)

(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package use-package-ensure-system-package
  :ensure t)


;;; Initial packages --- these must load before everything

(use-package cus-edit
  :demand
  :ensure nil
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

(use-package general
  :demand
  :config
  (general-evil-setup t)

  (general-create-definer my-leader-def
    :prefix "SPC"
    :states '(normal visual))
  
  (general-create-definer my-local-leader-def
    :prefix ","
    :states '(normal visual))

  (my-leader-def
    "b" '(:ignore t :wk "Buffers")
    "bK" 'kill-buffer
    "br" 'rename-buffer

    "e" '(:ignore t :wk "Errors")
    "E" '(:ignore t :wk "Spelling")
    "f" '(:ignore t :wk "Files")
    "g" '(:ignore t :wk "Global")
    "n" '(:ignore t :wk "Notes")
    "p" '(:ignore t :wk "Projects")
    "s" '(:ignore t :wk "Search")
    "t" '(:ignore t :wk "Term")

    "T" '(:ignore t :wk "Toggle")
    "TT" 'toggle-truncate-lines
    "Ti" 'lsp-ui-imenu
    "Tp" 'electric-pair-local-mode
    "Tu" 'undo-tree-visualize
    "Tv" 'visual-line-mode
    "Tw" 'toggle-word-wrap
    "Tz" 'global-ligature-mode
    "Tl" 'global-display-line-numbers-mode

    "w" '(:ignore t :wk "Window")
    "p" '(:ignore t :wk "Projectile")))

(use-package hydra
  :demand
  :config
  (use-package use-package-hydra))

(use-package evil
  :demand
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil) 		; evil-colleciton expects this
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

  ;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (defadvice evil-inner-word (around underscore-as-word activate)
    (let ((table (copy-syntax-table (syntax-table))))
      (modify-syntax-entry ?_ "w" table)
      (with-syntax-table table
	ad-do-it)))

  (evil-mode t)

  (use-package evil-collection
    :after evil
    :config
    (setq my/evil-collection-disabled-modes '(lispy company))
    (evil-collection-init
     (seq-difference evil-collection--supported-modes my/evil-collection-disabled-modes)))

  (use-package evil-surround
    :config
    (global-evil-surround-mode 1))

  (use-package evil-escape
    :custom
    (evil-escape-key-sequence "jk")
    :config
    (evil-escape-mode t))

  (use-package evil-args
    :config
    ;; bind evil-args text objects
    (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
    (define-key evil-outer-text-objects-map "a" 'evil-outer-arg)))


;;; Builtin configurations

(use-package emacs
  :ensure nil
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)

  (setq indent-tabs-mode nil
	read-process-output-max (* 1024 1024) ; increase performance: https://emacs-lsp.github.io/lsp-mode/page/performance/
	;; line-number-display-limit 1	; no line numbers in modeline

	ring-bell-function #'ignore
	visible-bell t

	window-resize-pixelwise t
	frame-resize-pixelwise t

	create-lockfiles nil
	kill-buffer-query-functions nil
	delete-by-moving-to-trash t)

  (when (window-system)
    (tool-bar-mode -1)
    (scroll-bar-mode -1)
    (tooltip-mode -1)
    (menu-bar-mode -1)
    (setq frame-title-format nil))

  (unless (window-system)
    (set-display-table-slot standard-display-table 'vertical-border (make-glyph-code ?┃))))

(use-package font-lock
  :ensure nil
  :custom
  (font-lock-maximum-decoration t)) ; control amount of fontification, can be done per mode if slow

(use-package help
  :ensure nil
  :custom
  (help-window-select t))

(use-package window
  :ensure nil
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
  :ensure nil
  :general
  (my-leader-def
    "fs" 'save-buffer)
  :custom
  (backup-by-copying t)		    ; slow but sure way of saving
  (version-control t)		    ; version numbers for backup files
  (save-abbrevs 'silently)
  (backup-directory-alist `(("." . , user-temporary-file-directory)))
  (delete-old-versions t))

(use-package "startup"
  :ensure nil
  :custom
  (auto-save-list-file-prefix (concat user-temporary-file-directory ".auto-saves-"))
  (auto-save-file-name-transforms `((".*" ,user-temporary-file-directory t)))
  (inhibit-startup-screen t)
  (initial-buffer-choice (lambda () (switch-to-buffer "*Messages*")))
  (initial-major-mode #'fundamental-mode)
  (initial-scratch-message ";; Scratch Buffer, for lisp run (lisp-interaction-mode)"))

(use-package vc-hooks
  :ensure nil
  :custom
  (vc-follow-symlinks t))

(use-package paren
  :ensure nil
  :init
  (setq show-paren-mode t)
  :custom
  ((show-paren-when-point-inside-paren t)
   (show-paren-when-point-in-periphery t))
  :hook (prog-mode . show-paren-mode))

(use-package compile
  :ensure nil
  :custom
  ((compilation-scroll-output t)))

(use-package eldoc
  :hook (prog-mode org-mode)
  :ensure nil)

(use-package dired
  :ensure nil
  :custom (dired-listing-switches "-Algho --group-directories-first")

  :general
  (nmap "-" 'dired-jump)
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

(use-package winner
  :ensure nil
  :demand
  :config
  (winner-mode 1))

(use-package electric-pair-mode
  :ensure nil
  :hook (prog-mode . electric-pair-mode))

(use-package xwidget
  :ensure nil
  :if (window-system)
  :commands (xwidget-webkit-new-session)
  :config
  ;; (setq browse-url-browser-function 'xwidget-webkit-browse-url)
  (defun my/google-browse (search)
    (interactive "sSearch: ")
    (browse-url (format "https://www.google.co.uk/search?q=%s" (s-replace-all '((" " . "+")) search))))

  (defun my/google-xwidget (search)
    (interactive "sSearch: ")
    (xwidget-webkit-browse-url (format "https://www.google.co.uk/search?q=%s" (s-replace-all '((" " . "+")) search))))
  :general
  (nmap
    "gX" 'browse-url-at-point)
  (nmap :keymaps 'org-mode-map
    "gx" 'org-open-at-point
    "gX" 'xwidget-webkit-browse-url)
  (my-leader-def
    "gs" 'my/google-browse
    "gS" 'my/google-xwidget))


;;; Lore friendly improvements

(use-package restart-emacs
  :defer t
  :commands restart-emacs)

(use-package esup
  :disabled) ; doesn't work on v28 yet

(use-package which-key
  :demand
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
  :hook
  ((json-mode web-mode css-mode rjsx-mode typescript-mode scala-mode java-mode) . rainbow-delimiters-mode))

(use-package expand-region
  :general
  (general-def :states '(normal visual insert)
    "C-=" 'hydra-expand-region/body)
  :hydra
  (hydra-expand-region (:hint nil)
   "expand-region"
   ("h" er/expand-region "expand")
   ("l" er/contract-region "shrink")))

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
  :general
  (general-def :states '(normal visual emacs)
    "C-x o" 'ace-window
    "C-w C-w" 'ace-window)
  (my-leader-def
    "wd" 'ace-win-delete
    "ws" 'ace-win-swap)
  :commands
  (ace-win-swap ace-win-delete)
  :custom-face
  (aw-leading-char-face ((t (:inherit error :weight bold))))
  ;; (aw-leading-char-face ((t (:inherit warning :weight bold :height 2.0))))
  :config
  (setq aw-ignore-current t)
  (setq aw-minibuffer-flag t)
  (setq aw-keys '(?w ?e ?f ?j ?k ?l))

  (defun ace-win-delete ()
    (interactive)
    (ace-window 16))

  (defun ace-win-swap ()
    (interactive)
    (ace-window 4)))

(use-package auto-package-update
  :defer 10
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-interval 1)
  (auto-package-update-prompt-before-update t)
  :config
  (auto-package-update-at-time "09:18"))

(use-package page-break-lines
  :hook (emacs-lisp-mode . page-break-lines-mode))


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
    "wl" 'elscreen-toggle
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

  ;; light theme
  ;; :custom-face
  ;; (elscreen-tab-background-face ((t (:background "#dfdfdf" :height 1.3))))
  ;; (elscreen-tab-current-screen-face ((t (:background "#fafafa" :foreground "#a626a4"))))
  ;; (elscreen-tab-other-screen-face ((t (:background "#dfdfdf" :foreground "#a190a7"))))

  ;; one dark
  :custom-face
  ;; (elscreen-tab-background-face ((t (:background "#1c1f24" :height 1.3))))
  ;; (elscreen-tab-current-screen-face ((t (:background "#282c34" :foreground "#c678dd"))))
  ;; (elscreen-tab-other-screen-face ((t (:background "#1c1f24" :foreground "#a190a7"))))

  ;; nord
  :custom-face
  (elscreen-tab-background-face ((t (:background "#272C36" :height 1.5))))
  (elscreen-tab-current-screen-face ((t (:background "#2E3440" :foreground "#88C0D0"))))
  (elscreen-tab-other-screen-face ((t (:background "#272C36" :foreground "#5D80AE")))))

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
  :defer t
  :hook
  (vterm-mode-hook . (lambda ()
		       (setq-local evil-insert-state-cursor 'box) (evil-insert-state))))

(use-package multi-vterm
  :general
  (my-leader-def
    :keymaps 'override
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
  :init
  (solaire-global-mode +1)
  :custom
  ((doom-themes-enable-bold t)
   (doom-themes-enable-italic t))
  :config
  ;; (load-theme 'doom-one t)
  (load-theme 'doom-nord t)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)

  ;; set these after theme load
  ;; (set-face-attribute 'default nil :font "Hack Nerd Font")
  (set-face-attribute 'default nil :font "FiraCode Nerd Font")

  (set-face-attribute 'font-lock-comment-face nil :slant 'italic)

  (set-face-attribute 'font-lock-function-name-face nil :slant 'italic)
  (set-face-attribute 'font-lock-variable-name-face nil :weight 'semi-bold)

  ;; Set the fixed pitch face
  ;; (set-face-attribute 'fixed-pitch nil :font "Fira Code Retina")

  ;; Set the variable pitch face
  ;; (set-face-attribute 'variable-pitch nil :font "Avenir Next" :weight 'regular)
  )

(use-package all-the-icons)
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 20
	doom-modeline-env-version t
	doom-modeline-modal-icon t
	doom-modeline-vcs-max-length 24
	doom-modeline-buffer-file-name-style 'truncate-except-project
	;; Whether display buffer encoding.
	doom-modeline-buffer-encoding nil))

(use-package ligature
  :ensure nil
  :load-path "elpa/ligature.el"
  :hook (after-init . global-ligature-mode)
  :config
  ;; Enable the "www" ligature in every possible major mode
  (ligature-set-ligatures 't '("www"))
  ;; Enable all Cascadia Code ligatures in programming modes
  (ligature-set-ligatures
   'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
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
  :hook ((prog-mode . global-tree-sitter-mode)
	 (tree-sitter-after-on . tree-sitter-hl-mode))
  :commands (my/tree-sitter-hl)
  :general
  (my-leader-def
    "Th" 'my/tree-sitter-hl)
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
	(tree-sitter-hl-mode 't))))
  
  (use-package tree-sitter-langs))


;;; Projects / Navigation

(use-package projectile
  :defer t
  :custom
  (projectile-completion-system 'ivy)
  (projectile-indexing-method 'hybrid) ; use git whilst honoring .projectile ignores
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
				    :compile "yarn"
				    :test "yarn test"
				    :run "yarn start"
				    :test-suffix ".test")
  (projectile-register-project-type 'npm '("package-lock.json")
				    :compile "npm i"
				    :test "npm test"
				    :run "npm start"
				    :test-suffix "_test")
  (projectile-register-project-type 'gradle '("build.gradle")
				    :compile "npm i"
				    :test "npm test"
				    :run "npm start"
				    :test-suffix "Test"))

(use-package my-projectile-fns
  :after projectile
  :load-path "~/.emacs.d/elisp"
  :general
  (my-leader-def
    "pg" 'my/goto-github
    "bb" '(my/split-last-buffer :wk "split last buffer")))

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
  :general
  (general-def :keymaps 'override
    "C-x g" 'magit-status)
  (my-leader-def
    "gg" 'magit-status))

(use-package git-gutter
  :hook prog-mode
  :general
  (my-leader-def
    "G" 'hydra-git/body)
  :hydra
  (hydra-git
   (:hint nil
	  :post my/gutter-close-git-diff)
   "git-gutter"
   ("q" nil "quit" :color blue)
   ("r" git-gutter:update-all-windows "refresh")
   ("j" git-gutter:next-hunk "next")
   ("k" git-gutter:previous-hunk "previous")
   ("d" git-gutter:popup-hunk "diff")
   ("x" git-gutter:revert-hunk "revert hunk"))
  :config
  (defun my/gutter-close-git-diff ()
    "Close git-gutter diff if open"
    (interactive)
    (when (-contains? (window-list) (git-gutter:popup-buffer-window))
      (delete-window (git-gutter:popup-buffer-window)))))

(use-package forge
  :after magit
  :config
  (setq auth-sources '("~/.authinfo")))

(use-package wakatime-mode
  :after projectile
  :config
  (setq wakatime-cli-path "/usr/local/bin/wakatime")
  (global-wakatime-mode))


;;; Narrowing / Searching / Lists

(use-package ivy ;; TODO do i need these bindings and evil collection?
  :general
  (my-leader-def
    "sf" 'swiper

    "WW" 'ivy-push-view
    "WD" 'ivy-pop-view
    "Wl" 'ivy-switch-view)
  (general-def :keymaps 'override
    "C-s" 'swiper)
  :bind (:map ivy-minibuffer-map
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
  :custom
  ((ivy-use-virtual-buffers t)
   (ivy-wrap t)
   (ivy-count-format "(%d/%d) ")
   (ivy-initial-inputs-alist nil)      ; Don't start searches with ^
   (ivy-extra-directories ()))	       ; hide . and .. from file lists
  :config
  (ivy-mode 1)

  (setq ivy-re-builders-alist
	'((counsel-projectile-rg . ivy--regex-plus)
	  (swiper . ivy--regex-plus)	; fzy search in file is clumsy
	  (t . ivy--regex-fuzzy)))

  (setq enable-recursive-minibuffers t)

  (use-package ivy-rich
    :config
    (ivy-rich-mode 1))
  
  (use-package ivy-xref))

(use-package flx
  :after ivy)

(use-package ivy-posframe
  :disabled ;; this is pretty sexy, but clashes with frame resizing, macos full screen and elscreen
  :custom
  (ivy-posframe-width      115)
  (ivy-posframe-min-width  115)
  (ivy-posframe-height     10)
  (ivy-posframe-min-height 10)
  :config
  ()
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-center)))
  (setq ivy-posframe-parameters '((parent-frame . nil)
                                  (left-fringe . 8)
                                  (right-fringe . 8)))
  (ivy-posframe-mode 1))

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
    ":" '(counsel-command-history :wk "command history")
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "bL" '(counsel-switch-buffer :wk "global buffers")
    "si" 'counsel-imenu
    "gl" 'counsel-find-library)

  (my-local-leader-def :keymaps 'org-mode-map
    "gh" '(counsel-org-goto-all :wk "find heading")
    "gH" '(counsel-org-goto :wk "find heading in all"))
  :custom
  ;; ((counsel-find-file-ignore-regexp "(.|..)"))
  ((counsel-find-file-ignore-regexp "..")))

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
  :disabled 				; TODO practice with this first
  :bind
  ("C-c y s" . yas-insert-snippet)
  ("C-c y v" . yas-visit-snippet-file)
  :config
  ;; http://andreacrotti.github.io/yasnippet-snippets/snippets.html
  (use-package yasnippet-snippets)
  (yas-global-mode 1))

(use-package company
  :hook (prog-mode . company-mode)
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
  (company-minimum-prefix-length 10000)
  (company-idle-delay 0.0))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package lsp-mode
  :commands lsp
  :hook
  ((typescript-mode rjsx-mode web-mode scala-mode java-mode) . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :general
  (nmap "gh" 'lsp-describe-thing-at-point)
  :custom
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-disabled-clients '((json-mode . eslint)))
  (lsp-enable-file-watchers 'nil)
  (lsp-eslint-run "onType")
  (lsp-auto-execute-action 'nil))

(use-package lsp-ui
  :commands (lsp-ui-imenu)
  :hook (lsp-mode . lsp-ui-mode)
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
  :custom
  (lsp-ui-doc-header 'nil) ; looks shit
  (lsp-ui-sideline-enable 'nil) ; bloody overwhelming
  (lsp-ui-doc-use-webkit 'nil) ; don't be tempted by this, unless you really want to configure it
  (lsp-ui-doc-include-signature 'nil) ; eldoc does a better job of this
  (lsp-ui-doc-show-with-cursor 'nil) ; use keybinding instead or mouse
  (lsp-ui-imenu--custom-mode-line-format "lsp-ui-menu")
  (lsp-ui-doc-border "brightblack")
  (lsp-ui-doc-position 'at-point))

(use-package dap-mode
  :disabled
  :after lsp-mode
  :config (dap-auto-configure-mode))


;;; Programming Languages

;;;; Javascript / Typescript / Web

(use-package add-node-modules-path
  :hook
  (web-mode)
  (typescript-mode)
  (css-mode)
  (rjsx-mode))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :custom
  (js-indent-level 2)
  :config
  (setq typescript-indent-level 2))

(use-package rjsx-mode
  :mode "\\.jsx?\\'"
  :interpreter "node"
  :custom-face
  ;; doom-one
  ;; (rjsx-tag ((t (:slant italic :foreground "#c678dd"))))
  ;; (rjsx-attr ((t (:foreground "#ECBE7B"))))

  ;; doom-nord
  (js2-object-property ((t (:foreground "#ECEFF4"))))
  :config
  (add-hook 'rjsx-mode-hook #'js2-minor-mode)

  ;; Don't use built-in syntax checking
  (setq js2-mode-show-strict-warnings nil))

(use-package js2-refactor
  :hook ((web-mode rjsx-mode typescript-mode) . js2-refactor-mode))

(use-package web-mode
  :mode "\\.tsx\\'"
  :config
  (setq-default web-mode-comment-formats
		'(("javascript" . "//")
                  ("typescript" . "//")))

  (add-hook 'web-mode-hook #'my/web-mode-settings)

  (defun my/web-mode-settings ()
    "Hooks for Web mode."
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-enable-auto-quoting nil)
    (setq web-mode-markup-indent-offset 2)))

(use-package jest
  :hook ((rjsx-mode web-mode typescript-mode) . jest-minor-mode)
  :custom (jest-executable "yarn test"))

;;;; Scala
;; TODO probably need to set jenv

(use-package scala-mode
  :interpreter
  ("scala" . scala-mode))

(use-package lsp-metals
  :disabled
  :config (setq lsp-metals-treeview-show-when-views-received t))

;;;; Java

(use-package lsp-java
  :disabled
  :config (add-hook 'java-mode-hook 'lsp))

(use-package dap-java
  :disabled
  :ensure nil)

;;;; lisp

(use-package elisp-mode
  :ensure nil
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
  :hook (emacs-lisp-mode . lispy-mode))

(use-package lispyville
  :hook (lispy-mode . lispyville-mode)
  :config
  (lispyville-set-key-theme
   '(commentary	; comments on gc
     c-w	; C-w deletes backword word, so I should get used that
     ;; mark		 ; look into this another day
     prettify	 ; make == and the like work
     atom-motions ; make w and e respect atoms (e.g foo-bar is an atom)
     slurp/barf-cp
     additional	; binds a bunch of things to alt https://github.com/noctuid/lispyville#additional-key-theme
     additional-motions	; I mainly like this for so [ ] can act like 'd'
     additional-insert	; make M-[oO] M-[iI] smarter
     operators)))

(use-package paren-face
  :hook (lispy-mode . paren-face-mode))

;;;; Others

(use-package json-mode
  :mode "\\.json\\'")

(use-package yaml-mode
  :defer t)

(use-package graphql-mode
  :defer t)

(use-package dotenv-mode
  :mode "\\.env\\..*\\'"
  :hook (dotenv-mode . (lambda ()
              (set (make-local-variable 'comment-start) "# ")
              (set (make-local-variable 'comment-end) ""))))

(use-package rustic
  :defer 2)

(use-package dockerfile-mode
  :defer t)

(use-package docker-compose-mode
  :defer t)

(use-package docker
  :general
  (my-leader-def
    "gd" 'docker))

(use-package lsp-python-ms
  :disabled
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                          (require 'lsp-python-ms)
                          (lsp))))

(use-package flycheck
  ;; :custom
  ;; ((flycheck-check-syntax-automatically '(idle-buffer-switch save mode-enabled)))
  :hook (prog-mode . flycheck-mode)
  :general
  (my-leader-def
    "ef" 'my/eslint-fix-file-and-revert
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error
    "el" 'flycheck-list-errors
    "ee" 'flycheck-buffer
    ;; "ee" 'flycheck-display-error-at-point ;; not sure?
    "eh" 'flycheck-explain-error-at-point ;; not sure?
    "ei" 'flycheck-verify-setup))

(use-package editorconfig
  :hook (prog-mode . editorconfig-mode))


;;; Writing

(use-package flyspell
  :ensure nil
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
  (setq ispell-program-name "/usr/local/bin/aspell")

  (defun my/my-save-word ()
    (interactive)
    (let ((current-location (point))
	  (word (flyspell-get-word)))
      (when (consp word)
	(flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location)))))

(use-package flyspell-correct-ivy
  :after (flyspell ivy))

(use-package org
  :init
  ;; org is a special child that needs to be installed
  (unless (file-expand-wildcards (concat package-user-dir "/org-[0-9]*"))
    (package-install (elt (cdr (assoc 'org package-archive-contents)) 0)))

  (defvar org-directory "~/org-notes/")

  (defvar org-work-file "~/org-notes/org-sky-notes/work.org")
  (defvar org-personal-file "~/org-notes/org-me-notes/notes.org")

  (defvar org-default-notes-file (if (file-directory-p "~/sky")
				     (expand-file-name org-work-file)
				   (expand-file-name org-personal-file)))
  :commands
  (my/open-work-notes-file
   my/open-my-notes-file)
  :hook
  (org-mode . visual-line-mode)
  (org-mode . flyspell-mode)
  (org-mode . abbrev-mode)
  :custom
  (org-src-lang-modes '(("C" . c)
			("cpp" . c++)
			("bash" . sh)
			("sh" . sh)
			("elisp" . emacs-lisp)
			("javascript" . rjsx)
			("js" . rjsx)))
  (org-agenda-files (if (file-directory-p "~/sky")
			'("~/org-notes/org-sky-notes/work.org"
			  "~/org-notes/org-me-notes/notes.org")
		      '("~/org-notes/org-me-notes/notes.org")))
  :general
  (nmap :keymaps 'org-mode-map
    ">" 'org-shiftmetaright
    "<" 'org-shiftmetaleft
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)
  (my-local-leader-def :keymaps 'org-mode-map
    "c" 'org-toggle-checkbox
    "lt" 'my/org-toggle-list-checkbox
    "ls" 'org-sort-list
    "g" 'org-open-at-point
    "hh" 'org-toggle-heading
    "ho" 'evil-org-insert-heading-below
    "hn" 'org-insert-heading-respect-content
    "hs" 'org-insert-subheading
    "dr" 'org-table-kill-row
    "dc" 'org-table-delete-column
    "ic" 'org-table-insert-column
    "i-" 'org-table-insert-hline
    "s" 'org-sort
    "I" 'org-toggle-inline-images)
  (my-leader-def
    "nb" 'org-switchb
    "nl" 'org-store-link
    "nn" 'my/open-my-notes-file
    "nN" 'my/open-work-notes-file)
  :config
  (require 'org-tempo) ;; needed to add this to get template expansion to work again

  (use-package ob-typescript :after org)

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
	'((sequence "TODO(t)" "PROGRESS(p)" "|" "DONE(d)" "ARCHIVED(a)")))

  ;; seems broken
  (defun my/org-toggle-list-checkbox ()
    (interactive)
    (org-toggle-checkbox 4))

  (defun my/open-my-notes-file ()
    "Open rough notes."
    (interactive)
    (find-file org-personal-file))

  (defun my/open-work-notes-file ()
    "Open work notes."
    (interactive)
    (find-file org-work-file))

  (defun my/list-to-checkbox (arg)
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

  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t) ; allow bash
			       (typescript . t)
			       (haskell . t)
			       (ruby . t)
			       (io . t)))

  (require 'ob-js)
  ;; overwrite default wrapper function which doesn't work
  (setq org-babel-js-function-wrapper "(function() {
    require('util').inspect((function(){\n%s\n})())
  })()")

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

  (defun my/disable-org-checkdoc ()
    "Turn off elisp's flycheck checkdoc in src blocks."
    (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

  (add-hook 'org-src-mode-hook 'my/disable-org-checkdoc)

  ;; (use-package org-eldoc)

  (use-package org-bullets
    :if window-system
    :commands org-bullets-mode
    :hook (org-mode . org-bullets-mode)
    :custom
    (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

  ;; Drag-and-drop to `dired`
  (use-package org-download
    :hook (dired-mode-hook . org-download-enable))
  
  (use-package evil-org
    :config
    (add-hook 'org-mode-hook 'evil-org-mode)
    (add-hook 'evil-org-mode-hook
	      (lambda ()
		(evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))))
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys)))

(use-package org-alert
  :defer 20
  :custom (alert-default-style 'osx-notifier)
  :config

  (defun my/org-alert-remove-progress (fn &rest args)
    "Stop In Progress items having alerts"
    (let ((original-keywords (-copy org-done-keywords-for-agenda)))
      (setq org-done-keywords-for-agenda
	    (cons "PROGRESS" org-done-keywords-for-agenda))
      (let ((res (apply fn args)))
	(setq org-done-keywords-for-agenda original-keywords)
	res)))
  
  (advice-add 'org-alert--headline-complete? :around #'my/org-alert-remove-progress)

  (setq org-alert-interval 300)
  (setq org-alert-notification-title "Emacs | Org")
  (org-alert-enable))

(use-package markdown-mode
  :defer t
  :hook (markdown-mode . visual-line-mode)
  :general
  (nmap :keymaps 'markdown-mode-map
    "j" 'evil-next-visual-line
    "k" 'evil-previous-visual-line)

  (my-local-leader-def :keymaps 'markdown-mode-map
    "c" 'markdown-toggle-markup-hiding)
  :config
  (use-package markdown-toc))

(use-package poet-theme
  :disabled ; enable this section if writing
  :config
  (blink-cursor-mode 0)
  (load-theme 'poet t)
  (custom-set-faces
   '(vertical-border ((t (:background "black" :foreground "controlColor")))))
  ;; non monospace font
  (set-frame-font "Avenir Next:size=14")
  
  (use-package hide-mode-line
    :hook (org-mode . hide-mode-line-mode))

  (use-package olivetti
    :hook (org-mode . olivetti-mode)))

(use-package my-fns
  :load-path "~/.emacs.d/elisp"
  :general
  (general-def :keymaps 'override
    "C-s-<f8>" 'my/close-notifications-mac
    "C-M-<left>" 'frame-half-size-left
    "C-M-<right>" 'frame-half-size-right
    "C-M-<return>" 'toggle-frame-maximized)

  (my-leader-def
    "bk" '(my/kill-this-buffer :wk "kill buffer")
    "fk" '(my/delete-file-and-buffer :wk "delete file")
    "gp" 'my/pomo
    "gP" 'my/pomo-stop))

;; reset gc to something sensible for normal operation
(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here
