;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs
;; TODO understand folds
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;;; set up gui
;; =====================================================================================

(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
(setq ns-use-proxy-icon nil)
(setq frame-title-format nil)

;;; set up initial package-managers
;; =====================================================================================
(require 'package)

(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)

;; install use-package first if not install
;; this then handles packages from here
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)


;;; system
;; =====================================================================================

;; forward function declarations eliminate warnings about whether a
;; function is defined.
(declare-function exec-path-from-shell-initialize "exec-path-from-shell.el")

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; disable the toolbar at the top of the window
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; OSX specific code
(when (eq system-type 'darwin)

  ;; On OS X Emacs doesn't use the shell PATH if it's not started from
  ;; the shell. Let's fix that:
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)

  ;; It's all in the Meta
  (setq ns-function-modifier 'hyper)

  (menu-bar-mode +1)

  ;; Enable emoji, and stop the UI from freezing when trying to display them.
  (if (fboundp 'set-fontset-font)
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))
;; ;; https://github.com/purcell/exec-path-from-shell
;; ;; only need exec-path-from-shell on OSX
;; ;; this hopefully sets up path and other vars better
;; (exec-path-from-shell-initialize)

;; (when (memq window-system '(mac ns))
;;   (exec-path-from-shell-initialize))

;;; window
;; =====================================================================================

;; Split horizontally when opening a new window from a command
;; whenever possible.
(setq split-height-threshold nil)

(defun frontside-windowing-adjust-split-width-threshold ()
  "Change the value of `split-width-threshold' so that it will cause the screen
split once and only once.

For example, if the frame is 360 columns wide, then we want the
split-width-threshold to be 181. That way, when you split horizontally, the two
new windows will each be 180 columns wide, and sit just below the threshold.
"
  (setq split-width-threshold (+ 1 (/ (frame-width) 2))))

;; recaculate split-width-threshold with every change
(add-hook 'window-configuration-change-hook
          'frontside-windowing-adjust-split-width-threshold)

;;; settings
;; =====================================================================================
;; y or n instead of yes etc
(defalias 'yes-or-no-p 'y-or-n-p)
(setq help-window-select t
 ;; split-width-threshold 170 ;; always split vertically if there's room
      show-paren-mode t ;; highlight parens
      ;; no bells
      ring-bell-function #'ignore
      visible-bell t
      ;; don't resize emacs in steps, it looks weird
      window-resize-pixelwise t
      frame-resize-pixelwise t
      )

(setq-default indent-tabs-mode nil)
(setq-default font-lock-maximum-decoration 3)

(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)
        (dired-sort-toggle-or-edit)))
(setq vc-follow-symlinks t)


;;; functions
;; =====================================================================================

(defun reload-init-file ()
  "Reload init.el without restart."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun open-init-file ()
  "Open init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-notes-file ()
  "Open rough notes."
  (interactive)
  (find-file "~/org-notes/org-me-notes/rough.org"))

(defun what-face (pos)
  (interactive "d")
  (let ((face (or (get-char-property (pos) 'read-face-name)
                  (get-char-property (pos) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(defun shell-other-window ()
  "Open a `shell' in a new window."
  (interactive)
  (let ((buf (shell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

(defun frontmacs/vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))
(global-set-key (kbd "C-x 2") 'frontmacs/vsplit-last-buffer)

;; horizontal split, switch window, and open next buffer
(defun frontmacs/hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))
(global-set-key (kbd "C-x 3") 'frontmacs/hsplit-last-buffer)

(defun projectile-term ()
  "Create an ansi-term at the project root"
  (interactive)
  (let ((root (projectile-project-root))
	(buff-name (concat " [term] " (projectile-project-root))))
    (if (get-buffer buff-name)
      (switch-to-buffer-other-window buff-name)
      (progn
	(split-window-sensibly (selected-window))
	(other-window 1)
	(setq default-directory root)
	(ansi-term (getenv "SHELL"))
	(rename-buffer buff-name t)))))

(global-set-key (kbd "C-x M-t") 'projectile-term)

;; focus window after split
;; (global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
;; (global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(defun frame-half-size-left ()
  "Set the current frame to half the screen width."
  (interactive)
  (let ((frame (selected-frame))
        (one-half-display-pixel-width (/ (display-pixel-width) 2)))
    (set-frame-width frame one-half-display-pixel-width nil 'pixelwise)
    (set-frame-position frame 0 0))
  )

(defun frame-half-size-right ()
  "Set the current frame to half the screen width."
  (interactive)
  (let ((frame (selected-frame))
        (one-half-display-pixel-width (/ (display-pixel-width) 2)))
    (set-frame-width frame one-half-display-pixel-width nil 'pixelwise)
    (set-frame-position frame one-half-display-pixel-width 0))
  )

(global-set-key (kbd "C-M-<left>") 'frame-half-size-left)
(global-set-key (kbd "C-M-<right>") 'frame-half-size-right)
(global-set-key (kbd "C-M-<return>") 'toggle-frame-maximized)

;;; packages
;; =====================================================================================

(use-package paradox
  :init
  (paradox-enable))

(use-package xref
  :config
  (setq xref-prompt-for-identifier 'nil))

;; shared clipbaord
(use-package xclip
  :config
  (xclip-mode 1))

(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

(use-package org
  :init
  (defvar org-directory "~/org-notes/")
  (defvar org-default-notes-file (concat org-directory "/rough.org"))
  :bind
  ;; ("C-c c" . org-capture)
  ;; ("C-c l" . org-store-link)
  ;; ("C-c a" . org-agenda)
  ;; ("C-c c" . org-capture)
  ;; ("C-c b" . org-switchb)
  :hook
  (org-mode . visual-line-mode)
  (org-mode . flyspell-mode)
  :config
  (setq org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-hide-leading-stars t
        org-startup-folded t
        org-startup-indented t))

(use-package org-bullets
  :commands org-bullets-mode
  :hook (org-mode . org-bullets-mode))

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
 'append)

(use-package magit)

(use-package ace-jump-mode
  :bind
  ("C-c <SPC>" . ace-jump-mode))
 
(use-package ace-window
  :config
  (global-set-key (kbd "C-x o") 'ace-window)
  (setq aw-ignore-current t)
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  ;; (setq aw-background nil)
  (custom-set-faces '(aw-leading-char-face ((t (:height 3.0)))))
  )

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))


(use-package helm
  :config
  (helm-mode t)
  ;; (setq helm-M-x-fuzzy-match t)
  :bind
  ("M-x" . helm-M-x)
  )

(use-package helm-projectile
  :init
  (helm-projectile-on)
  :bind
  ("C-\\" . helm-projectile-rg)
  )

;; (use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package helm-rg
  :config
  (setq helm-rg-default-extra-args "--hidden"))
  ;; (setq helm-rg-default-extra-args "--hidden --follow"))

;; (use-package lsp-mode
;;   :hook (js2-mode . lsp)
;;   :commands lsp
;;   :config
;;   (setq lsp-enable-snippet nil)
;;   (setq lsp-auto-guess-root t)
;;   )

;; (use-package lsp-ui
;;   :config
;;   (setq lsp-ui-sideline-enable nil)
;;   ;; lsp-ui-sideline-show-symbol
;;   :commands lsp-ui-mode) ;; XXX aweful but maybe use peak.

;; (use-package lsp-ui
;;   :after lsp-mode
;;   :diminish
;;   :commands lsp-ui-mode
;;   :custom-face
;;   (lsp-ui-doc-background ((t (:background nil))))
;;   (lsp-ui-doc-header ((t (:inherit (font-lock-string-face italic)))))
;;   :bind (:map lsp-ui-mode-map
;;               ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;;               ([remap xref-find-references] . lsp-ui-peek-find-references)
;;               ("C-c u" . lsp-ui-imenu))
;;   :init
;;   (setq lsp-ui-doc-enable t
;;         lsp-ui-doc-header t
;;         lsp-ui-doc-include-signature t
;;         lsp-ui-doc-position 'top
;;         lsp-ui-doc-use-webkit t
;;         lsp-ui-doc-border (face-foreground 'default)
;;         lsp-ui-sideline-enable nil
;;         lsp-ui-sideline-ignore-duplicate t
;;         lsp-ui-sideline-show-code-actions nil)
;;   :config
;;   ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
;;   ;; https://github.com/emacs-lsp/lsp-ui/issues/243
;;   (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
;;     (setq mode-line-format nil)))

(use-package company
  :init
  (setq company-tooltip-align-annotations t
        company-global-modes
        '(not erc-mode message-mode help-mode gud-mode eshell-mode)
        company-backends '(company-capf)
        company-frontends
        '(company-pseudo-tooltip-frontend
          company-echo-metadata-frontend)
	)
  :config
  (global-company-mode +1)
  )

(use-package company-lsp
  :commands company-lsp
  )


;; (use-package company-lsp
;;   :commands company-lsp
;;   :config
;;   ;; (set-company-backend! 'lsp-mode 'company-lsp)
;;   (company-lsp-enable-snippet nil)
;;   )

;;; EVIL
;; =====================================================================================
;; You should enable global-evil-leader-mode before you enable evil-mode
;; https://github.com/cofi/evil-leader
	 ;; ("M-<f5>" . helm-find-files)
	 ;; ([f10] . helm-buffers-list)
	 ;; ([S-f10] . helm-recentf)))

;; /Users/adh23/doom-emacs/modules/config/default/+evil-bindings.el
(use-package evil-leader
  :init
  (setq evil-want-keybinding 'nil)
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<SPC>" 'projectile-find-file
    ";" 'helm-M-x
    ;; b --- buffers
    "bc" 'delete-window
    "bl" 'helm-buffers-list
    "bk" 'kill-buffer
    "bj" 'evil-show-jumps
    "bp" 'evil-prev-buffer
    "bP" 'evil-split-prev-buffer
    "bn" 'evil-next-buffer
    "bN" 'evil-split-next-buffer
    "br" 'rename-buffer

    ;; e -- error
    "ef" 'my|eslint-fix-file-and-revert

    ;; f --- file
    "ff" 'helm-find-files
    "fr" 'helm-recentf
    "fR" 'projectile-recentf
    "F" 'org-cycle ;; TODO deal with this
    "fv" 'open-init-file

    ;; g -- global
    "gs" 'reload-init-file ;; TODO make more glorious

    ;; w -- window
    "ww" "\C-x3"
    "wt" 'elscreen-toggle-display-tab
    "wr" 'elscreen-screen-nickname
    "wN" 'elscreen-create
    "wl" 'elscreen-toggle

    ;; s -- search
    "sf" 'helm-occur ;; great when you know what you need
    "si" 'helm-imenu ;; jump to def or explore
    "sI" 'helm-imenu-in-all-buffers ;; ideal when don't know
    "sp" 'helm-projectile-rg ;; also ag or grep
    "sl" 'xref-find-references ;; also ag or grep

    ;; n --- notes
    "nn" 'open-notes-file
    "nb" 'org-switchb

    ;; p --- project
    "p!" 'projectile-run-shell-command-in-root  ;;  "Run cmd in project root"
    "pa" 'projectile-add-known-project          ;;  "Add new project"
    "pb" 'projectile-switch-to-buffer           ;;  "Switch to project buffer"
    "pc" 'projectile-compile-project            ;;  "Compile in project"
    "pd" 'projectile-remove-known-project       ;;  "Remove known project"
    "pe" 'projectile-edit-dir-locals            ;;  "Edit project .dir-locals"
    "pf" 'projectile-find-file                  ;;  "Find file in project"
    "pF" 'projectile-find-file-in-known-projects ;;  "Find file in project"
    "pg" 'projectile-find-file-dwim ;;  "Find file in project at point better ffap"
    "pi" 'projectile-invalidate-cache           ;;  "Invalidate project cache"
    "pk" 'projectile-kill-buffers               ;;  "Kill project buffers"
    "po" 'projectile-find-other-file            ;;  "Find other file"
    "pp" 'projectile-switch-project             ;;  "Switch project"
    "pr" 'projectile-recentf                    ;;  "Find recent project files"
    "ps" 'helm-projectile-rg ;; also ag or grep

    ;; t --- terminal
    "to" 'multi-term
    "tt" 'projectile-term
    ))
;; will likely need this for org mode:
;; (evil-leader/set-key-for-mode 'emacs-lisp-mode "b" 'byte-compile-file)

;; prevent tab being taken in org mode for terminal emacs
;; (defvar evil-want-C-i-jump (or (daemonp) (display-graphic-p)))

;; Scrolling
(defun evil-scroll-down-other-window ()
  (interactive)
  (scroll-other-window))

(defun evil-scroll-up-other-window ()
  (interactive)
  (scroll-other-window '-))


(use-package evil
  :after evil-leader
  :init
  (setq evil-vsplit-window-right t
	evil-want-C-i-jump nil
	evil-split-window-below t)
  :config
  (evil-mode t)
  ;; (setq evil-mode-line-format 'before)
  ;; (setq evil-emacs-state-cursor  '("red" box))
  ;; (setq evil-normal-state-cursor '("magenta" box))
  ;; (setq evil-visual-state-cursor '("red" box))
  ;; (setq evil-insert-state-cursor '("gray" bar))
  ;; (setq evil-motion-state-cursor '("HotPink2" box))

  ;; better ffap
  ;; (evil-define-key 'normal 'global
  ;;   ("gf" . helm-projectile-find-file-dwim))
  ;; (define-key evil-normal-state-map
  ;;   (kbd "C-S-d") 'evil-scroll-down-other-window)

  ;; (define-key evil-normal-state-map
  ;;   (kbd "C-S-u") 'evil-scroll-up-other-window)

  (define-key evil-normal-state-map "s" 'ace-jump-mode)

  (define-key evil-normal-state-map "gf" 'helm-projectile-find-file-dwim)
  (define-key evil-normal-state-map "gD" 'helm-lsp-workspace-symbol)
  (define-key evil-normal-state-map "gh" 'lsp-describe-thing-at-point)
  ;; (define-key evil-normal-state-map "-" 'dired-jump)
  (define-key evil-insert-state-map (kbd "C-@") 'company-complete)
  ;; gui mode
  (define-key evil-insert-state-map (kbd "C-SPC") 'company-complete)
  ;; reset
  (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)

  ;; get scroll up back and replace with C-m as it's just return
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  ;; (define-key evil-normal-state-map (kbd "C-m") 'universal-argument)
  ;; remap to sexp
  (define-key evil-normal-state-map (kbd "C-M-l") 'forward-sexp)
  (define-key evil-normal-state-map (kbd "C-M-h") 'backward-sexp) ;; mark-defun
  (define-key evil-normal-state-map (kbd "C-M-k") 'backward-up-list) ;; kill-sexp
  (define-key evil-normal-state-map (kbd "C-M-j") 'down-list)

  ;; bring line into focus and attempt to show context.
  (define-key evil-normal-state-map (kbd "L") 'reposition-window)
  )

;; (use-package evil-collection
;;   :after evil
;;   :config
;;   (evil-collection-init 'calendar)
;;   (setq evil-collection-mode-list 'nil))

(use-package evil-commentary
  :config
  (evil-commentary-mode t))

;; https://github.com/Somelauw/evil-org-mode
;; (use-package evil-org
;;   :ensure t
;;   :after org
;;   :config
;;   (add-hook 'org-mode-hook 'evil-org-mode)
;;   (add-hook 'evil-org-mode-hook
;; 	    (lambda ()
;; 	      (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))))
;;   (require 'evil-org-agenda)
;;   (evil-declare-key 'normal org-mode-map ;; (evil-define-key in https://github.com/noctuid/evil-guide#binding-keys-to-keys-keyboard-macros ?
;;     ",c" 'org-toggle-checkbox
;;     ",g" 'org-open-at-point
;;     ",hn" 'org-insert-heading-respect-content ;; there is an evil for this?
;;      ">" 'org-shiftmetaright
;;      "<" 'org-shiftmetaleft
;;     (kbd "TAB") 'org-cycle)
;;   (evil-org-agenda-set-keys))

(use-package evil-escape
  :config
  (evil-escape-mode t)
  (setq evil-escape-key-sequence "jk")
  )

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; (use-package centaur-tabs
;;   :demand
;;   :config
;;   (centaur-tabs-mode t)
;;   (setq centaur-tabs-set-close-button nil)
;;   (setq centaur-tabs-set-modified-marker t)
;;   (setq centaur-tabs--buffer-show-groups t)
;;   :bind
;;   (:map evil-normal-state-map
;; 	     ("g t" . centaur-tabs-forward)
;; 	     ("g T" . centaur-tabs-backward))
;;   )

(use-package evil-extra-operator
  :config
  (global-evil-extra-operator-mode 1))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package elscreen
  :config
  (setq elscreen-display-screen-number nil
        elscreen-tab-display-kill-screen 'nil
        elscreen-tab-display-control 'nil)
  )

(use-package evil-tabs
  :after elscreen
  :config
  (global-evil-tabs-mode t)
  )

;; evil-tabs :q closes whole tab so thisgg should fix it and come last
(evil-ex-define-cmd "q[uit]" 'evil-quit)


(use-package highlight-parentheses
  :diminish
  ;; :hook (emacs-lisp-mode . highlight-parentheses-mode)
  )
;; (add-hook 'emacs-lisp-mode-hook (lambda() highlight-parentheses-mode t)

;; (use-package ido
;;   :init
;;   (ido-mode t)
;;   (use-package ido-vertical-mode
;;     :init (ido-vertical-mode 1))
;;   :config
;;   (setq ido-enable-flex-matching t))


;; prompts for key bindings - https://github.com/justbur/emacs-which-key
;; does suuport evil bindings but can deal with that if needs be
(use-package which-key
  :config
  (which-key-mode t)
(which-key-add-key-based-replacements
  "<SPC> b" "Buffers"
  "<SPC> e" "Errors"
  "<SPC> f" "Files"
  "<SPC> g" "Global"
  "<SPC> n" "Notes"
  ;; "<SPC> p" "Projects"
  "<SPC> s" "Search"
  "<SPC> t" "Term"
  "<SPC> w" "Window"
  ))

;; https://github.com/syl20bnr/spacemacs/blob/c7a103a772d808101d7635ec10f292ab9202d9ee/layers/%2Bdistributions/spacemacs-base/keybindings.el#L16 could probably be stolen here

;; (define-key some-map "f" '("foo" . long-name-for-command-foo))
;; deal with this to be done after
;; (use-package dracula-theme
;;   :config
;;   (load-theme 'dracula t))

(use-package doom-themes
  :config
  (load-theme 'doom-one t)
  ;; (load-theme 'doom-city-lights t)
  ;; (load-theme 'doom-dracula t)
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

;; (use-package powerline
;;   :config
;;   ;; (powerline-default-theme))
;;   (powerline-center-evil-theme))
(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))

(use-package hide-mode-line
  :hook ((term-mode occur) . hide-mode-line-mode))

(use-package all-the-icons)


(use-package multi-term
  :config
  (setq multi-term-program "/bin/zsh")
  )

(require 'dotenv-mode) ; unless installed from a package
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)) ;; for optionally supporting additional file extensions such as `.env.test' with this major mode

;;; sort out all this
;; =====================================================================================
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
  		(append flycheck-disabled-checkers
  			'(javascript-jshint)))
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")
  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
    (append flycheck-disabled-checkers
      '(json-jsonlist)))
  )

(defun my|eslint-fix-file ()
  "Run eslint --fix on current file."
  (interactive)
  (message (concat "eslint --fixing" (buffer-file-name) "using"))
  (shell-command
   (concat "cd " (projectile-project-root) " && node_modules/eslint/bin/eslint.js --config ./.eslintrc.js --fix " (buffer-file-name))))

(defun my|eslint-fix-file-and-revert ()
  (interactive)
  (my|eslint-fix-file)
  (revert-buffer t t))

;; (add-hook 'js2-mode-hook
;;           (lambda ()
;;             (add-hook 'after-save-hook #'eslint-fix-file-and-revert)))


(use-package json-mode)
(use-package js2-mode)
(use-package rjsx-mode)
(use-package graphql-mode)

;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))


(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))




;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my/use-eslint-from-node-modules ()
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)


;; https://github.com/chiply/spot4e
(require 'spot4e "~/.emacs.d/spot4e")
(setq spot4e-refresh-token (getenv "SPOTIFY_TOKEN"))
(run-with-timer 0 (* 60 59) 'spot4e-refresh)
;;; init.el ends here.
