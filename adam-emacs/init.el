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
;;; settings
;; =====================================================================================
;; y or n instead of yes etc
(defalias 'yes-or-no-p 'y-or-n-p)
(setq split-width-threshold 170 ;; always split vertically if there's room
      help-window-select t
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

;;; packages
;; =====================================================================================

(use-package paradox
  :init
  (paradox-enable))

;; shared clipbaord
(use-package xclip
  :config
  (xclip-mode 1))

(use-package org-bullets)

(use-package org
  :init
  (defvar org-directory "~/org-notes/")
  (defvar org-default-notes-file (concat org-directory "/rough.org"))
  :bind
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c b" . org-switchb)
  ; ; :hook
  ; (org-bullets-mode 1)           ; "prettier" bullets
  ; (org-indent-mode 1)          ; margin-based indentation
  ; (visual-line-mode 1)         ; line wrapping
  )
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
; (add-hook 'org-mode-hook (lambda () (org-indent-mode 1)))
; (add-hook 'org-mode-hook (lambda () (org-visual-mode 1)))

(font-lock-add-keywords
 'org-mode
 `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
 'append)

(setq-default
 org-fontify-done-headline t
 org-fontify-whole-heading-line t
 org-hide-leading-stars t
 org-startup-folded t
 org-startup-indented t
 )

(use-package magit)

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

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package helm-rg
  :config
  (setq helm-rg-default-extra-args "--hidden"))
  ;; (setq helm-rg-default-extra-args "--hidden --follow"))

(use-package lsp-mode
  :hook (js2-mode . lsp)
  :commands lsp
  :config
  (setq lsp-enable-snippet nil)
  (setq lsp-auto-guess-root t)
  )

(use-package lsp-ui :commands lsp-ui-mode) ;; XXX aweful but maybe use peak.

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

(require 'init-evil "~/.emacs.d/init-evil")

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
(use-package dracula-theme
  :config
  (load-theme 'dracula t))

(use-package powerline
  :config
  (powerline-default-theme))

(use-package all-the-icons)


(use-package multi-term
  :config
  (setq multi-term-program "/bin/zsh")
  )

(require 'dotenv-mode) ; unless installed from a package
(add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)) ;; for optionally supporting additional file extensions such as `.env.test' with this major mode

;;; sort out all this
;; =====================================================================================
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

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  ;; disable jshint since we prefer eslint checking
  ;; (setq-default flycheck-disabled-checkers
  ;; 		(append flycheck-disabled-checkers
  ;; 			'(javascript-jshint)))
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")
  ;; disable json-jsonlist checking for json files
  ;; (setq-default flycheck-disabled-checkers
  ;;   (append flycheck-disabled-checkers
  ;;     '(json-jsonlist)))
  )



;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(exec-path-from-shell-initialize)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

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

;; focus window after split
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))
