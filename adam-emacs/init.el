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

;;; settings
;; =====================================================================================
;; y or n instead of yes etc
(defalias 'yes-or-no-p 'y-or-n-p)
(setq split-width-threshold 170 ;; always split vertically if there's room
      help-window-select t
      show-paren-mode t ;; highlight parens
      )

;;; packages
;; =====================================================================================

(use-package paradox
  :init
  (paradox-enable))

(use-package org
  :init
  (defvar org-directory "~/org-notes/")
  (defvar org-default-notes-file (concat org-directory "/rough.org"))
  :bind
  ("C-c c" . org-capture)
  ("C-c l" . org-store-link)
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c b" . org-switchb))

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
;; (use-package lsp-ui :commands lsp-ui-mode) ;; XXX aweful but maybe use peak.

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
  "<SPC> p" "Projects"
  "<SPC> s" "Search"
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



;;; sort out all this
;; =====================================================================================
(use-package json-mode)
(use-package js2-mode)
(use-package rjsx-mode)
(use-package graphql-mode)

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
