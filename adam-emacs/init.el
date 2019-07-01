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
  "open init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun open-notes-file ()
  "open init.el"
  (interactive)
  (find-file "~/org-notes/org-me-notes/rough.org"))

;;; settings
;; =====================================================================================
;; y or n instead of yes etc
(defalias 'yes-or-no-p 'y-or-n-p)
;; always split vertically
(setq split-width-threshold 170)

;;; packages
;; =====================================================================================

(use-package org
  :config
  (defvar org-directory "~/org-notes/"))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

(use-package helm
  :config
  (helm-mode t)
  ;; (setq helm-M-x-fuzzy-match t)
  :bind ("M-x" . helm-M-x))

(use-package helm-projectile
  :config
  (helm-projectile-on)
  :bind ("C-\\" . helm-projectile-rg))

(use-package helm-rg
  :config
  (helm-projectile-on)
  (setq helm-rg-default-extra-args "--hidden"))

(require 'init-evil "~/.emacs.d/init-evil")

(use-package ido
  :init
  (ido-mode t)
  (use-package ido-vertical-mode
    :init (ido-vertical-mode 1))
  :config
  (setq ido-enable-flex-matching t))


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

;; (use-package flycheck)
;; (use-package web-mode)
;; (use-package js2-mode)
;; (use-package json-mode)


;; http://www.flycheck.org/manual/latest/index.html
(use-package flycheck)
(use-package js2-mode)

;; use web-mode for .jsx files
(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

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

;; adjust indents for web-mode to 2 spaces
(defun my-web-mode-hook ()
  "Hooks for Web mode. Adjust indents"
  ;;; http://web-mode.org/
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))
(add-hook 'web-mode-hook  'my-web-mode-hook)

;; for better jsx syntax-highlighting in web-mode
;; - courtesy of Patrick @halbtuerke
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
    (let ((web-mode-enable-part-face nil))
      ad-do-it)
    ad-do-it))

;; focus window after split
(global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
(global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))
