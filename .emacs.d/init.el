(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#1e1f29" "#ff5c57" "#5af78e" "#f3f99d" "#57c7ff" "#ff6ac1" "#57c7ff" "#eff0eb"])
 '(ansi-term-color-vector
   [unspecified "#1e1f29" "#ff5c57" "#5af78e" "#f3f99d" "#57c7ff" "#ff6ac1" "#57c7ff" "#eff0eb"])
 '(custom-safe-themes
   (quote
    ("0e8bac1e87493f6954faf5a62e1356ec9365bd5c33398af3e83cfdf662ad955f" default)))
 '(package-selected-packages
   (quote
    (org-evil exec-path-from-shell json-mode js2-mode web-mode flycheck projectile all-the-icons powerline zenburn-theme which-key use-package snazzy-theme ido-vertical-mode helm guide-key evil-leader))))

;; set up initial package-managers
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

;; You should enable global-evil-leader-mode before you enable evil-mode
;; https://github.com/cofi/evil-leader
(defun reload-init-file ()
  "Reload init.el without restart."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun open-init-file ()
  "open init.el"
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(use-package ido
  :init
  (ido-mode t)
  (use-package ido-vertical-mode
    :init (ido-vertical-mode 1))
  :config
  (setq ido-enable-flex-matching t))

(use-package evil-leader
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<SPC>" 'helm-M-x
    "pp" 'find-file
    "pf" 'helm-find
    "e" 'find-file
    "gs" 'reload-init-file
    "gv" 'open-init-file
    "ww" "\C-x3"
    "bl" 'switch-to-buffer
    "bk" 'kill-buffer))
;; will likely need this for org mode:
;; (evil-leader/set-key-for-mode 'emacs-lisp-mode "b" 'byte-compile-file)

(use-package evil
  :config
  (evil-mode t))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
            (lambda ()
              (evil-org-set-key-theme)))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package helm
  :config
  (helm-mode t)
  (setq helm-M-x-fuzzy-match t))
  ;; :bind (("M-x" . helm-M-x)
	 ;; ("M-<f5>" . helm-find-files)
	 ;; ([f10] . helm-buffers-list)
	 ;; ([S-f10] . helm-recentf)))

;; prompts for key bindings - https://github.com/justbur/emacs-which-key
;; does suuport evil bindings but can deal with that if needs be
(use-package which-key
  :config
  (which-key-mode t))

(which-key-add-key-based-replacements
  "<SPC> b" "Buffers"
  "<SPC> p" "Projects"
  "<SPC> w" "Window"
  "<SPC> g" "Global")
;; https://github.com/syl20bnr/spacemacs/blob/c7a103a772d808101d7635ec10f292ab9202d9ee/layers/%2Bdistributions/spacemacs-base/keybindings.el#L16 could probably be stolen here

;; (define-key some-map "f" '("foo" . long-name-for-command-foo))

;; GUI
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "#272935" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 130 :width normal :family "Fira Code")))))

;; deal with this to be done after
(use-package snazzy-theme)
(load-theme 'snazzy t)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)  


(use-package powerline
  :config
  (powerline-default-theme))

(use-package all-the-icons)

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))


;; sort out all this

;; (use-package flycheck)
;; (use-package web-mode)
;; (use-package js2-mode)
;; (use-package json-mode)


;; http://www.flycheck.org/manual/latest/index.html
(require 'flycheck)
(require 'js2-mode)

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
