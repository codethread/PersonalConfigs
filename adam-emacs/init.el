;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;;; set up initial package-managers
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
;; install use-package first if not installed, this then handles packages from here
(setq package-enable-at-startup nil)
(package-initialize)
(unless (package-installed-p 'use-package) (package-refresh-contents) (package-install 'use-package))
(eval-when-compile (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)
;;; Use-package set up, now import modules.
;; =====================================================================================

(require 'init-settings (concat user-emacs-directory "init-settings.el"))
(require 'init-core (concat user-emacs-directory "init-core.el"))
(require 'init-window (concat user-emacs-directory "init-window.el"))
(require 'init-terminal (concat user-emacs-directory "init-terminal.el"))
(require 'init-appearance (concat user-emacs-directory "init-appearance.el"))
(require 'init-company-lsp (concat user-emacs-directory "init-company-lsp.el"))
(require 'init-evil (concat user-emacs-directory "init-evil.el"))
(require 'init-org (concat user-emacs-directory "init-org.el"))
(require 'init-misc (concat user-emacs-directory "init-misc.el"))

;;; WIP AREA
;; =====================================================================================
(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (defun flycheck-eslint-config-exists-p ()
  "Whether there is a valid eslint config for the current buffer."
  (let* ((executable (flycheck-find-checker-executable 'javascript-eslint))
         (exitcode (and executable (call-process executable nil nil nil
                                                 "--print-config" "."))))
    (eq exitcode 0)))

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")
  (setq-default flycheck-disabled-checkers
  		(append flycheck-disabled-checkers
  			'(lsp-ui javascript-jshint json-jsonlint scss-lint emacs-lisp-checkdoc))))

(defun replace-in-string (WHAT WITH in)
  "`WHAT' to be replaced with `WITH' `IN' string."
  (replace-regexp-in-string (regexp-quote WHAT) WITH in nil 'literal))

(defun my|test-file ()
  "Run eslint --fix on current file."
  (interactive)
  (message (concat "testing " (buffer-file-name)))
  (save-buffer)
  (async-shell-command
   (concat "cd " (projectile-project-root) " && node_modules/.bin/jest " (buffer-file-name) " --collectCoverageOnlyFrom " (replace-in-string ".spec.js" ".jsx" buffer-file-name))))

(defun my|eslint-fix-file ()
  "Run eslint --fix on current file."
  (interactive)
  (message (concat "eslint --fixing" (buffer-file-name) "using"))
  (save-buffer)
  (shell-command
   (concat "cd " (projectile-project-root) " && node_modules/eslint/bin/eslint.js"
           (cond ((file-exists-p "./.eslintrc.js") " --config ./.eslintrc.js")
                 ((file-exists-p "./.eslintrc.yml") " --config ./.eslintrc.yml"))
           " --fix " (buffer-file-name))))

;; (defun my|stylelint-fix-file ()
;;   "Run eslint --fix on current file."
;;   (interactive)
;;   (save-buffer)
;;   (shell-command
;;    (concat "cd " (projectile-project-root) " && node_modules/stylelint/bin/stylelint.js --syntax scss --custom-formatter='./scripts/lint/stylelint-formatter' --fix " (buffer-file-name))))

(defun my|stylelint-fix-file ()
  "Run eslint --fix on current file."
  (interactive)
  (save-buffer)
  (shell-command
   (concat "cd " (projectile-project-root) " && node_modules/stylelint/bin/stylelint.js --syntax scss --fix " (buffer-file-name))))

(defun my|eslint-fix-file-and-revert ()
  (interactive)
  (my|eslint-fix-file)
  (revert-buffer t t))

;; (add-hook 'scss-mode-hook
;;           (lambda () (add-hook 'after-save-hook #'my|stylelint-fix-file)))

(use-package json-mode)

(use-package js2-mode
  :config
  (setq js2-mode-show-parse-errors 'nil
        js2-mode-show-strict-warnings 'nil
        js-chain-indent t
        js2-highlight-level 3
        js2-highlight-external-variables t))

(use-package rjsx-mode)
(use-package graphql-mode)

(use-package web-mode)
;; (flycheck-add-mode 'typescript-tslint 'web-mode)
  ;; :hook (lsp-mode))

;; (require 'web-mode)
;; (add-hook 'web-mode-hook
;;           (lambda ()
;;             (when (string-equal "tsx" (file-name-extension buffer-file-name))
;;               (setup-tide-mode))))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . rjsx-mode)) ;; i think node scripts

;; disable js2-mode warnings and errors since we'll use eslint
;; by default.
(custom-set-variables '(js2-mode-show-parse-errors nil)
                      '(js2-mode-show-strict-warnings nil))

;;; parse node.js stack traces in compilation buffer.s
(require 'compile)
(add-to-list 'compilation-error-regexp-alist 'node)
(add-to-list 'compilation-error-regexp-alist-alist
             '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 2 3 4))

;;; Specific versions of node packages installed on a per-project
;;; basis are the norm in JS development. So, for example, if you're
;;; using `eslint' to stylecheck your code, this will make project
;;; buffers find `node_modules/.bin/eslint' before any other
;;; executable in their `exec-path'
;; TODO: see if this replaces my eslint stuff
;; (require 'add-node-modules-path)
;; (add-hook 'prog-mode-hook #'add-node-modules-path)

;; use local eslint from node_modules before global
;; http://emacs.stackexchange.com/questions/21205/flycheck-with-file-relative-eslint-executable
(defun my|use-eslint-from-node-modules ()
  "Use eslint from nodemodules."
  (interactive)
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (eslint (and root
                      (expand-file-name "node_modules/eslint/bin/eslint.js"
                                        root))))
    (when (and eslint (file-executable-p eslint))
      (setq-local flycheck-javascript-eslint-executable eslint))))

(defun my|use-stylelint-from-node-modules ()
  "Use stylelint from node_modules."
  (let* ((root (locate-dominating-file
                (or (buffer-file-name) default-directory)
                "node_modules"))
         (stylelint (and root
                      (expand-file-name "node_modules/stylelint/bin/stylelint.js"
                                        root))))
    (when (and stylelint (file-executable-p stylelint))
      (setq-local flycheck-scss-stylelint-executable stylelint))))

;; (defun my|use-tslint-from-node-modules ()
;;   "Use tslint from nodemodules."
;;   (interactive)
;;   (let* ((root (locate-dominating-file
;;                 (or (buffer-file-name) default-directory)
;;                 "node_modules"))
;;          (eslint (and root
;;                       (expand-file-name "node_modules/tslint/bin/tslint.js"
;;                                         root))))
;;     (when (and eslint (file-executable-p eslint))
;;       (setq-local flycheck-typescript-tslint-config eslint))))

(add-hook 'flycheck-mode-hook #'my|use-eslint-from-node-modules)
(flycheck-add-mode 'javascript-eslint 'web-mode)
;; (add-hook 'flycheck-mode-hook #'my|use-stylelint-from-node-modules)

;;; init.el ends here.
