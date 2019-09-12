(use-package add-node-modules-path
  :config
  (add-hook 'prog-mode-hook #'add-node-modules-path))
  ;; :hook
  ;; (typescript-mode)
  ;; (rjsx-mode)
  ;; (web-mode))

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")
  (setq-default flycheck-disabled-checkers
  		(append flycheck-disabled-checkers
  			'(javascript-jshint json-jsonlint scss-lint emacs-lisp-checkdoc)))
  (flycheck-add-mode 'javascript-eslint 'web-mode))

(use-package json-mode)

(use-package js2-mode
  :config
  (setq js2-mode-show-parse-errors 'nil
        js2-mode-show-strict-warnings 'nil
        js-chain-indent t
        js2-highlight-level 3
	js2-highlight-external-variables t
	;; disable js2-mode warnings and errors since we'll use eslint
	js2-mode-show-parse-errors nil
	js2-mode-show-strict-warnings nil))

(use-package rjsx-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
  (add-to-list 'interpreter-mode-alist '("node" . rjsx-mode)))

(use-package graphql-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.gql\\'" . graphql-mode)))

(use-package web-mode
  :config
  (add-hook 'web-mode-hook 'flycheck-mode)
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))

;; (use-package typescript)


;;; parse node.js stack traces in compilation buffer.s
(require 'compile)
(add-to-list 'compilation-error-regexp-alist 'node)
(add-to-list 'compilation-error-regexp-alist-alist
             '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 2 3 4))


(provide 'init-langs)
