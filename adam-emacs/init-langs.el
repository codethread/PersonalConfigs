(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :hook
  (flycheck-mode . my|use-eslint-from-node-modules)
  (flycheck-mode . my|use-stylelint-from-node-modules)
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
  			'(lsp-ui javascript-jshint json-jsonlint scss-lint emacs-lisp-checkdoc)))

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
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode)))


;;; parse node.js stack traces in compilation buffer.s
(require 'compile)
(add-to-list 'compilation-error-regexp-alist 'node)
(add-to-list 'compilation-error-regexp-alist-alist
             '(node "^[[:blank:]]*at \\(.*(\\|\\)\\(.+?\\):\\([[:digit:]]+\\):\\([[:digit:]]+\\)" 2 3 4))

;; TODO: see if this replaces my eslint stuff
;; (require 'add-node-modules-path)
;; (add-hook 'prog-mode-hook #'add-node-modules-path)

(provide 'init-langs)
