(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)
        (dired-sort-toggle-or-edit)))

;; annotations to install package
(use-package paradox
  :init
  (paradox-enable))

;; adds highlights to TODO and FIXME.
(use-package fic-mode
  :config (fic-mode 1))

;; jump to def without lsp
(use-package xref
  :config
  (setq xref-prompt-for-identifier 'nil))

(use-package fold-this)

;; shared clipbaord
(use-package xclip
  :config
  (xclip-mode 1))

(use-package flyspell
  :config
  (setq ispell-program-name "/usr/local/bin/aspell"))

(use-package editorconfig
  :config
  (editorconfig-mode 1))

(use-package dotenv-mode
  :config
  (add-hook 'dotenv-mode-hook
            (lambda ()
              (set (make-local-variable 'comment-start) "# ")
              (set (make-local-variable 'comment-end) "")))

  ;; for optionally supporting additional file extensions such as `.env.test' with this major mode
  (add-to-list 'auto-mode-alist '("\\.env\\..*\\'" . dotenv-mode)))

(use-package highlight-parentheses
  :config
  (global-highlight-parentheses-mode t))

(use-package undo-tree
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package magit)

(use-package ace-jump-mode)

(use-package ace-window
  :bind ("M-o" . ace-window)
  :commands
  (ace-win-swap ace-win-delete)
  :config
  (setq aw-ignore-current t)
  (setq aw-minibuffer-flag t)
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l))

  (defun ace-win-delete ()
    (interactive)
    (ace-window 16))

  (defun ace-win-swap ()
    (interactive)
    (ace-window 4))

  ;; turn off grey background
  ;; (setq aw-background nil)
  (custom-set-faces '(aw-leading-char-face ((t (:inherit warning :weight bold :height 2.0))))))

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1)
  (projectile-register-project-type 'yarn '("yarn.lock")
                                    :compile "yarn"
                                    :test "yarn test"
                                    :run "yarn start"
                                    :test-suffix ".spec")
  (projectile-register-project-type 'npm '("package-lock.json")
                                    :compile "npm i"
                                    :test "npm test"
                                    :run "npm start"
				    :test-suffix "_test")

  (defun my|test-file ()
    "Run tests on current file."
    (interactive)
    (message (concat "testing " (buffer-file-name)))
    (save-buffer)
    (async-shell-command
     (concat "cd " (projectile-project-root) " && node_modules/.bin/jest " (buffer-file-name) " --collectCoverageOnlyFrom " (my|replace-in-string ".spec.js" ".jsx" buffer-file-name))))

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
    (revert-buffer t t)))

(use-package helm
  :bind
  ("M-x" . helm-M-x)
  :config
  (helm-mode t))

(use-package helm-projectile
  :init
  (helm-projectile-on)
  :bind
  ("C-\\" . helm-projectile-rg))

(use-package helm-lsp :commands helm-lsp-workspace-symbol)

(use-package helm-rg
  :config
  (setq helm-rg-default-extra-args "--hidden"))
;; (setq helm-rg-default-extra-args "--hidden --follow"))

(use-package helm-flyspell)

(provide 'init-core)
