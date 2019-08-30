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

;; prompts for key bindings - https://github.com/justbur/emacs-which-key
(use-package which-key
  :config
  (which-key-mode t)
  (which-key-add-key-based-replacements
    "<SPC> b" "Buffers"
    "<SPC> e" "Errors"
    "<SPC> f" "Files"
    "<SPC> g" "Global"
    "<SPC> n" "Notes"
    "<SPC> p" "Projects"
    "<SPC> s" "Search"
    "<SPC> t" "Term"
    "<SPC> w" "Window"
    ))

(use-package magit)

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
                                    :test-suffix ".spec"))

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
