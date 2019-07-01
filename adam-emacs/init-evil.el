;;; init-evil.el --- Initialization for evil
;;; Commentary: Keep all the evil things together

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
    "bb" 'helm-buffers-list
    "bk" 'kill-buffer

    ;; f --- file
    "ff" 'helm-find-files
    "fr" 'helm-recentf
    "fR" 'projectile-recentf
    "F" 'org-cycle ;; TODO deal with this
    "fv" 'open-init-file

    "gs" 'reload-init-file ;; TODO make more glorious
    "ww" "\C-x3"

    ;; n --- notes
    "nn" 'open-notes-file

    ;; p --- project
    ;; "." '+default/browse-project               ;;  "Browse project"
    ;; ">" 'doom/browse-in-other-project          ;;  "Browse other project"
    ;; "?" 'doom/find-file-in-other-project       ;;  "Find file in other project"
    ;; "pb" 'projectile-switch-to-buffer           ;;  "Switch to project buffer"
    ;; "pf" 'projectile-find-file                  ;;  "Find file in project"
    ;; "pp" 'projectile-switch-project             ;;  "Switch project"
    ;; "pr" 'projectile-recentf                    ;;  "Find recent project files"
    ;; "x" 'doom/open-project-scratch-buffer      ;;  "Pop up scratch buffer"
    ;; "X" 'doom/switch-to-project-scratch-buffer ;;  "Switch to scratch buffer"
    ;; "t" '+default/project-tasks)               ;;  "List project tasks"

    ;; p --- project
    "p!" 'projectile-run-shell-command-in-root  ;;  "Run cmd in project root"
    "pa" 'projectile-add-known-project          ;;  "Add new project"
    "pb" 'helm-projectile-switch-to-buffer           ;;  "Switch to project buffer"
    "pc" 'projectile-compile-project            ;;  "Compile in project"
    "pd" 'projectile-remove-known-project       ;;  "Remove known project"
    "pe" 'projectile-edit-dir-locals            ;;  "Edit project .dir-locals"
    "pf" 'helm-projectile-find-file                  ;;  "Find file in project"
    "pF" 'helm-projectile-find-file-in-known-projects ;;  "Find file in project"
    "pi" 'projectile-invalidate-cache           ;;  "Invalidate project cache"
    "pk" 'projectile-kill-buffers               ;;  "Kill project buffers"
    "po" 'projectile-find-other-file            ;;  "Find other file"
    "pp" 'helm-projectile-switch-project             ;;  "Switch project"
    "pr" 'helm-projectile-recentf                    ;;  "Find recent project files"
    "ps" 'helm-projectile-rg ;; also ag or grep
    ))
;; will likely need this for org mode:
;; (evil-leader/set-key-for-mode 'emacs-lisp-mode "b" 'byte-compile-file)

;; prevent tab being taken in org mode for terminal emacs
;; (defvar evil-want-C-i-jump (or (daemonp) (display-graphic-p)))


(use-package evil
  :after evil-leader
  :init
  (setq evil-vsplit-window-right t
	evil-split-window-below t
	evil-want-C-u-scroll t)
  :config
  (evil-mode t))

(use-package evil-commentary
  :config
  (evil-commentary-mode t))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))))
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(provide 'init-evil)
