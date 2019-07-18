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
    "bc" 'delete-window
    "bl" 'helm-buffers-list
    "bk" 'kill-buffer
    "bp" 'evil-prev-buffer
    "bP" 'evil-split-prev-buffer
    "bn" 'evil-next-buffer
    "bN" 'evil-split-next-buffer
    "br" 'rename-buffer

    ;; f --- file
    "ff" 'helm-find-files
    "fr" 'helm-recentf
    "fR" 'projectile-recentf
    "F" 'org-cycle ;; TODO deal with this
    "fv" 'open-init-file

    ;; g -- global
    "gs" 'reload-init-file ;; TODO make more glorious

    ;; w -- window
    "ww" "\C-x3"
    "wt" 'elscreen-toggle-display-tab
    "wr" 'elscreen-screen-nickname
    "wN" 'elscreen-create
    "wl" 'elscreen-toggle

    ;; s -- search
    "sf" 'helm-occur

    ;; n --- notes
    "nn" 'open-notes-file
    "nb" 'org-switchb

    ;; p --- project
    "p!" 'projectile-run-shell-command-in-root  ;;  "Run cmd in project root"
    "pa" 'projectile-add-known-project          ;;  "Add new project"
    "pb" 'projectile-switch-to-buffer           ;;  "Switch to project buffer"
    "pc" 'projectile-compile-project            ;;  "Compile in project"
    "pd" 'projectile-remove-known-project       ;;  "Remove known project"
    "pe" 'projectile-edit-dir-locals            ;;  "Edit project .dir-locals"
    "pf" 'projectile-find-file                  ;;  "Find file in project"
    "pF" 'projectile-find-file-in-known-projects ;;  "Find file in project"
    "pg" 'projectile-find-file-dwim ;;  "Find file in project at point better ffap"
    "pi" 'projectile-invalidate-cache           ;;  "Invalidate project cache"
    "pk" 'projectile-kill-buffers               ;;  "Kill project buffers"
    "po" 'projectile-find-other-file            ;;  "Find other file"
    "pp" 'projectile-switch-project             ;;  "Switch project"
    "pr" 'projectile-recentf                    ;;  "Find recent project files"
    "ps" 'projectile-rg ;; also ag or grep

    ;; t --- terminal
    "tt" 'multi-term
    ))
;; will likely need this for org mode:
;; (evil-leader/set-key-for-mode 'emacs-lisp-mode "b" 'byte-compile-file)

;; prevent tab being taken in org mode for terminal emacs
;; (defvar evil-want-C-i-jump (or (daemonp) (display-graphic-p)))

;; Scrolling
(defun evil-scroll-down-other-window ()
  (interactive)
  (scroll-other-window))

(defun evil-scroll-up-other-window ()
  (interactive)
  (scroll-other-window '-))


(use-package evil
  :after evil-leader
  :init
  (setq evil-vsplit-window-right t
	evil-want-C-i-jump nil
	evil-split-window-below t)
  :config
  (evil-mode t)
  (setq evil-mode-line-format 'before)
  (setq evil-emacs-state-cursor  '("red" box))
  (setq evil-normal-state-cursor '("gray" box))
  (setq evil-visual-state-cursor '("gray" box))
  (setq evil-insert-state-cursor '("gray" bar))
  (setq evil-motion-state-cursor '("gray" box))
  ;; better ffap
  ;; (evil-define-key 'normal 'global
  ;;   ("gf" . helm-projectile-find-file-dwim))
  ;; (define-key evil-normal-state-map
  ;;   (kbd "C-S-d") 'evil-scroll-down-other-window)

  ;; (define-key evil-normal-state-map
  ;;   (kbd "C-S-u") 'evil-scroll-up-other-window)

  (define-key evil-normal-state-map "gf" 'helm-projectile-find-file-dwim)
  (define-key evil-normal-state-map "gD" 'helm-lsp-workspace-symbol)
  (define-key evil-normal-state-map "gh" 'lsp-describe-thing-at-point)
  (define-key evil-normal-state-map "-" 'dired-jump)
  (define-key evil-insert-state-map (kbd "C-@") 'company-complete)
  ;; gui mode
  (define-key evil-insert-state-map (kbd "C-SPC") 'company-complete)
  )

(use-package evil-collection
  :config
  (evil-collection-init))

(use-package evil-commentary
  :config
  (evil-commentary-mode t))

;; https://github.com/Somelauw/evil-org-mode
(use-package evil-org
  :ensure t
  :after org
  :config
  (add-hook 'org-mode-hook 'evil-org-mode)
  (add-hook 'evil-org-mode-hook
	    (lambda ()
	      (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading))))
  (require 'evil-org-agenda)
  (evil-declare-key 'normal org-mode-map ;; (evil-define-key in https://github.com/noctuid/evil-guide#binding-keys-to-keys-keyboard-macros ?
    ",c" 'org-toggle-checkbox
    ",g" 'org-open-at-point
    ",hn" 'org-insert-heading-respect-content ;; there is an evil for this?
     ">" 'org-shiftmetaright
     "<" 'org-shiftmetaleft
    (kbd "TAB") 'org-cycle)
  (evil-org-agenda-set-keys))

(use-package evil-escape
  :config
  (evil-escape-mode t)
  (setq evil-escape-key-sequence "jk")
  )

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-tabs
  :config
  (global-evil-tabs-mode t))

;; evil-tabs :q closes whole tab so this should fix it and come last
(evil-ex-define-cmd "q[uit]" 'evil-quit)

(provide 'init-evil)
