;;; EVIL
;; =====================================================================================
;; prompts for key bindings - https://github.com/justbur/emacs-which-key
;; not strictly evil, but keybiding specific
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
    "<SPC> w" "Window"))

;; You should enable global-evil-leader-mode before you enable evil-mode
(use-package evil-leader
  :init
  (setq evil-want-keybinding 'nil)
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<SPC>" 'projectile-find-file
    ";" 'helm-M-x
    "." 'ace-window
    ;; b --- buffers
    "bb" 'my|split-last-buffer
    "bl" 'helm-buffers-list
    "bk" 'my|kill-this-buffer
    "bK" 'kill-buffer
    "bj" 'evil-show-jumps
    "bp" 'evil-prev-buffer
    "bP" 'evil-split-prev-buffer
    "bn" 'evil-next-buffer
    "bN" 'evil-split-next-buffer
    "br" 'rename-buffer

    ;; e -- error
    "ef" 'my|eslint-fix-file-and-revert
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error
    "el" 'flycheck-list-errors
    "ee" 'flycheck-display-error-at-point ;; not sure?
    "eh" 'flycheck-explain-error-at-point ;; not sure?

    ;; E - flyspell
    "E" 'helm-flyspell-correct

    ;; f --- file
    "ff" 'helm-find-files
    "fr" 'helm-recentf
    "fR" 'projectile-recentf
    "F"  'org-cycle ;; TODO deal with this
    "fv" 'my|open-init-file
    "fk" 'my|delete-file-and-buffer

    ;; g -- global
    "gs" 'my|reload-init-file ;; TODO make more glorious

    ;; w -- window
    "wd" 'ace-win-delete
    "ww" 'evil-window-vsplit
    "wt" 'elscreen-toggle-display-tab
    "wk" 'delete-window
    "wK" 'elscreen-kill
    "wr" 'elscreen-screen-nickname
    "wN" 'elscreen-create
    "wl" 'elscreen-toggle
    "ws" 'ace-win-swap

    ;; s -- search
    "sf" 'helm-occur ;; great when you know what you need
    "si" 'helm-imenu ;; jump to def or explore
    "sI" 'helm-imenu-in-all-buffers ;; ideal when don't know
    "sp" 'helm-projectile-rg ;; also ag or grep
    "sl" 'xref-find-references ;; also ag or grep

    ;; n --- notes
    "nn" 'my|open-my-notes-file
    "nN" 'my|open-work-notes-file
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
    "pR" 'projectile-regenerate-tags                    ;;  "Find recent project files"
    "ps" 'helm-projectile-rg ;; also ag or grep
    "po" 'projectile-toggle-between-implementation-and-test
    "pt" 'my|test-file ;; test file in project

    ;; t --- terminal
    "tn" 'my|projectile-shell-new
    "te" 'my|projectile-shell-toggle
    "tt" 'my|projectile-term-toggle)
  )
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


;; Put this somewhere better
(global-set-key (kbd "C-M-<left>") 'frame-half-size-left)
(global-set-key (kbd "C-M-<right>") 'frame-half-size-right)
(global-set-key (kbd "C-M-<return>") 'toggle-frame-maximized)

(use-package evil
  :after evil-leader
  :init
  (setq evil-vsplit-window-right t
	evil-want-C-i-jump nil
	evil-split-window-below t
        evil-want-keybinding nil)
  :config
  (evil-mode t)
  ;; (setq evil-mode-line-format 'before)
  ;; (setq evil-emacs-state-cursor  '("red" box))
  ;; (setq evil-normal-state-cursor '("magenta" box))
  ;; (setq evil-visual-state-cursor '("red" box))
  ;; (setq evil-insert-state-cursor '("gray" bar))
  ;; (setq evil-motion-state-cursor '("HotPink2" box))

  ;; better ffap
  ;; (evil-define-key 'normal 'global
  ;;   ("gf" . helm-projectile-find-file-dwim))
  ;; (define-key evil-normal-state-map
  ;;   (kbd "C-S-d") 'evil-scroll-down-other-window)

  ;; (define-key evil-normal-state-map
  ;;   (kbd "C-S-u") 'evil-scroll-up-other-window)

  (define-key evil-normal-state-map "s" 'ace-jump-mode)
  (define-key evil-normal-state-map "S" 'ace-jump-char-mode)

  (define-key evil-normal-state-map "gf" 'helm-projectile-find-file-dwim)
  (define-key evil-normal-state-map "gD" 'helm-lsp-workspace-symbol)
  (define-key evil-normal-state-map "gh" 'lsp-describe-thing-at-point)
  ;; (define-key evil-normal-state-map "-" 'dired-jump)
  (define-key evil-insert-state-map (kbd "C-@") 'company-complete)
  ;; gui mode
  (define-key evil-insert-state-map (kbd "C-SPC") 'company-complete)

  ;; reset
  (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)

  ;; get scroll up back and replace with C-m as it's just return
  (define-key evil-normal-state-map (kbd "C-u") 'evil-scroll-up)
  (define-key evil-normal-state-map (kbd "C-y") 'universal-argument)

  ;; TODO; might want these one day
  ;; (define-key global-map (kbd "M-u") 'universal-argument)
  ;; (define-key universal-argument-map (kbd "C-u") nil)
  ;; (define-key universal-argument-map (kbd "M-u") 'universal-argument-more)

  ;; (define-key evil-normal-state-map (kbd "C-m") 'universal-argument)
  ;; remap to sexp
  (define-key evil-normal-state-map (kbd "C-M-l") 'forward-sexp)
  (define-key evil-normal-state-map (kbd "C-M-h") 'backward-sexp) ;; mark-defun
  (define-key evil-normal-state-map (kbd "C-M-k") 'backward-up-list) ;; kill-sexp
  (define-key evil-normal-state-map (kbd "C-M-j") 'down-list)

  ;; bring line into focus and attempt to show context.
  (define-key evil-normal-state-map (kbd "L") 'reposition-window)

  ;; blacklist
  (evil-set-initial-state 'shell-mode 'emacs)

  ;; org mode
  (evil-define-key 'normal org-mode-map ",c" 'org-toggle-checkbox)
  ;; - thing => - [ ] thing => - thing
  (evil-define-key 'normal org-mode-map ",lt" 'my|org-toggle-list-checkbox)
  (evil-define-key 'normal org-mode-map ",ls" 'org-sort-list)

  (evil-define-key 'normal org-mode-map ",g" 'org-open-at-point)
  (evil-define-key 'normal org-mode-map ",hh" 'org-toggle-heading)
  (evil-define-key 'normal org-mode-map ",ho" 'evil-org-insert-heading-below)
  (evil-define-key 'normal org-mode-map ",hn" 'org-insert-heading-respect-content)
  (evil-define-key 'normal org-mode-map ",s" 'org-insert-subheading)
  (evil-define-key 'normal org-mode-map ",dr" 'org-table-kill-row)
  (evil-define-key 'normal org-mode-map ",dc" 'org-table-delete-column)
  (evil-define-key 'normal org-mode-map ",ic" 'org-table-insert-column)
  (evil-define-key 'normal org-mode-map ",i-" 'org-table-insert-hline)
  (evil-define-key 'normal org-mode-map ">" 'org-shiftmetaright)
  (evil-define-key 'normal org-mode-map "<" 'org-shiftmetaleft)
  ;; move over wrapped lines
  (evil-define-key 'normal org-mode-map "j" 'evil-next-visual-line)
  (evil-define-key 'normal org-mode-map "k" 'evil-previous-visual-line)

  (evil-define-key 'normal markdown-mode-map ",c" 'markdown-toggle-markup-hiding)

  ;; move over wrapped lines
  (evil-define-key 'normal markdown-mode-map "j" 'evil-next-visual-line)
  (evil-define-key 'normal markdown-mode-map "k" 'evil-previous-visual-line))

(use-package evil-collection
  :after evil
  :config
  ;; https://github.com/emacs-evil/evil-collection/blob/master/evil-collection-dired.el
  (evil-collection-init '(dired term ansi-term)))
  ;; (setq evil-collection-mode-list 'nil))

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
  (evil-org-agenda-set-keys))

(use-package evil-escape
  :config
  (evil-escape-mode t)
  (setq evil-escape-key-sequence "jk")
  )

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

;; XXX: removing use of gh hover
;; (use-package evil-extra-operator
;;   :config
;;   (global-evil-extra-operator-mode 1))

(use-package evil-matchit
  :config
  (global-evil-matchit-mode 1))

(use-package elscreen
  :init
  (setq elscreen-display-tab nil)
  :config
  (setq elscreen-display-screen-number nil
        elscreen-default-buffer-initial-message nil
        elscreen-display-tab nil
        elscreen-tab-display-kill-screen nil
        elscreen-tab-display-control nil))

(use-package evil-tabs
  :after elscreen
  :config
  (global-evil-tabs-mode t))

;; evil-tabs :q closes whole tab so thisgg should fix it and come last
(evil-ex-define-cmd "q[uit]" 'evil-quit)

(provide 'init-evil)
