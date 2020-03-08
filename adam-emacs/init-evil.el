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
    "<SPC> F" "Fold"
    "<SPC> g" "Global"
    "<SPC> n" "Notes"
    "<SPC> p" "Projects"
    "<SPC> s" "Search"
    "<SPC> S" "Slack"
    "<SPC> t" "Term"
    "<SPC> w" "Window"
    )
  (which-key-add-major-mode-key-based-replacements 'org-mode
  ", d" "Delete"
  ", h" "Heading"
  ", i" "Insert"
  ", l" "List"
  )

  )

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
    "bj" 'evil-show-jumps
    "bk" 'my|kill-this-buffer
    "bK" 'kill-buffer
    "bl" 'helm-projectile-switch-to-buffer
    "bL" 'helm-buffers-list
    "bn" 'evil-next-buffer
    "bN" 'evil-split-next-buffer
    "bp" 'evil-prev-buffer
    "bP" 'evil-split-prev-buffer
    "br" 'rename-buffer
    "bx" 'font-lock-fontify-buffer ;; repaint the buffer

    ;; e -- error
    "ef" 'my|eslint-fix-file-and-revert
    "en" 'flycheck-next-error
    "ep" 'flycheck-previous-error
    "el" 'flycheck-list-errors
    "ee" 'flycheck-buffer
    ;; "ee" 'flycheck-display-error-at-point ;; not sure?
    "eh" 'flycheck-explain-error-at-point ;; not sure?

    ;; E - flyspell
    "E" 'helm-flyspell-correct

    ;; f --- file
    "ff" 'helm-find-files
    "fr" 'helm-recentf
    "fR" 'projectile-recentf
    "fv" 'my|open-init-file
    "fk" 'my|delete-file-and-buffer

    ;; F --- fold
    "FF"  'hs-toggle-hiding
    "FI"  'hs-hide-all
    "FO"  'hs-show-all

    ;; g -- global
    "gs" 'my|reload-init-file ;; TODO make more glorious
    "gg" 'magit-status

    ;; w -- window
    "wK" 'elscreen-kill
    "wN" 'elscreen-create
    "wd" 'ace-win-delete
    "wk" 'delete-window
    "wl" 'elscreen-toggle
    "wo" 'delete-other-windows
    "wr" 'elscreen-screen-nickname
    "ws" 'ace-win-swap
    "wt" 'elscreen-toggle-display-tab
    "ww" 'evil-window-vsplit

    ;; s -- search
    "sf" 'helm-occur ;; great when you know what you need
    "si" 'helm-imenu ;; jump to def or explore
    "sI" 'helm-imenu-in-all-buffers ;; ideal when don't know
    "sp" 'helm-projectile-rg ;; also ag or grep
    "ss" 'helm-rg ;; M-d to change dir
    "sl" 'xref-find-references ;; also ag or grep

    ;; n --- notes
    "na" 'org-agenda
    "nb" 'org-switchb
    "nc" 'org-capture
    "nh" 'helm-org-agenda-files-headings ;; search through headings
    "nl" 'org-store-link
    "nn" 'my|open-my-notes-file
    "nN" 'my|open-work-notes-file

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

    ;; S -- slack
    "SS" 'slack-im-select
    "Su" 'helm-slack-unreads
    "Sk" 'helm-slack ;; quite slow to load all groups

    ;; t --- terminal
    "tn" 'my|projectile-shell-new
    "te" 'my|projectile-shell-toggle
    "tt" 'my|projectile-term-toggle
    ;; r --- run
    "r" 'my|run-ruby
    )
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

;; window adjustments
(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; this exists to match shortcut on mac
(global-set-key (kbd "C-s-<f8>") 'my|close-notifications-mac)

;; TODO: investigate, https://www.reddit.com/r/emacs/comments/4ud5h4/how_do_i_use_helmcommandprefix_while_creating/
;; (define-prefix-command 'my-helm-commands)
;; (define-key my-helm-commands "b" 'helm-buffers-list)
;; (define-key my-helm-commands "f" 'helm-find-files)
;; (define-key global-map "C-c h" my-helm-commands)

(use-package evil
  :after evil-leader
  :init
  (setq evil-vsplit-window-right t
	evil-want-C-i-jump nil
	evil-split-window-below t
        evil-want-keybinding nil)
  :bind
  (:map evil-insert-state-map
	("C-@" . company-complete)
	;; gui mode
	("C-SPC" . company-complete)
	)
  (:map evil-normal-state-map
	("s" . ace-jump-mode)
	("S" . ace-jump-char-mode)
	("gf" . helm-projectile-find-file-dwim)
	("gD" . evil-goto-definition)
	;; ("gd" . lsp-goto-implementation)
	("gd" . lsp-find-definition)
	("gh" . lsp-describe-thing-at-point)
	;; ("C-o" . xref-pop-marker-stack)
	;; (define-key evil-normal-state-map "-" 'dired-jump)
	("C-@" . company-complete)
	;; gui mode
	("C-SPC" . company-complete)
	;; reset
	("C-e" . move-end-of-line)
	;; get scroll up back and replace with C-m as it's just return
	("C-u" . evil-scroll-up)
	("C-y" . universal-argument)
	("L" . reposition-window))
  :config
  ;; https://emacs.stackexchange.com/questions/9583/how-to-treat-underscore-as-part-of-the-word
  (defadvice evil-inner-word (around underscore-as-word activate)
    (let ((table (copy-syntax-table (syntax-table))))
      (modify-syntax-entry ?_ "w" table)
      (with-syntax-table table
	ad-do-it)))
  (evil-mode t)
  ;; (setq evil-mode-line-format 'before)
  ;; (setq evil-emacs-state-cursor  '("red" box))
  ;; (setq evil-normal-state-cursor '("magenta" box))
  ;; (setq evil-visual-state-cursor '("red" box))
  ;; (setq evil-insert-state-cursor '("gray" bar))
  ;; (setq evil-motion-state-cursor '("HotPink2" box))

  ;; (define-key evil-normal-state-map
  ;;   (kbd "C-S-u") 'evil-scroll-up-other-window)
  (define-key universal-argument-map (kbd "C-y") 'universal-argument-more)

  ;; remap to sexp
  (define-key evil-normal-state-map (kbd "C-M-l") 'forward-sexp)
  (define-key evil-normal-state-map (kbd "C-M-h") 'backward-sexp) ;; mark-defun
  (define-key evil-normal-state-map (kbd "C-M-k") 'backward-up-list) ;; kill-sexp
  (define-key evil-normal-state-map (kbd "C-M-j") 'down-list)
  ;; bring line into focus and attempt to show context.
  ;; blacklist
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  ;; web-mode
  ;; (define-key js2-refactor-mode-map (kbd "C-c C-e C-f") 'js2r-extract-function)
  ;; (evil-define-key 'normal js2-refactor-mode-map ",c" 'org-toggle-checkbox)
  ;; org mode
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
  (evil-define-key 'normal org-mode-map ",c" 'org-toggle-checkbox)
  ;; - thing => - [ ] thing => - thing
  (evil-define-key 'normal org-mode-map ",lt" 'my|org-toggle-list-checkbox)
  (evil-define-key 'normal org-mode-map ",ls" 'org-sort-list)

  (evil-define-key 'normal org-mode-map ",g" 'org-open-at-point)
  (evil-define-key 'normal org-mode-map ",hh" 'org-toggle-heading)
  (evil-define-key 'normal org-mode-map ",ho" 'evil-org-insert-heading-below)
  (evil-define-key 'normal org-mode-map ",hn" 'org-insert-heading-respect-content)
  (evil-define-key 'normal org-mode-map ",hs" 'org-insert-subheading)
  (evil-define-key 'normal org-mode-map ",dr" 'org-table-kill-row)
  (evil-define-key 'normal org-mode-map ",dc" 'org-table-delete-column)
  (evil-define-key 'normal org-mode-map ",ic" 'org-table-insert-column)
  (evil-define-key 'normal org-mode-map ",i-" 'org-table-insert-hline)
  (evil-define-key 'normal org-mode-map ">" 'org-shiftmetaright)
  (evil-define-key 'normal org-mode-map "<" 'org-shiftmetaleft)
  (evil-define-key 'normal org-mode-map ",s" 'org-sort)
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
