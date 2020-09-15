(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

(setq package-enable-at-startup nil)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents) (package-install 'use-package))
(eval-when-compile (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(declare-function exec-path-from-shell-initialize "exec-path-from-shell.el")

;; OSX specific code
(when (eq system-type 'darwin)
  ;; On OS X Emacs doesn't use the shell PATH if it's not started from
  ;; the shell. Let's fix that:
  (require 'exec-path-from-shell)
  (setq exec-path-from-shell-arguments '("-l"))
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "SPOTIFY_TOKEN" "SLACK_SKY_EMACS_TOKEN"))
  (exec-path-from-shell-initialize)


  (setq ns-function-modifier 'hyper) ;; fix alt as meta key

  (menu-bar-mode +1)

  ;; Enable emoji, and stop the UI from freezing when trying to display them.
  (if (fboundp 'set-fontset-font)
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))

;; y or n instead of yes etc
(defalias 'yes-or-no-p 'y-or-n-p)

;; close buffers without confirm
(setq kill-buffer-query-functions
       (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

;; Backup and Autosave Directories
(setq temporary-file-directory "~/tmp/")
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq indent-tabs-mode nil
      font-lock-maximum-decoration 3

      ;; highlight parens
      show-paren-mode t

      ;; reduce the frequency of garbage collection by making it happen on
      ;; each 50MB of allocated data (the default is on every 0.76MB)
      gc-cons-threshold 50000000

      ;; warn when opening files bigger than 100MB
      large-file-warning-threshold 100000000
      vc-follow-symlinks t

      help-window-select t
      ;; always split vertically if there's room
      ;; split-width-threshold 170
      ;; Split horizontally when opening a new window from a command
      split-height-threshold nil
      ring-bell-function #'ignore
      visible-bell t
      show-paren-mode 1
      window-resize-pixelwise t
      save-abbrevs 'silently
      frame-resize-pixelwise t
      ; auto-save-file-name-transforms `((".*" . "~/.emacs-file-saves")) ; store all backup files in home directory
      backup-by-copying t ; slow but sure way of saving
      ;; If that's too slow for some reason you might also
      ;; have a look at backup-by-copying-when-linked
      ;; https://stackoverflow.com/questions/151945/how-do-i-control-how-emacs-makes-backup-files
      version-control t                 ; version numbers for backup files
      dired-listing-switches "-lat" ; list, all, alphabetical
      delete-old-versions t ; delete excess backup files silently
      compilation-scroll-output t
      )

(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;; (setq ns-use-proxy-icon nil) ;; not sure why undefined

;; Set symbol for the border
(set-display-table-slot standard-display-table
                        'vertical-border
                        (make-glyph-code ?┃))

(setq frame-title-format nil)

(if (eq system-type 'darwin)
  (set-face-attribute 'default nil :height 140)
  (set-face-attribute 'default nil :height 160)) ;; larger font on linux setup

;;; functions
;; ==================================================================================

;; stolen from crux https://github.com/bbatsov/crux/blob/master/crux.el#L347
(defun my|delete-file-and-buffer ()
  "Kill the current buffer and deletes the file it is visiting."
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
          (vc-delete-file filename)
        (progn
          (delete-file filename)
          (message "Deleted file %s" filename)
          (kill-buffer))))))

;; (defun my|md-link-to-org ()
;;   ;; Can also be adapted to use the region, but one would need to add
;;   ;; a marker and region-end.  Remember to remove marker at end.
;;   (let ((markdown-regex-link-inline
;;          ;; from http://jblevins.org/git/markdown-mode.git/tree/markdown-mode.el
;;           "\\(!\\)?\\(\\[\\)\\([^]^][^]]*\\|\\)\\(\\]\\)\\((\\)\\([^)]*?\\)\\(?:\\s-+\\(\"[^\"]*\"\\)\\)?\\()\\)"))
;;     (while (search-forward-regexp markdown-regex-link-inline (point-max) t)
;;       (replace-match "[[\\6][\\3]]"))))

;; https://www.reddit.com/r/emacs/comments/64xb3q/killthisbuffer_sometimes_just_stops_working/
(defun my|kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun my|open-init-file ()
  "Open init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun my|replace-in-string (WHAT WITH in)
  "`WHAT' to be replaced with `WITH' `IN' string."
  (replace-regexp-in-string (regexp-quote WHAT) WITH in nil 'literal))

(defun frontside-windowing-adjust-split-width-threshold ()
  "Change the value of `split-width-threshold' so that it will cause the screen
split once and only once.

For example, if the frame is 360 columns wide, then we want the
split-width-threshold to be 181. That way, when you split horizontally, the two
new windows will each be 180 columns wide, and sit just below the threshold.
"
  (setq split-width-threshold (+ 1 (/ (frame-width) 2))))

;; recaculate split-width-threshold with every change
(add-hook 'window-configuration-change-hook
          'frontside-windowing-adjust-split-width-threshold)

(defadvice delete-window (after restore-balance activate)
  "Balance deleted windows."
  (balance-windows))

;; horizontal split, switch window, and open next buffer
(defun my|split-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

(defun frame-half-size-left ()
  "Set the current frame to half the screen width."
  (interactive)
  (let ((frame (selected-frame))
        (one-half-display-pixel-width (/ (display-pixel-width) 2)))
    (set-frame-width frame one-half-display-pixel-width nil 'pixelwise)
    (set-frame-position frame 0 0)))

(defun frame-half-size-right ()
  "Set the current frame to half the screen width."
  (interactive)
  (let ((frame (selected-frame))
        (one-half-display-pixel-width (/ (display-pixel-width) 2)))
    (set-frame-width frame one-half-display-pixel-width nil 'pixelwise)
    (set-frame-position frame one-half-display-pixel-width 0)))

(defun my|close-notifications-mac ()
  "Close Mac notifications."
  (interactive)
  (message "closing notifications")
  (save-window-excursion
    (async-shell-command
     (concat "automator ~/Library/services/Close\\ all\\ notifications.workflow"))))

(defun my|pomo ()
  "Start a pomodoro timer in the background."
  (interactive)
  (message "starting 25min timer")
  (save-window-excursion
    (async-shell-command
     (concat "pomo") "pomo-timer")))

(defun my|pomo-stop ()
  "Finish existing pomo timer."
  (interactive)
  (message "stopping pomo timer")
  (when (get-buffer "pomo-timer")
    (kill-buffer "pomo-timer"))
  (save-window-excursion
    (async-shell-command
     (concat "pomo --complete"))))

(defun my|my-save-word ()
  (interactive)
  (let ((current-location (point))
        (word (flyspell-get-word)))
    (when (consp word)    
      (flyspell-do-correct 'save nil (car word) current-location (cadr word) (caddr word) current-location))))

(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)
        (dired-sort-toggle-or-edit)))

;; TODO still getting there
(defun my|replace-word-under-cursor ()
  "Replace word under cursor."
  (interactive)
  (print (thing-at-point 'word)))

(global-set-key (kbd "C-q") 'my|replace-word-under-cursor)

(defun exercism-submit ()
  "Submit current file."
  (interactive)
  (async-shell-command (concat "exercism submit " (buffer-file-name))))

(define-minor-mode exercism-mode
  "Toggle exercism mode.
     Interactively with no argument, this command toggles the mode.
     A positive prefix argument enables the mode, any other prefix
     argument disables it.  From Lisp, argument omitted or nil enables
     the mode, `toggle' toggles the state.
     
     When Exercism mode is enabled, A few things are bound to C-c.
     See the command \\[exercism-submit]."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " exercism"
  ;; The minor mode bindings.
  '(([C-c C-e s] . exercism-submit))
  :group 'exercism)

;;; packages
;; ==================================================================================
(use-package restart-emacs)

(use-package dired-k
  :hook (dired-mode . dired-k)
  :config
  (setq dired-k-padding 5))

(use-package hydra)

;; too slow
;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook)
;;   (setq dashboard-startup-banner 'logo)
;;   (setq dashboard-set-file-icons t)
;;   (setq dashboard-set-navigator t))

(defhydra hydra-zoom (global-map "<f2>")
  "zoom"
  ("g" text-scale-increase "in")
  ("l" text-scale-decrease "out"))

(defhydra hydra-window (:color red
                        :hint nil)
  "
Delete: _o_nly  _da_ce  _dw_indow  _db_uffer  _df_rame
  Move: _s_wap
Frames: _f_rame new  _df_ delete
  Misc: _m_ark _a_ce  _u_ndo  _r_edo"
  ("h" shrink-window-horizontally)
  ("j" shrink-window)
  ("k" enlarge-window)
  ("l" enlarge-window-horizontally)
  ("H" hydra-move-splitter-left)
  ("J" hydra-move-splitter-down)
  ("K" hydra-move-splitter-up)
  ("L" hydra-move-splitter-right)
  ;("t" transpose-frame "'")
  ;; winner-mode must be enabled
  ("u" winner-undo)
  ("r" winner-redo) ;;Fixme, not working?
  ("o" delete-other-windows :exit t)
  ("a" ace-window :exit t)
  ("f" new-frame :exit t)
  ("s" ace-swap-window)
  ("da" ace-delete-window)
  ("dw" delete-window)
  ("db" kill-this-buffer)
  ("df" delete-frame :exit t)
  ("q" nil)
  ;("i" ace-maximize-window "ace-one" :color blue)
  ;("b" ido-switch-buffer "buf")
  ("m" headlong-bookmark-jump))

(use-package delight)

;; adds highlights to TODO and FIXME.
(use-package fic-mode
  :hook
  (prog-mode)
  ;; (web-mode)
  :config
  (custom-set-faces
   '(fic-face ((t (:inherit warning :weight bold))))))

;; jump to def without lsp
(use-package xref
  :config
  (setq xref-prompt-for-identifier 'nil))

(use-package xclip
  :config
  (xclip-mode 1))

(use-package vterm)

(use-package multi-vterm
  :after ('vterm . 'evil)
  :config
  (add-hook 'vterm-mode-hook
	    (lambda ()
	      (setq-local evil-insert-state-cursor 'box)
	      (evil-insert-state)))
  (define-key vterm-mode-map [return]                      #'vterm-send-return)

  (setq vterm-keymap-exceptions nil))

(use-package flyspell
  :config
  (setq ispell-program-name "/usr/local/bin/aspell"))

(use-package flyspell-correct-ivy
  :after ('flyspell . 'ivy))

(use-package autorevert
  :delight auto-revert-mode)

(use-package eldoc
  :delight)

(use-package editorconfig
  :delight
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

(use-package undo-tree
  :delight
  :config
  (setq undo-tree-auto-save-history t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package magit)

(use-package ace-jump-mode
  :config
  (autoload 'ace-jump-mode-pop-mark "Ace jump back:-)" t))

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
  ;; Remove the mode name for projectile-mode, but show the project name.
  :delight '(:eval (concat " " (projectile-project-name)))
  :config
  (setq projectile-completion-system 'ivy)
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

  (projectile-register-project-type 'gradle '("build.gradle")
				    :compile "npm i"
				    :test "npm test"
				    :run "npm start"
				    :test-suffix "Test")

  (projectile-register-project-type 'sbt '("build.sbt")
				    :compile "sbt compile"
				    :test "sbt test"
				    :test-suffix "Test")
  (defun my|test-file-ts ()
    "Run tests on current typescript file."
    (interactive)
    (message (concat "testing " (buffer-file-name)))
    (save-buffer)
    (async-shell-command
     (concat "cd " (projectile-project-root) " && node_modules/.bin/jest --config='./.jest/jest.all.config.js' " (buffer-file-name))))

  (defun my|test-file ()
    "Run tests on current file."
    (interactive)
    (message (concat "testing " (buffer-file-name)))
    (save-buffer)
    (async-shell-command
     (concat "cd " (projectile-project-root) " && node_modules/.bin/jest " (buffer-file-name) " --watch --collectCoverageOnlyFrom " (my|replace-in-string ".spec.js" ".jsx" buffer-file-name))))

  (defun my|test-file-mocha ()
    "Run tests on current file."
    (interactive)
    (message (concat "testing " (buffer-file-name)))
    (save-buffer)
    (async-shell-command
     (concat "cd "
	     (projectile-project-root)
	     ;; " && NODE_ENV=test node_modules/.bin/mocha --config=test/unit/.mocharc.js --chuftey "
	     " && NODE_ENV=test node_modules/.bin/mocha --config ./test/unit/.mocharc.js -w "
	     (buffer-file-name)
	     )))

  (defun my|eslint-fix-file ()
    "Run eslint --fix on current file."
    (interactive)
    (message (concat "eslint --fixing " (buffer-file-name) " using"))
    (save-buffer)
    (async-shell-command
     (concat "cd " (projectile-project-root) " && node_modules/eslint/bin/eslint.js"
	     (cond ((file-exists-p "./.eslintrc.js") " --config ./.eslintrc.js")
		   ((file-exists-p "./.eslintrc.yml") " --config ./.eslintrc.yml"))
	     " --fix " (buffer-file-name))))

  (defun my|prettier-fix-file ()
    "Run prettier on current file."
    (interactive)
    (message (concat "prettier --writing " (buffer-file-name) " using"))
    (save-buffer)
    (async-shell-command
     (concat
      "cd " (projectile-project-root) " && node_modules/.bin/prettier --write " (buffer-file-name)))
    (revert-buffer t t))

  ;; (defun my|stylelint-fix-file ()
  ;;   "Run eslint --fix on current file."
  ;;   (interactive)
  ;;   (save-buffer)
  ;;   (shell-command
  ;;    (concat "cd " (projectile-project-root) " && node_modules/stylelint/bin/stylelint.js --syntax scss --custom-formatter='./scripts/lint/stylelint-formatter' --fix " (buffer-file-name))))


  (defun my|stylelint-fix-file ()
    "Run stylelint --fix on current file."
    (interactive)
    (save-buffer)
    (shell-command
     (concat "cd " (projectile-project-root) " && node_modules/stylelint/bin/stylelint.js --syntax scss --fix " (buffer-file-name))))

  (defun my|run-ruby ()
    (interactive)
    (save-buffer)
    (async-shell-command (concat "ruby " (buffer-file-name))))

  (defun my|projectile-shell-new ()
    "Create an `eshell' at the project root."
    (interactive)
    (progn
      (split-window-sensibly (selected-window))
      (other-window 1)
      (setq default-directory (projectile-project-root))
      (eshell (getenv "SHELL"))))


  (defun my|eslint-fix-file-and-revert ()
    (interactive)
    (my|eslint-fix-file)
    (revert-buffer t t)))

(use-package ivy :demand
  :delight
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t
	ivy-count-format "%d/%d "))

(use-package swiper
  :bind ("C-s". swiper))

(use-package counsel
  :bind (
	 ("C-x C-f" . counsel-find-file)))

(use-package counsel-projectile
  :bind (("C-\\" . counsel-projectile-rg)))

;; good for project wide search, has nice buffer and toggles
(use-package deadgrep)

;; simple search which starts in current dir and can be expanded out
(use-package ripgrep)

(use-package yasnippet
  :config (yas-global-mode))
  ;; :config
  ;; (yas-reload-all)
  ;; (add-hook 'scala-mode-hook #'yas-minor-mode))

;; added this to get typescript to work in org-babel
(use-package ob-typescript)

(use-package org
  :init
  (defvar org-directory "~/org-notes/")
  (defvar org-work-directory (concat org-directory "org-sky-notes/"))
  (defvar org-me-directory (concat org-directory "org-me-notes/"))
  (defvar org-default-notes-file (concat org-directory "/capture.org"))
  (defvar org-work-file (concat org-work-directory "/work.org"))
  (defvar org-personal-file (concat org-me-directory "/notes.org"))
  ;; (defvar org-agenda-files (org-personal-file org-default-notes-file org-work-file))
  (defvar org-agenda-files '("~/org-notes/capture.org"
			     ;; "~/org-notes/org-me-notes/notes.org"
			     "~/org-notes/org-sky-notes/work.org"))
  :commands
  (my|open-work-notes-file
   my|open-my-notes-file)
  :hook
  (org-mode . visual-line-mode)
  (org-mode . flyspell-mode)
  (org-mode . abbrev-mode)
  :config
  (require 'org-tempo) ;; needed to add this to get template expansion to work again
  ;; set scratch buffer to org mode
  ;; (setq initial-major-mode 'org-mode)

  (setq org-startup-indented t
	org-fontify-done-headline t
	org-fontify-whole-heading-line t
        org-hide-leading-stars t
        org-hide-block-startup t
	org-hide-emphasis-markers t
	org-startup-folded t
	org-log-done 'time
	org-ellipsis " ▾"
        org-image-actual-width nil ; allows images to be resized with #+ATTR_ORG: :width 100
	org-agenda-span 8)

  (setq org-todo-keywords
      '((sequence "TODO(t)" "PROGRESS(p)" "|" "DONE(d)")))

  ;; seems broken
  (defun my|org-toggle-list-checkbox ()
    (interactive)
    (org-toggle-checkbox 4))

  (defun my|open-my-notes-file ()
    "Open rough notes."
    (interactive)
    (find-file org-personal-file))

  (defun my|open-work-notes-file ()
    "Open work notes."
    (interactive)
    (find-file org-work-file))

  (defun my|list-to-checkbox (arg)
    (interactive "P")
    (let ((n (or arg 1)))
      (when (region-active-p)
	(setq n (count-lines (region-beginning)
			     (region-end)))
	(goto-char (region-beginning)))
      (dotimes (i n)
	(beginning-of-line)
	(re-search-forward "- " nil t)
	(replace-match "- [ ] ")
	(forward-line))
      (beginning-of-line)))

  ;; (defun org-babel-execute:typescript (body params)
  ;;   (let ((org-babel-js-cmd "npx ts-node < "))
  ;;     (org-babel-execute:js body params)))

  (org-babel-do-load-languages
   'org-babel-load-languages '((shell . t) ; allow bash
			       (js . t)
			       (typescript . t)
			       (haskell . t)
			       (ruby . t)
			       (io . t)))

  ;; Highlight done todos with different colors.
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
   'append)

  (setq org-capture-templates
	'(("t" "TODO" entry (file+headline org-default-notes-file "Collect")
	   "* TODO %? %^G \n  %U" :empty-lines 1)
	  ("s" "Scheduled TODO" entry (file+headline org-default-notes-file "Collect")
	   "* TODO %? %^G \nSCHEDULED: %^t\n  %U" :empty-lines 1)
	  ("d" "Deadline" entry (file+headline org-default-notes-file "Collect")
	   "* TODO %? %^G \n  DEADLINE: %^t" :empty-lines 1)
	  ("p" "Priority" entry (file+headline org-default-notes-file "Collect")
	   "* TODO [#A] %? %^G \n  SCHEDULED: %^t")
	  ("a" "Appointment" entry (file+headline org-default-notes-file "Collect")
	   "* %? %^G \n  %^t")
	  ("l" "Link" entry (file+headline org-default-notes-file "Tasks")
	   "* TODO LINK %?\n  %u\n  %a")
	  ("n" "Note" entry (file+headline org-default-notes-file "Notes")
	   "* %? %^G\n%U" :empty-lines 1)
	  ("j" "Journal" entry (file+datetree org-default-notes-file)
	   "* %? %^G\nEntered on %U\n")))
  (setq org-agenda-custom-commands
	'(("p" todo "PROGRESS")
	  ("w" . "SKYPORT+Name tags searches") ; description for "w" prefix
	  ("ws" tags "+skyport+graphql")
	  ("u" "Unscheduled TODO"
	   ((todo ""
		  ((org-agenda-overriding-header "\nUnscheduled TODO")
		   (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled 'timestamp)))))
	   nil
	   nil)
	  ("a" "adenda and things"
	   ((agenda)
	    (todo "PROGRESS")
	    (tags-todo "work")
	    (tags-todo "skyport")))))
  )

;; only show bullets in gui
(if window-system
    (use-package org-bullets
      :commands org-bullets-mode
      :hook (org-mode . org-bullets-mode)
      :custom
      (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●"))))

(use-package org-download)

(use-package org-alert
  :custom (alert-default-style 'osx-notifier)
  :config
  (setq org-alert-interval 300)
  (org-alert-enable))

(use-package markdown-mode
  :hook
  (markdown-mode . flyspell-mode)
  (markdown-mode . visual-line-mode))

(use-package markdown-toc)

(use-package wakatime-mode
  :delight
  :config
  (setq wakatime-cli-path "/usr/local/bin/wakatime")
  (global-wakatime-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; (use-package rjsx-mode
;;   :config
;;   (add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
;;   (add-to-list 'interpreter-mode-alist '("node" . rjsx-mode)))

(use-package yaml-mode)

(use-package dockerfile-mode)

(use-package docker-compose-mode)

(use-package protobuf-mode)

(use-package json-mode)

(use-package graphql-mode
  :config
  (add-to-list 'auto-mode-alist '("\\.gql\\'" . graphql-mode)))

(use-package ob-graphql)

(use-package go-mode
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  ;; (add-hook 'go-mode-hook 'my|go-checkers)

  (defun my|go-checkers ()
    "Use gofmt despite lsp's enthusiasm"
    (interactive)
    (setq-local flycheck-checker 'go-gofmt)))

(use-package web-mode
  :init
  ;; (add-to-list 'auto-mode-alist '("\\.js\\'" . web-mode))
  ;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.ts\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  :config
  (setq-default web-mode-comment-formats
              '(("javascript" . "//")
                ("typescript" . "//")))

  ;; (setq web-mode-content-types-alist
  ;; 	'(("jsx"  . ".*\\.js[x]?\\'")))
  ;; (add-hook 'web-mode-hook 'my|web-checkers)
  (add-hook 'web-mode-hook 'my|web-mode-settings)

  (defun my|web-mode-settings ()
    "Hooks for Web mode."
    (setq web-mode-enable-auto-closing t)
    (setq web-mode-enable-auto-quoting nil)
    (setq web-mode-markup-indent-offset 2))

  (defun my|web-checkers ()
    "Use eslint despite lsp's enthusiasm"
    (interactive)
    (setq-local flycheck-checker 'javascript-eslint)))

(use-package add-node-modules-path
  :init
  :hook
  (web-mode)
  (js-mode)
  (rjsx-mode))

(use-package rust-mode
  :config
  (setq rust-format-on-save t))

(use-package cargo
  :hook (rust-mode . cargo-minor-mode))

(use-package scala-mode
  :mode "\\.s\\(cala\\|bt\\)$")

(use-package sbt-mode
  :commands sbt-start sbt-command
  :bind
  (:map scala-mode-map
	("C-c C-x" . sbt-run-previous-command))
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false")))

(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package lsp-mode
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :init (setq lsp-keymap-prefix "s-l")
  :hook
  (prog-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp-deferred
  :config
  ;; still a bit shakey on this
  (setq lsp-enable-file-watchers 'nil)
  (setq lsp-diagnostics-provider :flymake)
  ;; (setq lsp-eslint-server-command 
  ;;  '("node" 
  ;;    "/Users/adh23/.vscode/extensions/dbaeumer.vscode-eslint-2.1.5/server/out/eslintServer.js" 
  ;;    "--stdio"))
  (setq lsp-enable-snippet 'nil))

(use-package dap-mode
  :after lsp-mode
  :config (dap-auto-configure-mode))

(use-package lsp-java
  :after lsp-mode
  :hook (java-mode . lsp))

(use-package dap-java
  :ensure nil)

(use-package gradle-mode)

(use-package lsp-metals
  :disabled
  :after lsp-mode)

(use-package glsl-mode)

;; (use-package flycheck
;;   :after lsp-mode
;;   :init (global-flycheck-mode)
;;   :config
;;   ;; (flycheck-add-mode 'javascript-eslint 'web-mode)
;;   ;; (flycheck-add-next-checker 'javascript-eslint 'lsp))

;;   ;; go-build is last in the checkers, so then finish with lsp
;;   ;; (flycheck-add-next-checker 'go-build 'lsp))
;;   ;; (add-hook 'go-mode-hook
;;   ;; 	    (lambda () (flycheck-add-next-checker 'lsp 'go-vet))))
;;   ;; (add-hook 'web-mode-hook
;;   ;; 	    (lambda () (flycheck-add-next-checker 'lsp 'javascript-eslint))))
;;   ;; (add-hook 'web-mode-hook
;;   ;; 	    (lambda () (flycheck-select-checker 'javascript-eslint)))
;;   (add-hook 'js-mode-hook
;; 	    (lambda () (flycheck-select-checker 'javascript-eslint)))
;;   )

;; (use-package dired+
;;   :config
;;   (toggle-diredp-find-file-reuse-dir 1))


(use-package elscreen
  :after (evil)
  :demand t
  :init
  (defhydra hydra-tabs
    (:color red :hint nil
	    :pre (setq elscreen-display-tab t)
	    :post (setq elscreen-display-tab nil))
    "tabs"
    ("l" elscreen-next "next")
    ("h" elscreen-previous "previous")
    ("j" nil "quit" :color blue)
    ("n" elscreen-create "new" :color blue))
  :config
  (elscreen-start)

  (setq elscreen-display-screen-number nil
	elscreen-default-buffer-initial-message nil
	elscreen-display-tab nil
	elscreen-tab-display-kill-screen nil
	elscreen-tab-display-control nil)

  ;; light theme
  ;; :custom-face
  ;; (elscreen-tab-background-face ((t (:background "#dfdfdf" :height 1.3))))
  ;; (elscreen-tab-current-screen-face ((t (:background "#fafafa" :foreground "#a626a4"))))
  ;; (elscreen-tab-other-screen-face ((t (:background "#dfdfdf" :foreground "#a190a7"))))

  :custom-face
  (elscreen-tab-background-face ((t (:background "#1c1f24" :height 1.3))))
  (elscreen-tab-current-screen-face ((t (:background "#282c34" :foreground "#c678dd"))))
  (elscreen-tab-other-screen-face ((t (:background "#1c1f24" :foreground "#a190a7")))))

;; EVIL
;; --------------------------------------------------------
(use-package which-key
  :config
  (which-key-mode t)
  (which-key-add-key-based-replacements
    "<SPC> b" "Buffers"
    "<SPC> e" "Errors"
    "<SPC> E" "Spelling"
    "<SPC> f" "Files"
    "<SPC> F" "Fold"
    "<SPC> g" "Global"
    "<SPC> n" "Notes"
    "<SPC> p" "Projects"
    "<SPC> s" "Search"
    "<SPC> S" "Slack"
    "<SPC> t" "Term"
    "<SPC> w" "Window"
    "<SPC> W" "Layouts"
    )
  (which-key-add-major-mode-key-based-replacements 'org-mode
  ", d" "Delete"
  ", h" "Heading"
  ", i" "Insert"
  ", l" "List"
  ))

(use-package evil-leader
  :init
  (setq evil-want-keybinding 'nil)
  :config
  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "<SPC>" 'counsel-projectile-find-file
    ";" 'counsel-M-x
    ":" 'counsel-command-history
    "." 'ace-window

    ;; b --- buffers
    "bb" 'my|split-last-buffer
    "bj" 'evil-show-jumps
    "bk" 'my|kill-this-buffer
    "bK" 'kill-buffer
    "bl" 'counsel-projectile-switch-to-buffer
    "bL" 'counsel-ibuffer
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
    "ei" 'flycheck-verify-setup

    ;; E - flyspell
    "EE" 'flyspell-correct-wrapper
    "ES" 'my|my-save-word
    "EB" 'flyspell-buffer
    "EP" 'evil-prev-flyspell-error
    "EN" 'evil-next-flyspell-error

    ;; f --- File
    "ff" 'counsel-find-file
    "fr" 'counsel-recentf
    "fv" 'my|open-init-file
    "fk" 'my|delete-file-and-buffer
    "fs" 'save-buffer

    ;; F --- fold
    "FF"  'hydra-hs/body
    "FO"  'hydra-folding/body

    ;; g -- global
    "gs" 'my|reload-init-file ;; TODO make more glorious
    "gg" 'magit-status
    "gp" 'my|pomo
    "gP" 'my|pomo-stop
    "gl" 'counsel-find-library

    ;; w -- window
    "wK" 'elscreen-kill
    "we" 'hydra-tabs/body
    "wN" 'elscreen-create
    "wd" 'ace-win-delete
    "wk" 'delete-window
    "wl" 'elscreen-toggle
    "wo" 'delete-other-windows
    "wr" 'elscreen-screen-nickname
    "ws" 'ace-win-swap
    "wt" 'elscreen-toggle-display-tab
    "ww" 'evil-window-vsplit

    ;; W -- window configurations
    "WW" 'ivy-push-view
    "WD" 'ivy-pop-view
    "Wl" 'ivy-switch-view

    ;; s -- search
    "sf" 'swiper			;; great when you know what you need
    "si" 'counsel-imenu			;; jump to def or explore
    "sI" 'helm-imenu-in-all-buffers	;; ideal when don't know
    "sp" 'deadgrep			;; also ag or grep
    "ss" 'ripgrep-regexp		;; search from current dir out
    "sl" 'xref-find-references		;; also ag or grep

    ;; n --- notes
    "na" 'org-agenda
    "nb" 'org-switchb
    "nc" 'org-capture
    "nh" 'helm-org-agenda-files-headings ;; search through headings
    "nl" 'org-store-link
    "nn" 'my|open-my-notes-file
    "nN" 'my|open-work-notes-file

    ;; p --- project
    "p!" 'projectile-run-shell-command-in-root		;;  "Run cmd in project root"
    "pa" 'projectile-add-known-project			;;  "Add new project"
    "pb" 'projectile-switch-to-buffer			;;  "Switch to project buffer"
    "pc" 'projectile-compile-project			;;  "Compile in project"
    "pd" 'projectile-find-dir				;;  "Remove known project"
    "pe" 'projectile-edit-dir-locals			;;  "Edit project .dir-locals"
    "pf" 'counsel-projectile-find-file			;;  "Find file in project"
    "pF" 'projectile-find-file-in-known-projects	;;  "Find file in project"
    "pg" 'projectile-find-file-dwim			;;  "Find file in project at point better ffap"
    "pi" 'projectile-invalidate-cache			;;  "Invalidate project cache"
    "pk" 'projectile-kill-buffers			;;  "Kill project buffers"
    "po" 'projectile-toggle-between-implementation-and-test
    "pO" 'projectile-find-other-file			;;  "Find other file"
    "pp" 'counsel-projectile-switch-project		;;  "Switch project"
    "pr" 'projectile-recentf				;;  "Find recent project files"
    "pR" 'projectile-regenerate-tags			;;  "Find recent project files"
    "ps" 'deadgrep					;;  "Search with rg"
    "pt" 'my|test-file					;; test file in project

    ;; S -- slack
    "SS" 'slack-im-select
    "Su" 'helm-slack-unreads
    "Sk" 'helm-slack ;; quite slow to load all groups

    ;; t --- terminal
    "tn" 'multi-vterm
    "tt" 'multi-vterm-projectile

    ;; r --- run
    "r" 'hydra-window/body
    ;; "r" 'my|run-ruby

    ))

(global-set-key (kbd "C-s-<f8>") 'my|close-notifications-mac)

(use-package evil
  :after evil-leader
  :init
  (setq evil-vsplit-window-right t
	evil-want-C-i-jump nil
	evil-split-window-below t
        evil-want-keybinding nil)
  :bind
  (:map evil-insert-state-map
	("C-@" . completion-at-point)
	;; gui mode
	("C-SPC" . completion-at-point)
	)
  (:map evil-normal-state-map
	("-"		. (lambda () (interactive) (dired ".")))
	;; ("-"		. (lambda () (interactive) (fild-alternate-file "..")))
	("s"		. ace-jump-mode)
	("S"		. ace-jump-char-mode)
	("gf"		. projectile-find-file-dwim)
	("gD"		. evil-goto-definition)
	("gd"		. lsp-find-definition)
	("gh"		. lsp-describe-thing-at-point)
	("C-@"		. completion-at-point)
	("C-SPC"	. completion-at-point) ; gui mode
	("C-e"		. move-end-of-line) ; reset
	("C-u"		. evil-scroll-up) ; get scroll up back and replace with C-m as it's just return
	("C-y"		. universal-argument)
	("L"		. reposition-window))
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
  ;; (define-key evil-normal-state-map (kbd "C-M-l") 'forward-sexp)
  ;; (define-key evil-normal-state-map (kbd "C-M-h") 'backward-sexp) ;; mark-defun
  ;; (define-key evil-normal-state-map (kbd "C-M-k") 'backward-up-list) ;; kill-sexp
  ;; (define-key evil-normal-state-map (kbd "C-M-j") 'down-list)
  ;; bring line into focus and attempt to show context.
  ;; blacklist
  (evil-set-initial-state 'shell-mode 'emacs)
  (evil-set-initial-state 'help-mode 'emacs)
  ;; web-mode
  ;; (define-key js2-refactor-mode-map (kbd "C-c C-e C-f") 'js2r-extract-function)
  ;; (evil-define-key 'normal js2-refactor-mode-map ",c" 'org-toggle-checkbox)
  ;; vterm mode
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-e")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-f")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-a")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-v")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-b")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-w")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-u")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-n")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-m")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-p")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-j")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-k")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-r")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-t")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-g")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-c")      #'vterm--self-insert)
  ;; (evil-define-key 'insert vterm-mode-map (kbd "C-SPC")    #'vterm--self-insert)
  ;; (evil-define-key 'normal vterm-mode-map (kbd "C-d")      #'vterm--self-insert)
  ;; (evil-define-key 'normal vterm-mode-map (kbd ",c")       #'multi-vterm)
  ;; (evil-define-key 'normal vterm-mode-map (kbd ",n")       #'multi-vterm-next)
  ;; (evil-define-key 'normal vterm-mode-map (kbd ",p")       #'multi-vterm-prev)
  ;; (evil-define-key 'normal vterm-mode-map (kbd "i")        #'evil-insert-resume)
  ;; (evil-define-key 'normal vterm-mode-map (kbd "o")        #'evil-insert-resume)
  ;; (evil-define-key 'normal vterm-mode-map (kbd "<return>") #'evil-insert-resume)
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
  (evil-define-key 'normal org-mode-map ",I" 'org-toggle-inline-images)
  ;; move over wrapped lines
  (evil-define-key 'normal org-mode-map "j" 'evil-next-visual-line)
  (evil-define-key 'normal org-mode-map "k" 'evil-previous-visual-line)

  (evil-define-key 'normal markdown-mode-map ",c" 'markdown-toggle-markup-hiding)

  ;; move over wrapped lines
  (evil-define-key 'normal markdown-mode-map "j" 'evil-next-visual-line)
  (evil-define-key 'normal markdown-mode-map "k" 'evil-previous-visual-line))

(use-package evil-mc
  :demand t
  :bind ("M-d" . hydra-mc/body)
  :init
  (defhydra hydra-mc (:color red :hint nil)
    "
Add:		 Jump:
------------------------------
_a_ll		 _N_ext
_n_ext		 _P_revious  
_p_reivous	 
_s_kip
"
    ("a" evil-mc-make-all-cursors)
    ("q" evil-mc-undo-all-cursors "quit" :color blue)
    ("N" evil-mc-make-and-goto-next-cursor)
    ("P" evil-mc-make-and-goto-prev-cursor)
    ("s" evil-mc-skip-and-goto-next-match)
    ("n" evil-mc-make-and-goto-next-match)
    ("p" evil-mc-make-and-goto-prev-match))
  :config
  (global-evil-mc-mode  1))


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

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package evil-collection
  :after evil
  :config
  ;; https://github.com/emacs-evil/evil-collection/blob/master/evil-collection-dired.el
  (evil-collection-init))
  ;; (evil-collection-init '(dired term ansi-term)))
  ;; (setq evil-collection-mode-list 'nil))

(use-package evil-escape
  :config
  (evil-escape-mode t)
  (setq evil-escape-key-sequence "jk"))

;; THEMES + FACES
;; --------------------------------------------------------
(use-package nord-theme)

(use-package doom-themes
  ;; :unless window-system
  :config
  (when window-system (set-frame-font "Hack Nerd Font:size=14"))
  ;; (when window-system (set-frame-font "FiraCode Nerd Font:size=14"))

  ;; (if window-system
  ;;     (load-theme 'doom-one t)
  ;;   (load-theme 'doom-nord t))

  (load-theme 'doom-nord t)

  ;; (load-theme 'nord t))

  ;; (load-theme 'doom-one-light t) ;; good for sun

  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t))

(defface bday-face
  '((t (:inherit web-mode-constant-face :weight bold)))
  "Face to use for key words in web mode"
  :group 'web-mode)

(font-lock-add-keywords 'web-mode `(
				    ("return " 0 'bday-face t)
				    ("export " 0 'bday-face t)
				    ("type " 0 'web-mode-type-face t)
				    ) 'append)
;; TODO remove as it slows things
(use-package doom-modeline
  ;; :unless window-system
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 20
	doom-modeline-env-version t
	doom-modeline-modal-icon t
	doom-modeline-vcs-max-length 24
	doom-modeline-buffer-file-name-style 'truncate-except-project
	;; Whether display buffer encoding.
	doom-modeline-buffer-encoding nil))

(use-package olivetti
  :disabled
  :if window-system
  :hook (org-mode . olivetti-mode))

(use-package poet-theme
  :disabled
  :if window-system
  :config
  (blink-cursor-mode 0)
  (load-theme 'poet t)
  (custom-set-faces
   '(vertical-border ((t (:background "black" :foreground "controlColor")))))
  ;; non monospace font
  (set-frame-font "Avenir Next:size=14"))

(use-package hide-mode-line
  :disabled
  :hook (org-mode . hide-mode-line-mode))

;; (setq-default mode-line-buffer-identification
;;               (let ((orig  (car mode-line-buffer-identification)))
;;                 `(:eval (cons (concat ,orig (abbreviate-file-name default-directory))
;;                               (cdr mode-line-buffer-identification)))))

;; https://gitlab.com/mark.feller/emacs.d/blob/master/modules/module-solarized.el
;; https://www.reddit.com/r/emacs/comments/6ftm3x/share_your_modeline_customization/
;; https://gitlab.com/mark.feller/emacs.d/blob/master/modules/module-solarized.el
;; (setq mode-line-format
;;       (list
;;        ;; value of current buffer name
;;        (propertize
;; 	(when (projectile-project-name) (concat "" (projectile-project-name) "/"))
;; 	'face '(:foreground "blue"))
;;        "%b"
;;        (when (buffer-modified-p)
;; 	(propertize "*"
;; 		    'face '(:foreground "green")
;; 		    'help-echo "buffer modified."))
;;        "  "
;;        ;; value of current line number
;;        "%l  "
;;        ;; value of `mode-name'
;;        "%m "
;;        minor-mode-list))

;;; WIP
;; ==================================================================================


;;; init.el ends here
