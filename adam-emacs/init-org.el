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
			     "~/org-notes/org-me-notes/notes.org"
			     "~/org-notes/org-sky-notes/work.org"))
  :commands
  (my|open-work-notes-file
   my|open-my-notes-file)
  :bind
  (("C-c c" . org-capture)
   ("C-c l" . org-store-link)
   ("C-c a" . org-agenda))
  :hook
  (org-mode . visual-line-mode)
  (org-mode . flyspell-mode)
  :config
  ;; set scratch buffer to org mode
  (setq initial-major-mode 'org-mode)

  (setq org-fontify-done-headline t
        org-fontify-whole-heading-line t
        org-hide-leading-stars t
        org-hide-block-startup t
        org-startup-folded t
        org-startup-indented t
	org-log-done 'time)

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
  ;; babel stuff
  ;; allow bash
  (org-babel-do-load-languages 'org-babel-load-languages '(
							   (shell . t)
							   (js . t)
							   ))

  ;; Highlight done todos with different colors.
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
   'append)

  ;; capture templates
  ;; (setq org-capture-templates
  ;;   '(("t" "Todo" entry (file "~/org-notes/capture.org")
  ;;      "* TODO %?\n%U" :empty-lines 1)
  ;;     ("T" "Todo with Clipboard" entry (file "~/org-notes/capture.org")
  ;;      "* TODO %?\n%U\n   %c" :empty-lines 1)
  ;;     ("n" "Note" entry (file "~/org-notes/capture.org")
  ;;      "* NOTE %?\n%U" :empty-lines 1)
  ;;     ("N" "Note with Clipboard" entry (file "~/org-notes/capture.org")
  ;;      "* NOTE %?\n%U\n   %c" :empty-lines 1)
  ;;     ("e" "Event" entry (file+headline "~/org-notes/capture.org" "Transient")
  ;;      "* EVENT %?\n%U" :empty-lines 1)
  ;;     ("E" "Event With Clipboard" entry (file+headline "~/org-notes/capture.org" "Transient")
  ;;      "* EVENT %?\n%U\n   %c" :empty-lines 1))
  ;;   )

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
  )

;; only show bullets in gui
(if window-system
    (use-package org-bullets
      :commands org-bullets-mode
      :hook (org-mode . org-bullets-mode)))

(use-package markdown-mode
  :hook
  (markdown-mode . flyspell-mode)
  (markdown-mode . visual-line-mode))

(use-package markdown-toc)

(provide 'init-org)
