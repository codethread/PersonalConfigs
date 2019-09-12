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
  ;; :bind
  ;; (("C-c c" . org-capture)
  ;;  ("C-c l" . org-store-link)
  ;;  ("C-c a" . org-agenda))
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
        org-startup-indented t)

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

  ;; Highlight done todos with different colors.
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)" 1 'org-headline-done prepend))
   'append)
  )

(use-package org-bullets
  :commands org-bullets-mode
  :hook (org-mode . org-bullets-mode))

(use-package markdown-mode
  :hook
  (markdown-mode . flyspell-mode)
  (markdown-mode . visual-line-mode))

(provide 'init-org)
