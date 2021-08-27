;;; org-helpers.el --- small utils for org-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'org)

(defvar org-personal-file "~/Dropbox/org-me-notes/notes.org")
(defvar org-work-file "~/OneDrive - Sky/dev/org-sky-notes/work.org")

;; seems broken
(defun my/org-toggle-list-checkbox ()
  (interactive)
  (org-toggle-checkbox 4))

(defun my/open-my-notes-file ()
  "Open rough notes."
  (interactive)
  (find-file org-personal-file))

(defun my/open-work-notes-file ()
  "Open work notes."
  (interactive)
  (find-file org-work-file))

(defun my/list-to-checkbox (arg)
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

(defun fill-buffer ()
  "call `fill-region' on entire buffer"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (fill-region (point-min) (point-max)))))

(provide 'org-helpers)
;;; org-helpers.el ends here
