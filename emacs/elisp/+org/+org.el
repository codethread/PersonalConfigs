;;; +org.el --- small utils for org-mode -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'org)

(defvar +org-personal-file "~/Dropbox/org-me-notes/notes.org")

(defvar +org-work-file "~/OneDrive - Sky/dev/org-sky-notes/work.org")

(defvar +org-heading-font "Futura"
  "Font to use for org title, headings and markdown headings.")

;; seems broken
(defun +org-toggle-list-checkbox ()
  (interactive)
  (org-toggle-checkbox 4))

(defun +org-list-to-checkbox (arg)
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

(defun +org-fill-buffer ()
  "call `fill-region' on entire buffer"
  (interactive)
  (save-excursion
    (save-restriction
      (widen)
      (fill-region (point-min) (point-max)))))

(defun +org-theme ()
  "Change org faces to a more minal style."
  (interactive)
  (custom-set-faces
   ;; hide the BEGIN and END in source blocks
   
   `(org-block			((t (:inherit fixed-pitch :background ,(doom-color 'base3) :extend t))))
   `(org-block-begin-line	((t (:foreground ,(doom-color 'base3)))))
   `(org-block-end-line		((t (:foreground ,(doom-color 'base3)))))
   
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))

   `(org-drawer			((t (:inherit fixed-pitch :foreground ,(doom-color 'base5) :height 1))))
   `(org-meta-line		((t (:inherit fixed-pitch :foreground ,(doom-color 'base5)))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))

   `(org-document-title		((t (:foreground ,(doom-color 'teal) :font ,+org-heading-font :height 1.6 :weight regular))))
   `(org-quote			((t (:inherit default :foreground ,(doom-color 'base6) :background ,(doom-color 'base3) :extend t :slant italic))))

   `(org-link			((t (:inherit fixed-pitch :foreground ,(doom-color 'blue) :underline t))))

   ;; stuff with =equals= (emphasis or inline quote, don't use in tables)
   `(org-verbatim		((t (:background ,(doom-color 'base4) :foreground ,(doom-color 'fg) :slant italic))))
   ;; stuff with ~tilde~ (inline code snippets)
   `(org-code			((t (:foreground ,(doom-color 'orange) :background ,(doom-color 'base3)))))
   `(org-code			((t (:foreground ,(doom-color 'base7) :background ,(doom-color 'base3)))))

   `(org-table			((t (:inherit fixed-pitch :foreground ,(doom-color 'base6)))))


   `(org-todo			((t (:foreground ,(doom-color 'yellow) :height 0.9))))
   `(org-headline-todo		((t (:foreground ,(doom-color 'base6)))))
   `(org-headline-done		((t (:strike-through t))))
   `(org-done			((t (:inherit org-headline-done :strike-through t :height 0.9))))
   `(org-checkbox		((t (:foreground ,(doom-color 'blue)))))
   `(org-date			((t (:foreground ,(doom-color 'magenta)))))

   `(org-level-1		((t (:foreground ,(doom-color 'base6) :font ,+org-heading-font :height 1.3))))
   `(org-level-2		((t (:foreground ,(doom-color 'base6) :font ,+org-heading-font :height 1.2))))
   `(org-level-3		((t (:foreground ,(doom-color 'base6) :height 1.1 :slant italic))))
   `(org-level-4		((t (:foreground ,(doom-color 'base6) :height 1.1))))
   `(org-level-5		((t (:foreground ,(doom-color 'base6) :height 1.1))))
   `(org-level-6		((t (:foreground ,(doom-color 'base6) :height 1.1))))
   `(org-level-7		((t (:foreground ,(doom-color 'base6) :height 1.1))))
   `(org-level-8		((t (:foreground ,(doom-color 'base6) :height 1.1))))))

;;;###autoload
(defun +org-open-my-notes-file ()
  "Open rough notes."
  (interactive)
  (find-file +org-personal-file))

;;;###autoload
(defun +org-open-work-notes-file ()
  "Open work notes."
  (interactive)
  (find-file +org-work-file))

(provide '+org)
;;; +org.el ends here

