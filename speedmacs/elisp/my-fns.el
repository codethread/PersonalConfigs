;;; my-fns.el --- summary -*- lexical-binding: t -*-

;; Author: Adam
;; Maintainer: Adam
;; Version: 1
;; Package-Requires: (none)
;; Homepage: none
;; Keywords: none


;; This file is not part of GNU Emacs

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.


;;; Commentary:

;; useful fns for my configs

;;; Code:

;; help with googling

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
  (projectile-previous-project-buffer))

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

(defun tdd-message ()
  "Display the three laws of TDD."
  (interactive)
  (message
   (concat
    (propertize "1. " 'face 'font-lock-keyword-face)
    "You can’t write "
    (propertize "any production code " 'face 'font-lock-constant-face)
    "until you have first "
    (propertize "written a failing spec\n" 'face 'font-lock-warning-face)
    (propertize "2. " 'face 'font-lock-keyword-face)
    "You can’t write "
    (propertize "more of a unit test " 'face 'font-lock-constant-face)
    "than is sufficient to "
    (propertize "fail" 'face 'font-lock-warning-face)
    ", and not compiling is failing\n"
    (propertize "3. " 'face 'font-lock-keyword-face)
    "You can’t write "
    (propertize "more production code " 'face 'font-lock-constant-face)
    "than is sufficient to "
    (propertize "pass " 'face 'font-lock-string-face)
    "the currently failing "
    (propertize "unit test\n" 'face 'font-lock-constant-face))))

(defun my|reload-init-file ()
  "Reload init.el without restart."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(provide 'my-fns)

;;; my-fns.el ends here
