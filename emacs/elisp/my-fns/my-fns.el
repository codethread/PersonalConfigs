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


;; (defun my/md-link-to-org ()
;;   ;; Can also be adapted to use the region, but one would need to add
;;   ;; a marker and region-end.  Remember to remove marker at end.
;;   (let ((markdown-regex-link-inline
;;          ;; from http://jblevins.org/git/markdown-mode.git/tree/markdown-mode.el
;;           "\\(!\\)?\\(\\[\\)\\([^]^][^]]*\\|\\)\\(\\]\\)\\((\\)\\([^)]*?\\)\\(?:\\s-+\\(\"[^\"]*\"\\)\\)?\\()\\)"))
;;     (while (search-forward-regexp markdown-regex-link-inline (point-max) t)
;;       (replace-match "[[\\6][\\3]]"))))


(defun my/open-init-file ()
  "Open init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun my/pomo ()
  "Start a pomodoro timer in the background."
  (interactive)
  (message "starting 25min timer")
  (save-window-excursion
    (async-shell-command
     (concat "pomo") "pomo-timer")))

(defun my/pomo-stop ()
  "Finish existing pomo timer."
  (interactive)
  (message "stopping pomo timer")
  (when (get-buffer "pomo-timer")
    (kill-buffer "pomo-timer"))
  (save-window-excursion
    (async-shell-command
     (concat "pomo --complete"))))

;; TODO still getting there
(defun my/replace-word-under-cursor ()
  "Replace word under cursor."
  (interactive)
  (print (thing-at-point 'word)))

;; (global-set-key (kbd "C-q") 'my/replace-word-under-cursor)

(defun my/tdd-message ()
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

(defun my/reload-init-file ()
  "Reload init.el without restart."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; (defun my/teardown
;;     "Remove all files from teardown file."
;;   (interactive)
;;  ())

;; (setq my-name "Adam")
;; ()
;; (defun hi () (insert "my name is " my-name))
;; (defun hello (name) (insert "hello " name))
;; (hello "you")

;; (progn
;;   (switch-to-buffer-other-window "*test*")
;;   (erase-buffer)
;;   (hello "you")
;;   (other-window 2))

;; (setq list-of-names '("Dave" "Barry" "Dean"))
;; (car list-of-names)
;; (cdr list-of-names)
;; (push "Steph" list-of-names)
;; (mapcar 'hello list-of-names)

(provide 'my-fns)

;;; my-fns.el ends here
