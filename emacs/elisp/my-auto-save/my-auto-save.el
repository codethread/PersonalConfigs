;;; my-auto-save.el --- summary -*- lexical-binding: t -*-

;; Author: Adam Hall
;; Maintainer: Adam Hall
;; Version: version
;; Package-Requires: (dependencies)
;; Homepage: homepage
;; Keywords: keywords


;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.


;;; Commentary:

;; save project files when navigating away
;; much code stolen from https://github.com/bbatsov/super-save

;;; Code:
(require 'projectile)

(defgroup my-auto-save nil
  "Smart-saving of buffers."
  :group 'tools
  :group 'convenience)

(defcustom my-auto-save-triggers
  '(switch-to-buffer other-window windmove-up windmove-down windmove-left windmove-right next-buffer previous-buffer)
  "A list of commands which would trigger `my-auto-save-command'."
  :group 'my-auto-save
  :type '(repeat symbol))

(defun my-auto-save-command ()
  "Save the current buffer if needed."
  (when (and buffer-file-name
	     (projectile-project-p) 	; is in a project
	     (buffer-modified-p (current-buffer))
	     (file-writable-p buffer-file-name)
	     (not (file-remote-p buffer-file-name)))
    (save-buffer)))

(defun my-auto-save-command-advice (&rest _args)
  "A simple wrapper around `my-auto-save-command' that's advice-friendly."
  (my-auto-save-command))

(defun my-auto-save-advise-trigger-commands ()
  "Apply my-auto-save advice to the commands listed in `my-auto-save-triggers'."
  (mapc (lambda (command)
          (advice-add command :before #'my-auto-save-command-advice))
        my-auto-save-triggers))

(defun my-auto-save-remove-advice-from-trigger-commands ()
  "Remove my-auto-save advice from to the commands listed in `my-auto-save-triggers'."
  (mapc (lambda (command)
          (advice-remove command #'my-auto-save-command-advice))
        my-auto-save-triggers))

(defun my-auto-save-init ()
  "Setup advice for auto save."
  (my-auto-save-advise-trigger-commands))

(defun my-auto-save-stop ()
  "Remove advice for auto save."
  (my-auto-save-remove-advice-from-trigger-commands))

;;;###autoload
(define-minor-mode my-auto-save-mode
  "A minor mode that saves your Emacs buffers when they lose focus."
  :group 'my-auto-save
  :global t
  (cond
   (my-auto-save-mode (my-auto-save-init))
   (t (my-auto-save-stop))))

(provide 'my-auto-save)

;;; my-auto-save.el ends here
