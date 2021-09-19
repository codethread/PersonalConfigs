;;; my-modeline.el --- summary -*- lexical-binding: t -*-

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

;; Lite and breazy

;;; Code:
(require 'doom-themes)

(defgroup my-modeline nil
  "My lite modeline."
  :group 'my-modeline
  :link '(url-link :tag "Homepage" "https://github.com/AHDesigns/PersonalConfigs"))

(defgroup my-modeline-faces nil
  "The faces of `my-modeline'."
  :group 'my-modeline
  :group 'faces
  :link '(url-link :tag "Homepage" "https://github.com/AHDesigns/PersonalConfigs"))

(defface my-modeline-base-face
  '((t (:inherit mode-line)))
  "Face used as a base for all modeline faces."
  :group 'my-modeline-faces)

(defface my-modeline-base-lrg-face
  '((t (:inherit my-modeline-base-face :height 1.2)))
  "Face used as a base for all plus sized modeline faces."
  :group 'my-modeline-faces)

(defface my-modeline-segment-face
  '((t (:inherit my-modeline-base-face)))
  "Face used for modeline segments.")

(defface my-modeline-segment-spc-face
  '((t (:inherit my-modeline-base-face)))
  "Face used for modeline space segments.")

(custom-set-faces
 `(my-modeline-base-face ((t :foreground ,(doom-color 'white) :background ,(doom-color 'base3)))))

(defun my-modeline-vc-branch ()
  "Get the current git branch."
  (substring vc-mode 5))

(setq mode-line-format
      (list
       (propertize "hello " 'face 'my-modeline-segment-spc-face)
       ;; value of `mode-name'
       ;; (propertize "%m: " 'face 'my-modeline-segment-face)
       ;; ;; value of current buffer name
       ;; (propertize "buffer %b, " 'face 'my-modeline-segment-face)
       ;; ;; value of current line number
       ;; '(:eval (concat " " (projectile-project-name)))

       ;; (my-modeline-vc-branch)
       "-- user: "
       ;; value of user
       (getenv "USER")))

;;; WIP ------------------------------------------------------------


;; use setq-default when finished

;; (setq-default mode-line-format (list
;; 				(propertize "%m: " 'face 'font-lock-keyword-face)
;; 				;; value of current buffer name
;; 				(propertize "buffer %b, " 'face 'my-modeline-segment)))


;; example of a modeline
;; ("%e" mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote mode-line-frame-identification mode-line-buffer-identification "   " mode-line-position evil-mode-line-tag
;;  (elscreen-display-screen-number
;;   (" " elscreen-mode-line-string))
;;  (vc-mode vc-mode)
;;  "  " mode-line-modes mode-line-misc-info mode-line-end-spaces)

(provide 'my-modeline)

;;; my-modeline.el ends here
