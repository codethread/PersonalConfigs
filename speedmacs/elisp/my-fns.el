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

(message "Hello World!")

(defun my|kill-buffer ()
   "Kill the current buffer without saving."
   (interactive "P")
   ())
;; https://github.com/Dewdrops/evil-extra-operator/blob/f2e9cd12bf9888c440b73628199dcfe7f8efbf4a/evil-extra-operator.el#L151
;; help with googling
(provide 'my-fns)

;;; my-fns.el ends here
