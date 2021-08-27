;;; my-gui-controls.el --- GUI controls -*- lexical-binding: t -*-

;;; Commentary:
;;; Code:
(require 'dash)
(require 's)

(defun frame-half-size-left ()
  "Set the current frame to half the screen width."
  (interactive)
  (my/move-frame 'left))

(defun frame-half-size-right ()
  "Set the current frame to half the screen width."
  (interactive)
  (my/move-frame 'right))

(defun my/move-frame (side)
  "Move frame to SIDE of either `left' or `right', with a dith of 50%."
  (-let ((frame (selected-frame))
	 ((&plist :width width :height height :x x) (get-display-half)))

    (set-frame-width frame width nil t)
    (set-frame-height frame height nil t)
    (set-frame-position frame
			(if (eq side 'left) 0 x)
			0)))

(defun get-display-half ()
  "Get the half width of the current display."
  (-let [screen-size (display-pixel-width)]
    (cond
     ;; macbook 16
     ((= screen-size 1792) (list :width 876 :height 1063 :x 896))
     ;; 4k monitor
     ((= screen-size 1920) (list :width 940 :height 1025 :x 960))
     ;; fallback
     (t (list :width (/ screen-size 2) :height (display-pixel-height) :x (/ screen-size 2))))))

(defun my/close-notifications-mac ()
  "Close Mac notifications."
  (interactive)
  (message "closing notifications")
  (save-window-excursion
    (async-shell-command
     (concat "automator ~/Library/services/Close\\ BSur\\ Notifications.workflow"))))

(provide 'my-gui-controls)

;;; my-gui-controls.el ends here
