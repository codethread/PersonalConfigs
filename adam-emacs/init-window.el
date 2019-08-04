;;; set up gui
;; =====================================================================================
(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark)) ;; assuming you are using a dark theme
;; (setq ns-use-proxy-icon nil) ;; not sure why undefined
(setq frame-title-format nil)

;;; window
;; =====================================================================================

(setq help-window-select t
 ;; split-width-threshold 170 ;; always split vertically if there's room
      ring-bell-function #'ignore
      visible-bell t
      window-resize-pixelwise t
      frame-resize-pixelwise t
      )

;; Split horizontally when opening a new window from a command
;; whenever possible.
(setq split-height-threshold nil)

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

(defun frontmacs/vsplit-last-buffer ()
  (interactive)
  (split-window-vertically)
  (other-window 1 nil)
  (switch-to-next-buffer))
(global-set-key (kbd "C-x 2") 'frontmacs/vsplit-last-buffer)

;; horizontal split, switch window, and open next buffer
(defun frontmacs/hsplit-last-buffer ()
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (switch-to-next-buffer))

;; focus window after split
;; (global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
;; (global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

(defun frame-half-size-left ()
  "Set the current frame to half the screen width."
  (interactive)
  (let ((frame (selected-frame))
        (one-half-display-pixel-width (/ (display-pixel-width) 2)))
    (set-frame-width frame one-half-display-pixel-width nil 'pixelwise)
    (set-frame-position frame 0 0))
  )

(defun frame-half-size-right ()
  "Set the current frame to half the screen width."
  (interactive)
  (let ((frame (selected-frame))
        (one-half-display-pixel-width (/ (display-pixel-width) 2)))
    (set-frame-width frame one-half-display-pixel-width nil 'pixelwise)
    (set-frame-position frame one-half-display-pixel-width 0))
  )

(global-set-key (kbd "C-M-<left>") 'frame-half-size-left)
(global-set-key (kbd "C-M-<right>") 'frame-half-size-right)
(global-set-key (kbd "C-M-<return>") 'toggle-frame-maximized)
(global-set-key (kbd "C-x 3") 'frontmacs/hsplit-last-buffer)

;; (defun switch-to-buffer--hack (orig-fun &rest args)
;;   (if-let ((win (get-buffer-window (car args))))
;;       (select-window win)
;;     (apply orig-fun args)))

;; (advice-add 'switch-to-buffer :around #'switch-to-buffer--hack)

(provide 'init-window)
