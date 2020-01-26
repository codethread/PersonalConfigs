;; forward function declarations eliminate warnings about whether a function is defined.
(declare-function exec-path-from-shell-initialize "exec-path-from-shell.el")

;; OSX specific code
(when (eq system-type 'darwin)
  ;; On OS X Emacs doesn't use the shell PATH if it's not started from
  ;; the shell. Let's fix that:
  (require 'exec-path-from-shell)
  (setq exec-path-from-shell-arguments 'nil)
  (exec-path-from-shell-initialize)

  (setq ns-function-modifier 'hyper) ;; fix alt as meta key

  (menu-bar-mode +1)

  ;; Enable emoji, and stop the UI from freezing when trying to display them.
  (if (fboundp 'set-fontset-font)
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))

;; y or n instead of yes etc
(defalias 'yes-or-no-p 'y-or-n-p)

;; close buffers without confirm
(setq kill-buffer-query-functions
       (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

(setq indent-tabs-mode nil
      font-lock-maximum-decoration 3

      ;; highlight parens
      show-paren-mode t

      ;; reduce the frequency of garbage collection by making it happen on
      ;; each 50MB of allocated data (the default is on every 0.76MB)
      gc-cons-threshold 50000000

      ;; warn when opening files bigger than 100MB
      large-file-warning-threshold 100000000
      vc-follow-symlinks t

      help-window-select t
      ;; always split vertically if there's room
      ;; split-width-threshold 170
      ;; Split horizontally when opening a new window from a command
      split-height-threshold nil
      ring-bell-function #'ignore
      visible-bell t
      show-paren-mode 1
      window-resize-pixelwise t
      save-abbrevs 'silently
      frame-resize-pixelwise t)

;;; functions
;; =====================================================================================

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

;; https://www.reddit.com/r/emacs/comments/64xb3q/killthisbuffer_sometimes_just_stops_working/
(defun my|kill-this-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))

(defun my|reload-init-file ()
  "Reload init.el without restart."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun my|open-init-file ()
  "Open init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

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
  (switch-to-next-buffer))

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

(defun my|replace-in-string (WHAT WITH in)
  "`WHAT' to be replaced with `WITH' `IN' string."
  (replace-regexp-in-string (regexp-quote WHAT) WITH in nil 'literal))

(provide 'init-settings)
