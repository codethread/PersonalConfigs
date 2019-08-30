;; forward function declarations eliminate warnings about whether a function is defined.
(declare-function exec-path-from-shell-initialize "exec-path-from-shell.el")

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; OSX specific code
(when (eq system-type 'darwin)

  ;; On OS X Emacs doesn't use the shell PATH if it's not started from
  ;; the shell. Let's fix that:
  (require 'exec-path-from-shell)
  (exec-path-from-shell-initialize)

  ;; It's all in the Meta
  (setq ns-function-modifier 'hyper)

  (menu-bar-mode +1)

  ;; Enable emoji, and stop the UI from freezing when trying to display them.
  (if (fboundp 'set-fontset-font)
      (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend)))

;; y or n instead of yes etc
(defalias 'yes-or-no-p 'y-or-n-p)

 ;; highlight parens
(setq show-paren-mode t)

(setq-default indent-tabs-mode nil)
(setq-default font-lock-maximum-decoration 3)

(add-hook 'dired-mode-hook
      (lambda ()
        (dired-hide-details-mode)
        (dired-sort-toggle-or-edit)))

(setq vc-follow-symlinks t)

;; close buffers without confirm
(setq kill-buffer-query-functions
      (delq 'process-kill-buffer-query-function kill-buffer-query-functions))

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

(provide 'init-settings)
