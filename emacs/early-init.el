(setq gc-cons-threshold (* 1000 1000 1000)) ; garbage collection every 100MB (default is every 0.76MB)

(add-hook 'emacs-startup-hook #'my/print-init-time)

(setq initial-frame-alist
      `((horizontal-scroll-bars . nil)
	(vertical-scroll-bars . nil)
	(menu-bar-lines . 0)
	;; (background-color . "#2E3440")
	;; (foreground-color . "white")
	(ns-transparent-titlebar . t)
	(top . 150)
	(left . 400)))

(setq default-frame-alist (copy-alist initial-frame-alist))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)


(defun my/print-init-time ()
  "Print EMACS load time."
  (message
   "*** Emacs loaded in %s with %d garbage collections."
   (format "%.2f seconds" (float-time
			   (time-subtract after-init-time before-init-time)))
   gcs-done))

(defvar user-temporary-file-directory
  (concat temporary-file-directory user-login-name "/"))

(unless (file-directory-p user-temporary-file-directory)
  (make-directory user-temporary-file-directory))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:
