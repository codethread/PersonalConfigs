;;; +utils.el --- utility functions -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require 'dash)
(require 's)
(require 'cl-lib)

;;;###autoload
(defun +utils-shell-command-to-string (cmd)
  "Return stdout from `CMD' removing trailing line."
  (substring
   (shell-command-to-string (concat cmd " 2>/dev/null"))
   0 -1))

;;;###autoload
(defun +utils-replace-in-string (WHAT WITH in)
  "`WHAT' to be replaced with `WITH' `IN' string."
  (replace-regexp-in-string (regexp-quote WHAT) WITH in nil 'literal))

;;;###autoload
(defun +utils-close-notifications-mac ()
  "Close Mac notifications."
  (interactive)
  (message "closing notifications")
  (save-window-excursion
    (async-shell-command
     (concat "automator ~/Library/services/Close\\ BSur\\ Notifications.workflow"))))

;;;###autoload
(cl-defun +utils-when-monitor-size (&key small medium large)
  "Get a value based on the current monitor size. REST should be
key value pairs where the key is one of :small :medium
:large (1320 1600 1920 pixels respectively). If a size is not
matched, it will use the next lowest size (e.g if no value is
provided for a :medium, then the value provided for :small will
be used)."
  (let* ((screen (display-pixel-width))
	 (size (cond
		((<= screen 1280) 's)
		((< screen 1600) 'm)
		(t 'l))))
    (cond
     ((eq size 's) small)
     ((eq size 'm) (or medium small))
     ((eq size 'l) (or large medium small)))))
;; (defun +utils-md-link-to-org ()
;;   ;; Can also be adapted to use the region, but one would need to add
;;   ;; a marker and region-end.  Remember to remove marker at end.
;;   (let ((markdown-regex-link-inline
;;          ;; from http://jblevins.org/git/markdown-mode.git/tree/markdown-mode.el
;;           "\\(!\\)?\\(\\[\\)\\([^]^][^]]*\\|\\)\\(\\]\\)\\((\\)\\([^)]*?\\)\\(?:\\s-+\\(\"[^\"]*\"\\)\\)?\\()\\)"))
;;     (while (search-forward-regexp markdown-regex-link-inline (point-max) t)
;;       (replace-match "[[\\6][\\3]]"))))


(defun +utils-open-init-file ()
  "Open init.el."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun +utils-pomo ()
  "Start a pomodoro timer in the background."
  (interactive)
  (message "starting 25min timer")
  (save-window-excursion
    (async-shell-command
     (concat "pomo") "pomo-timer")))

(defun +utils-pomo-stop ()
  "Finish existing pomo timer."
  (interactive)
  (message "stopping pomo timer")
  (when (get-buffer "pomo-timer")
    (kill-buffer "pomo-timer"))
  (save-window-excursion
    (async-shell-command
     (concat "pomo --complete"))))

;; TODO still getting there
(defun +utils-replace-word-under-cursor ()
  "Replace word under cursor."
  (interactive)
  (print (thing-at-point 'word)))

;; (global-set-key (kbd "C-q") '+utils-replace-word-under-cursor)

(defun +utils-tdd-message ()
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

(defun +utils-reload-init-file ()
  "Reload init.el without restart."
  (interactive)
  (load-file "~/.emacs.d/init.el"))

;; (defun +utils-teardown
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

;; wip to find duplicate packages

;; (-let ((buff (get-file-buffer "~/PersonalConfigs/emacs/init.el"))
;;        (current-match t))
;;   (with-current-buffer buff
;;     (message (buffer-file-name))
;;     (save-excursion
;;       (goto-char (point-min))
;;       ;; skip past bootsrap code
;;       (search-forward ";;; Initial packages")
;;       (setq packages '())
;;       (while (setq current-match (search-forward "use-package " nil t))
;; 	(-when-let (package (thing-at-point 'symbol t))
;; 	  (-if-let (duplicate (alist-get package packages nil nil #'equal))
;; 	      (push (list package 2 (line-number-at-pos)) packages)
;; 	    (push (list package 1 (line-number-at-pos)) packages))))
;;       ;; (sort packages #'s-less?)
;;       packages)))

(provide '+utils)
;;; +utils.el ends here
