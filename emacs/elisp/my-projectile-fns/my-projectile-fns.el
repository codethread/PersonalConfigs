;;; projectile-fns.el --- custom functions that use projectile
;;; Commentary:
;;; Code:
(require 'projectile)
(require 'my-utils)

(defun my/goto-github ()
  "Navigate to github repo in default browser for current project."
  (interactive)
  (message (concat "Opening " (projectile-project-root)))
  (save-window-excursion
    (async-shell-command (concat "open -a '/Applications/Google Chrome.app' "
				 (replace-regexp-in-string "git@" "http://"
							   (replace-regexp-in-string ":" "/" (get-remote)))))))

(defun get-remote ()
  "Get git remote for current project."
  (my/shell-command-to-string (concat
			       "cd " (projectile-project-root) " && "
			       "git remote -v | grep git@github.com | grep fetch | head -1 | cut -f2 | cut -d' ' -f1")))

(defun my/projectile-shell-new ()
  "Create an `eshell' at the project root."
  (interactive)
  (progn
    (split-window-sensibly (selected-window))
    (other-window 1)
    (setq default-directory (projectile-project-root))
    (eshell (getenv "SHELL"))))

(defun my/test-file ()
  "Run test file for current file."
  (interactive)
  (message (concat "testing " (buffer-file-name)))
  (save-buffer)
  (async-shell-command
   (concat "cd " (projectile-project-root) " && node_modules/.bin/jest " (buffer-file-name) " --watch --collectCoverageOnlyFrom " (my/replace-in-string ".spec.js" ".jsx" buffer-file-name))))

(defun my/split-last-buffer ()
  "Open last buffer in horizontal split."
  (interactive)
  (split-window-horizontally)
  (other-window 1 nil)
  (projectile-previous-project-buffer))

(provide 'my-projectile-fns)

;;; projectile-fns.el ends here
