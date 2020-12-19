;;; my-utils.el --- utility functions
;;; Commentary:
;;; Code:
(defun my|shell-command-to-string (cmd)
  "Return stdout from `CMD' removing trailing line."
  (substring
   (shell-command-to-string (concat cmd " 2>/dev/null"))
   0 -1))

(defun my|replace-in-string (WHAT WITH in)
  "`WHAT' to be replaced with `WITH' `IN' string."
  (replace-regexp-in-string (regexp-quote WHAT) WITH in nil 'literal))

(provide 'my-utils)

;;; my-utils.el ends here
