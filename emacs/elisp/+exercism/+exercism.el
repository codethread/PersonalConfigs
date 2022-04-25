;;; exercism.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(defun exercism-submit ()
  "Submit current file."
  (interactive)
  (async-shell-command (concat "exercism submit " (buffer-file-name))))

(define-minor-mode exercism-mode
  "Toggle exercism mode. Interactively with no argument, this
     command toggles the mode. A positive prefix argument enables
     the mode, any other prefix argument disables it. From Lisp,
     argument omitted or nil enables the mode, `toggle' toggles
     the state.
     
     When Exercism mode is enabled, A few things are bound to
     C-c.  See the command \\[exercism-submit]."
  ;; The initial value.
  nil
  ;; The indicator for the mode line.
  " exercism"
  ;; The minor mode bindings.
  '(([C-c C-e s] . exercism-submit))
  :group 'exercism)

(provide 'exercism)

;;; exercism.el ends here
