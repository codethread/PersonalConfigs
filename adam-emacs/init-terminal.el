;; all things relating to terminals, shell, term, ansi-term, eshell

(setq explicit-shell-file-name "/bin/zsh")

(use-package multi-term
  :commands my|projectile-term-toggle
  :config
  (setq multi-term-program "/bin/zsh")

  (defun my|projectile-term-toggle ()
    "Goto `ansi-term' at root, or create one."
    (interactive)
    (let ((root (projectile-project-root))
	  (buff-name (concat "t:" (projectile-project-name))))
      (if (get-buffer buff-name)
	  (switch-to-buffer buff-name)
	(progn
	  (setq default-directory root)
	  (multi-term)
	  (rename-buffer buff-name t))))))

(defun my|shell-other-window ()
  "Open a `shell' in a new window."
  (interactive)
  (let ((buf (shell)))
    (switch-to-buffer (other-buffer buf))
    (switch-to-buffer-other-window buf)))

(defun my|eshell-new()
  "Open a new instance of eshell."
  (interactive)
  (eshell 'N))

(defun my|projectile-shell-toggle ()
  "Goto `eshell' at root, or create one."
  (interactive)
  (let ((root (projectile-project-root))
	(buff-name (concat "t:" (projectile-project-name))))
    (if (get-buffer buff-name)
	(switch-to-buffer buff-name)
      (progn
	(setq default-directory root)
	(eshell (getenv "SHELL"))
	(rename-buffer buff-name t)))))

(defun my|projectile-shell-new ()
  "Create an `eshell' at the project root."
  (interactive)
  (progn
    (split-window-sensibly (selected-window))
    (other-window 1)
    (setq default-directory (projectile-project-root))
    (eshell (getenv "SHELL"))))


(defun my|close-notifications-mac ()
  "Close Mac notifications."
  (interactive)
  (message "closing notifications")
  (save-window-excursion
    (async-shell-command
     (concat "automator ~/Library/Services/Close\\ all\\ notifications.workflow"))))

;; (defun elscreen-find-and-goto-by-buffer (&optional buffer create noselect)
;;   "Go to the screen that has the window with buffer BUFFER,
;; creating one if none already exists."
;;   (interactive)
;;   (let* ((prompt "Go to the screen with specified buffer: ")
;;          (create (or create (called-interactively-p 'any)))
;;          (buffer-name (or (and (bufferp buffer) (buffer-name buffer))
;;                           (and (stringp buffer) buffer)
;;                           (and (featurep 'iswitchb)
;;                                (iswitchb-read-buffer prompt))
;;                           (read-buffer prompt)))
;;          (target-screen (elscreen-find-screen-by-buffer
;;                          (get-buffer-create buffer-name) create)))
;;     (when target-screen
;;       (elscreen-goto target-screen)
;;       (unless noselect
;;         (select-window (get-buffer-window buffer-name))))
;;     target-screen))

(provide 'init-terminal)
