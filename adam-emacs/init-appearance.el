;; disable the toolbar at the top of the window
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

(add-to-list 'default-frame-alist '(tool-bar-lines . 0))
(add-to-list 'default-frame-alist '(menu-bar-lines . 0))
(add-to-list 'default-frame-alist '(vertical-scroll-bars))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'default-frame-alist '(ns-appearance . dark))
;; (setq ns-use-proxy-icon nil) ;; not sure why undefined

;; Set symbol for the border
(set-display-table-slot standard-display-table
                        'vertical-border 
                        (make-glyph-code ?┃))

(setq frame-title-format nil)

(use-package doom-themes
  :config
  ;; (load-theme 'doom-one t) ;; good in gui
  (load-theme 'doom-nord t)
  ;; (load-theme 'doom-city-lights t)
  ;; (load-theme 'doom-one-light t) ;; good for sun
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(use-package solaire-mode
  :hook
  ((change-major-mode after-revert ediff-prepare-buffer) . turn-on-solaire-mode)
  (minibuffer-setup . solaire-mode-in-minibuffer)
  :config
  (solaire-global-mode +1)
  (solaire-mode-swap-bg))

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode)
      :config
      (setq doom-modeline-vcs-max-length 24)
      (setq doom-modeline-buffer-file-name-style 'truncate-except-project)

      ;; Whether display buffer encoding.
      (setq doom-modeline-buffer-encoding nil)

      )

(use-package hide-mode-line
  :hook ((term-mode occur) . hide-mode-line-mode))

(use-package all-the-icons)

(defun my|face-under-cursor (pos)
  (interactive "d")
  (let ((face (or (get-char-property (pos) 'read-face-name)
                  (get-char-property (pos) 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

;; off in favour off rainbow-delimiters and show-paren-mode
;; (use-package highlight-parentheses
;;   :config
;;   (global-highlight-parentheses-mode t))

;; (use-package prettify-symbols
;;   :config
;;   (defconst lisp--prettify-symbols-alist
;;     '(
;;       ("lambda"  . ?λ)
;;       ("def" . ?ƒ)
;;       )))

;; (add-hook 'emacs-lisp-mode-hook 'prettify-symbols-mode)
;; (add-hook 'web-mode-hook 'prettify-symbols-mode)

;; (use-package dashboard
;;   :ensure t
;;   :config
;;   (dashboard-setup-startup-hook))

;; face overrides
(defface bday-face
  '((t (:inherit web-mode-constant-face :weight bold)))
  "Face to use for key words in web mode"
  :group 'web-mode)

(font-lock-add-keywords 'web-mode `(
				    ("return" 0 'bday-face t)
				    ("export" 0 'bday-face t)
				    ) 'append)

(provide 'init-appearance)
