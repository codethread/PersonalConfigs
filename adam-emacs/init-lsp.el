(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package lsp-mode
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  :init (setq lsp-keymap-prefix "s-l")
  :hook
  (prog-mode . lsp)
  (lsp-mode . lsp-enable-which-key-integration)
  :commands lsp
  :config
  (flycheck-add-next-checker 'javascript-eslint 'lsp))

(use-package helm-lsp
  :after (helm-mode)
  :commands helm-lsp-workspace-symbol)

(provide 'init-lsp)

;; (use-package lsp-mode
;;   :diminish lsp-mode
;;   :hook (prog-mode . lsp-deferred)
;;   :bind (:map lsp-mode-map
;;               ("C-c C-d" . lsp-describe-thing-at-point))
;;   :init (setq lsp-auto-guess-root t       ; Detect project root
;;               lsp-prefer-flymake nil      ; Use lsp-ui and flycheck
;;               lsp-auto-execute-action t
;; 	      ;; lsp-log-io t
;;               flymake-fringe-indicator-position 'right-fringe)
;;   :config
;;   (defun my|lsp-describe-thing-at-point ()
;;   "Display the full documentation of the thing at point."
;;   (interactive)
;;   (let ((contents (-some->> (lsp--text-document-position-params)
;;                             (lsp--make-request "textDocument/hover")
;;                             (lsp--send-request)
;;                             (gethash "contents")))
;;         (buffer (get-buffer-create "*lsp-help*"))
;; 	(oldBuffer (current-buffer))
;; 	)
;;     (if (and contents (not (equal contents "")) )
;;         (progn
;;           (pop-to-buffer buffer)
;;           (with-current-buffer buffer
;;             (let ((inhibit-read-only t))
;;               (erase-buffer)
;;               (insert (lsp--render-on-hover-content contents t))
;;               (goto-char (point-min))
;;               (view-mode t)
;; 	      ))
;; 	  (pop-to-buffer oldBuffer))
;;       (lsp--info "No content at point."))))
;;   )

;; mine XXX: disabled since lsp started using flycheck
;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :config
;;   (setq lsp-ui-doc-enable nil
;;       lsp-ui-peek-enable nil
;;       lsp-ui-sideline-enable nil
;;       lsp-ui-imenu-enable nil
;;       lsp-ui-flycheck-enable t
;;       lsp-ui-flycheck-live-reporting nil)
;;   (flycheck-add-next-checker 'lsp-ui 'javascript-eslint)
;;   ;; (flycheck-add-next-checker 'javascript-eslint 'lsp-ui)
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; :config
;; ;; Configure LSP clients
;; (use-package lsp-clients
;;   :ensure nil
;;   :init (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))))

;; TODO: this seems to turn off flycheck
;; (use-package lsp-ui
;;   :commands lsp-ui-doc-hide
;;   ;; :custom-face (lsp-ui-doc-background ((t (:background ,(face-background 'tooltip)))))
;;   :bind (:map lsp-ui-mode-map
;;               ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
;;               ([remap xref-find-references] . lsp-ui-peek-find-references)
;;               ("C-c u" . lsp-ui-imenu))
;;   :init (setq lsp-ui-doc-enable 'nil ;; nil seems to be ignored?
;;               ;; lsp-ui-doc-use-webkit nil
;;               ;; lsp-ui-doc-delay 1.0
;;               ;; lsp-ui-doc-include-signature t
;;               ;; lsp-ui-doc-position 'at-point
;;               ;; lsp-ui-doc-border (face-foreground 'default)

;;               lsp-ui-sideline-enable nil
;;               lsp-ui-sideline-ignore-duplicate t)
;;   :config
;;   (add-to-list 'lsp-ui-doc-frame-parameters '(right-fringe . 8))

;;   ;; `C-g'to close doc
;;   (advice-add #'keyboard-quit :before #'lsp-ui-doc-hide)

;;   ;; Reset `lsp-ui-doc-background' after loading theme
;;   (add-hook 'after-load-theme-hook
;;             (lambda ()
;;               (setq lsp-ui-doc-border (face-foreground 'default))
;;               (set-face-background 'lsp-ui-doc-background
;;                                    (face-background 'tooltip))))

;;   ;; WORKAROUND Hide mode-line of the lsp-ui-imenu buffer
;;   ;; @see https://github.com/emacs-lsp/lsp-ui/issues/243
;;   (defadvice lsp-ui-imenu (after hide-lsp-ui-imenu-mode-line activate)
;;     (setq mode-line-format nil)))

