;;; init-company-lsp.el --- lsp and comapny completion -*- lexical-binding: t; -*-

(use-package lsp-mode
  :diminish lsp-mode
  :hook (prog-mode . lsp-deferred)
  :bind (:map lsp-mode-map
              ("C-c C-d" . lsp-describe-thing-at-point))
  :init (setq lsp-auto-guess-root t       ; Detect project root
              lsp-prefer-flymake nil      ; Use lsp-ui and flycheck
              lsp-auto-execute-action t
	      ;; lsp-log-io t
              flymake-fringe-indicator-position 'right-fringe)
  :config
  (defun my|lsp-describe-thing-at-point ()
  "Display the full documentation of the thing at point."
  (interactive)
  (let ((contents (-some->> (lsp--text-document-position-params)
                            (lsp--make-request "textDocument/hover")
                            (lsp--send-request)
                            (gethash "contents")))
        (buffer (get-buffer-create "*lsp-help*"))
	(oldBuffer (current-buffer))
	)
    (if (and contents (not (equal contents "")) )
        (progn
          (pop-to-buffer buffer)
          (with-current-buffer buffer
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert (lsp--render-on-hover-content contents t))
              (goto-char (point-min))
              (view-mode t)
	      ))
	  (pop-to-buffer oldBuffer))
      (lsp--info "No content at point."))))
  )

;; mine
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable nil
      lsp-ui-peek-enable nil
      lsp-ui-sideline-enable nil
      lsp-ui-imenu-enable nil
      lsp-ui-flycheck-enable t
      lsp-ui-flycheck-live-reporting nil)
  (flycheck-add-next-checker 'lsp-ui 'javascript-eslint)
  ;; (flycheck-add-next-checker 'javascript-eslint 'lsp-ui)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode))

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

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config (use-package yasnippet-snippets))

(use-package company-lsp
  :after lsp-mode
  :init (setq company-lsp-cache-candidates 'auto))

(use-package company
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :bind (("M-/" . company-complete)
         :map company-active-map
         ("C-k" . company-select-previous)
         ("C-j" . company-select-next)
         ("C-l" . company-complete-selection)
	 ("C-d" . company-next-page)
	 ("C-u" . company-previous-page)
	 ("<tab>" . company-complete-common-or-cycle)
         ("C-c C-y" . my-company-yasnippet)
	 ("C-SPC" . company-complete-common)
	 :map company-search-map
         ("C-k" . company-select-previous)
         ("C-j" . company-select-next))
  :hook (after-init . global-company-mode)
  :init
  (defun my-company-yasnippet ()
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))
  :config
  (setq company-tooltip-align-annotations t
        company-tooltip-limit 12
        company-idle-delay 1
        company-echo-delay (if (display-graphic-p) nil 0)
        company-minimum-prefix-length 2
        company-require-match nil
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
	company-sort-by-backend-importance t
	))

;; Better sorting and filtering
;; seems to just sort by length, which is shit
;; (use-package company-prescient
;;   :init (company-prescient-mode 1))

(use-package company-box
  :diminish
  :functions (my-company-box--make-line
              my-company-box-icons--elisp)
  :commands (company-box--get-color
             company-box--resolve-colors
             company-box--add-icon
             company-box--apply-color
             company-box--make-line
             company-box-icons--elisp)
  :hook (company-mode . company-box-mode)
  :init (setq company-box-backends-colors nil
              company-box-show-single-candidate t
              company-box-max-candidates 50
              company-box-doc-delay 0.5)
  :config
  ;; Support `company-common'
  (defun my-company-box--make-line (candidate)
    (-let* (((candidate annotation len-c len-a backend) candidate)
            (color (company-box--get-color backend))
            ((c-color a-color i-color s-color) (company-box--resolve-colors color))
            (icon-string (and company-box--with-icons-p (company-box--add-icon candidate)))
            (candidate-string (concat (propertize (or company-common "") 'face 'company-tooltip-common)
                                      (substring (propertize candidate 'face 'company-box-candidate) (length company-common) nil)))
            (align-string (when annotation
                            (concat " " (and company-tooltip-align-annotations
                                             (propertize " " 'display `(space :align-to (- right-fringe ,(or len-a 0) 1)))))))
            (space company-box--space)
            (icon-p company-box-enable-icon)
            (annotation-string (and annotation (propertize annotation 'face 'company-box-annotation)))
            (line (concat (unless (or (and (= space 2) icon-p) (= space 0))
                            (propertize " " 'display `(space :width ,(if (or (= space 1) (not icon-p)) 1 0.75))))
                          (company-box--apply-color icon-string i-color)
                          (company-box--apply-color candidate-string c-color)
                          align-string
                          (company-box--apply-color annotation-string a-color)))
            (len (length line)))
      (add-text-properties 0 len (list 'company-box--len (+ len-c len-a)
                                       'company-box--color s-color)
                           line)
      line))
  (advice-add #'company-box--make-line :override #'my-company-box--make-line)

  ;; Prettify icons
  (defun my-company-box-icons--elisp (candidate)
    (when (derived-mode-p 'emacs-lisp-mode)
      (let ((sym (intern candidate)))
        (cond ((fboundp sym) 'Function)
              ((featurep sym) 'Module)
              ((facep sym) 'Color)
              ((boundp sym) 'Variable)
              ((symbolp sym) 'Text)
              (t . nil)))))
  (advice-add #'company-box-icons--elisp :override #'my-company-box-icons--elisp)

  (when (and (display-graphic-p)
             (require 'all-the-icons nil t))
    (declare-function all-the-icons-faicon 'all-the-icons)
    (declare-function all-the-icons-material 'all-the-icons)
    (setq company-box-icons-all-the-icons
          `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.9 :v-adjust -0.2))
            (Text . ,(all-the-icons-faicon "text-width" :height 0.85 :v-adjust -0.05))
            (Method . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Function . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Constructor . ,(all-the-icons-faicon "cube" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-purple))
            (Field . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
            (Variable . ,(all-the-icons-faicon "tag" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-lblue))
            (Class . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Interface . ,(all-the-icons-material "share" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Module . ,(all-the-icons-material "view_module" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Property . ,(all-the-icons-faicon "wrench" :height 0.85 :v-adjust -0.05))
            (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.9 :v-adjust -0.2))
            (Value . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Enum . ,(all-the-icons-material "storage" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.9 :v-adjust -0.2))
            (Snippet . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2))
            (Color . ,(all-the-icons-material "palette" :height 0.9 :v-adjust -0.2))
            (File . ,(all-the-icons-faicon "file-o" :height 0.9 :v-adjust -0.05))
            (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.9 :v-adjust -0.2))
            (Folder . ,(all-the-icons-faicon "folder-open" :height 0.9 :v-adjust -0.05))
            (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-lblue))
            (Constant . ,(all-the-icons-faicon "bolt" :height 0.9 :v-adjust -0.05)) ;; TODO; something wrong here?
            (Struct . ,(all-the-icons-material "settings_input_component" :height 0.9 :v-adjust -0.2 :face 'all-the-icons-orange))
            (Event . ,(all-the-icons-faicon "bolt" :height 0.85 :v-adjust -0.05 :face 'all-the-icons-orange))
            (Operator . ,(all-the-icons-material "control_point" :height 0.9 :v-adjust -0.2))
            (TypeParameter . ,(all-the-icons-faicon "arrows" :height 0.85 :v-adjust -0.05))
            (Template . ,(all-the-icons-material "format_align_center" :height 0.9 :v-adjust -0.2)))
          company-box-icons-alist 'company-box-icons-all-the-icons)))

;; Popup documentation for completion candidates
(use-package company-quickhelp
  :defines company-quickhelp-delay
  :bind (:map company-active-map
              ([remap company-show-doc-buffer] . company-quickhelp-manual-begin))
  :hook (global-company-mode . company-quickhelp-mode)
  :init (setq company-quickhelp-delay 0.5))

(provide 'init-company-lsp)
