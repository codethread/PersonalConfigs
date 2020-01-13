;; enable the doom theme if working
(use-package doom-themes
  :config
  (if window-system
      (load-theme 'doom-one t)
    (load-theme 'doom-nord t))
  ;; (load-theme 'doom-one-light t) ;; good for sun
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-height 20
	doom-modeline-env-version t
	doom-modeline-modal-icon t
	doom-modeline-vcs-max-length 24
	doom-modeline-buffer-file-name-style 'truncate-except-project
	;; Whether display buffer encoding.
	doom-modeline-buffer-encoding nil))

(provide 'init-doom-themes)
