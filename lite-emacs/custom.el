(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("e1ecb0536abec692b5a5e845067d75273fe36f24d01210bf0aa5842f2a7e029f" default))
 '(package-selected-packages
   '(org-alert org-download olivetti nord-theme flycheck dashboard lsp-metals yasnippet ob-graphql sbt-mode scala-mode hydra use-package-hydra evil-mc evil-multiedit ripgrep deadgrep multi-libvterm org-tempo docker-compose-mode dockerfile-mode go-mode docker-compose docker yaml-mode org-bullets json-mode doom-modeline evil-escape counsel-projectile doom-themes lsp-mode counsel swiper dracula dracula-theme evil-surround evil-org evil-commentary evil-collection evil-leader which-key cargo rust-mode web-mode graphql-mode rjsx-mode wakatime-mode markdown-toc markdown-mode ivy projectile ace-window ace-jump-mode magit undo-tree dotenv-mode editorconfig xclip fic-mode exec-path-from-shell use-package))
 '(safe-local-variable-values
   '((org-download-image-dir . "./orgpics")
     (org-download-image-dir . "./pics")
     (org-download-image-dir . "~/Dropbox/org-me-notes/pics")
     (org-download-image-dir . "~/org-notes/org-me-notes/pics")
     (eval add-hook 'before-save-hook #'lsp-eslint-apply-all-fixes nil t)
     (flycheck-checker . lsp)
     (lsp-eslint-working-directories . "[./src]")
     (lsp-enable-file-watchers quote nil)))
 '(wakatime-python-bin nil))
;;; custom.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:inherit warning :weight bold :height 2.0))))
 '(elscreen-tab-background-face ((t (:background "#dfdfdf" :height 1.3))))
 '(elscreen-tab-current-screen-face ((t (:background "#fafafa" :foreground "#a626a4"))))
 '(elscreen-tab-other-screen-face ((t (:background "#dfdfdf" :foreground "#a190a7"))))
 '(fic-face ((t (:inherit warning :weight bold)))))
