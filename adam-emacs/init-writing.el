;; enable this section if writing
(use-package olivetti
  :hook (org-mode . olivetti-mode))

(use-package poet-theme
  :config
  (blink-cursor-mode 0)
  (load-theme 'poet t)
  (custom-set-faces
   '(vertical-border ((t (:background "black" :foreground "controlColor")))))
  ;; non monospace font
  (set-frame-font "Avenir Next:size=14"))

(use-package hide-mode-line
  :hook (org-mode . hide-mode-line-mode))

(provide 'init-writing)
