;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Adam Hall"
      user-mail-address "adamhalldesigns@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
(setq doom-font (font-spec :family "FiraCode Nerd Font" :size 12))
(setq doom-variable-pitch-font (font-spec :family "Georgia" :size 1.1))
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they accept

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-nord)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(use-package! tree-sitter
  :hook
  (tree-sitter-after-on . tree-sitter-hl-mode)
  ((typescript-mode rustic-mode) . tree-sitter-mode)
  :config
  ;; This makes every node a link to a section of code
  (setq tree-sitter-debug-jump-buttons t
        ;; and this highlights the entire sub tree in your code
        tree-sitter-debug-highlight-jump-region t)
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-tsx-mode . tsx))
  (add-to-list 'tree-sitter-major-mode-language-alist '(typescript-mode . tsx)))

(use-package! my-org-helpers
  :hook (org-mode . my/org-theme)
  :after (:any org org-roam))

(use-package! my-markdown-helpers
  :hook (markdown-mode . my/markdown-theme))

;; (use-package! typescript-mode
;;   :hook (typescript-mode . my/ts-mode-settings)
;;   :config
;;   (defun my/ts-mode-settings ()
;;     "Hook for ts mode."
;;     ;; (setq flycheck-checker 'javascript-eslint)
;;     (flycheck-add-next-checker 'lsp 'javascript-eslint)))

;; (use-package! web-mode
;;   :hook ((web-mode-hook) . my/web-mode-settings)
;;   :config
;;   (defun my/web-mode-settings ()
;;     "Hooks for Web mode."
;;     (interactive)
;;     (flycheck-add-mode 'javascript-eslint 'web-mode)
;;     (flycheck-add-next-checker 'lsp 'javascript-eslint)
;;     ;; (flycheck-add-mode 'css-stylelint 'web-mode)
;;     ;; (flycheck-add-next-checker 'javascript-eslint 'css-stylelint)

;;     ;; (setq web-mode-code-indent-offset 2)
;;     ;; (setq web-mode-markup-indent-offset 2)
;;     ;; (setq web-mode-enable-auto-closing t)
;;     ;; (setq web-mode-enable-auto-quoting nil)
;;     ))
