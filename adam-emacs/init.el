;;; init.el --- Initialization file for Emacs
;;; Commentary: Emacs Startup File --- initialization for Emacs
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
;;; set up initial package-managers
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))

;; install use-package first if not installed, this then handles packages from here
(setq package-enable-at-startup nil)
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents) (package-install 'use-package))
(eval-when-compile (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)
;;; Use-package set up, now import modules.

;; (if (require 'quelpa nil t)
;;    (quelpa-self-upgrade)
;;  (with-temp-buffer
;;    (url-insert-file-contents "https://github.com/quelpa/quelpa/raw/master/bootstrap.el")
;;    (eval-buffer)))
;; =====================================================================================

;; load emacs settings and some custom functions
(require 'init-settings (concat user-emacs-directory "init-settings.el"))
(require 'init-terminal (concat user-emacs-directory "init-terminal.el"))
(require 'init-core (concat user-emacs-directory "init-core.el"))
;; some packages have a lot of config, so broken into own files
(require 'init-lsp (concat user-emacs-directory "init-lsp.el"))
(require 'init-company (concat user-emacs-directory "init-company.el"))

(require 'init-org (concat user-emacs-directory "init-org.el"))
(require 'init-appearance (concat user-emacs-directory "init-appearance.el"))

(require 'init-doom-themes (concat user-emacs-directory "init-doom-themes.el"))
;; (require 'init-writing (concat user-emacs-directory "init-writing.el"))

;; general packages
(require 'init-misc (concat user-emacs-directory "init-misc.el"))

(require 'init-langs (concat user-emacs-directory "init-langs.el"))
(require 'init-evil (concat user-emacs-directory "init-evil.el"))
;; shrug
