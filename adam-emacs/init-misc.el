;; https://github.com/chiply/spot4e
;; (require 'spot4e (concat user-emacs-directory "spot4e.el"))
;; (setq spot4e-refresh-token (getenv "SPOTIFY_TOKEN"))
;; (run-with-timer 0 (* 60 59) 'spot4e-refresh)

;; TODO: fix python
;; (use-package wakatime-mode
;;   :config
;;   (setq wakatime-cli-path "/usr/local/bin/wakatime")
;;   (global-wakatime-mode))

(use-package org-pomodoro
  :commands (org-pomodoro)
  :config
  (setq alert-user-configuration (quote ((((:category . "org-pomodoro")) libnotify nil)))))

;; (use-package eww
;;   :init
;;   (setq browse-url-browser-function
;; 	'((".*google.*maps.*" . browse-url-generic)
;; 	  ;; Github goes to firefox, but not gist
;; 	  ("http.*\/\/github.com" . browse-url-generic)
;; 	  ("groups.google.com" . browse-url-generic)
;; 	  ("docs.google.com" . browse-url-generic)
;; 	  ("melpa.org" . browse-url-generic)
;; 	  ("build.*\.elastic.co" . browse-url-generic)
;; 	  (".*-ci\.elastic.co" . browse-url-generic)
;; 	  ("gradle-enterprise.elastic.co" . browse-url-generic)
;; 	  ("internal-ci\.elastic\.co" . browse-url-generic)
;; 	  ("zendesk\.com" . browse-url-generic)
;; 	  ("salesforce\.com" . browse-url-generic)
;; 	  ("stackoverflow\.com" . browse-url-generic)
;; 	  ("apache\.org\/jira" . browse-url-generic)
;; 	  ("thepoachedegg\.net" . browse-url-generic)
;; 	  ("zoom.us" . browse-url-generic)
;; 	  ("t.co" . browse-url-generic)
;; 	  ("twitter.com" . browse-url-generic)
;; 	  ("\/\/a.co" . browse-url-generic)
;; 	  ("youtube.com" . browse-url-generic)
;; 	  ("amazon.com" . browse-url-generic)
;; 	  ("slideshare.net" . browse-url-generic)
;; 	  ("." . eww-browse-url)))
;;   (setq shr-external-browser 'browse-url-generic)
;;   (setq browse-url-browser-function 'browse-url-default-macosx-browser)
;;   (add-hook 'eww-mode-hook #'toggle-word-wrap)
;;   (add-hook 'eww-mode-hook #'visual-line-mode)
;;   :config
;;   (use-package s :ensure t)
;;   (define-key eww-mode-map "o" 'eww)
;;   (define-key eww-mode-map "O" 'eww-browse-with-external-browser)
;;   (define-key eww-mode-map "j" 'next-line)
;;   (define-key eww-mode-map "k" 'previous-line)

;;   (use-package eww-lnum
;;     :config
;;     (bind-key "f" #'eww-lnum-follow eww-mode-map)
;;     (bind-key "U" #'eww-lnum-universal eww-mode-map)))

(provide 'init-misc)
