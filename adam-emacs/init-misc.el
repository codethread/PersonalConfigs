;; https://github.com/chiply/spot4e
;; (use-package spot4e
;;   :load-path "./spot4e.el"
;;   :config
;;   (setq spot4e-refresh-token (getenv "SPOTIFY_TOKEN"))
;;   (run-with-timer 0 (* 60 59) 'spot4e-refresh))

;; (require 'spot4e (concat user-emacs-directory "spot4e.el"))
;; (setq spot4e-refresh-token (getenv "SPOTIFY_TOKEN"))
;; (run-with-timer 0 (* 60 59) 'spot4e-refresh)

;; https://github.com/danielfm/spotify.el
;; https://developer.spotify.com/dashboard/applications/dbe911bf500c4b418b5e5aafee3a99f6

;; (quelpa '(spotify :fetcher git :url "https://github.com/danielfm/spotify.el"))
;; (add-to-list 'load-path "/Users/adh23/.emacs.d/quelpa/build/spotify")
;; (require 'spotify)
;; (setq spotify-oauth2-client-id "xxx")
;; (setq spotify-oauth2-client-secret "xxx")
;; (define-key spotify-mode-map (kbd "C-c .") 'spotify-command-map)

;; TODO: fix python
(use-package wakatime-mode
  :config
  (setq wakatime-cli-path "/usr/local/bin/wakatime")
  (global-wakatime-mode))

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

; I'm using use-package and el-get and evil

;; (el-get-bundle slack)
(use-package slack
  :commands (slack-start)
  :disabled
  :init
  (setq slack-buffer-emojify t) ;; if you want to enable emoji, default nil
  (setq slack-prefer-current-team t)
  :config
  (slack-register-team
   :name "sky.slack.com"
   :token (getenv "SLACK_SKY_EMACS_TOKEN")
   :check-ping-timeout-sec 99 ;; default 20
   :reconnect-auto nil
   :default t
   :mark-as-read-immediately t
   :animate-image t
   )
   ;; :subscribed-channels '(graphql graphql-releases))

  (evil-define-key 'normal slack-info-mode-map
    ",u" 'slack-room-update-messages)
  (evil-define-key 'normal slack-mode-map
    ",c" 'slack-buffer-kill
    ",ra" 'slack-message-add-reaction
    ",rr" 'slack-message-remove-reaction
    ",rs" 'slack-message-show-reaction-users
    ",pl" 'slack-room-pins-list
    ",pa" 'slack-message-pins-add
    ",pr" 'slack-message-pins-remove
    ",mm" 'slack-message-write-another-buffer
    ",me" 'slack-message-edit
    ",md" 'slack-message-delete
    ",u" 'slack-room-update-messages
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel
    "\C-n" 'slack-buffer-goto-next-message
    "\C-p" 'slack-buffer-goto-prev-message
    )
  (evil-define-key 'insert slack-message-buffer-mode
    "<return>" (defun me|slack-new-line ()
		 (interactive)
		 (insert "
")))

   (evil-define-key 'normal slack-edit-message-mode-map
    ",k" 'slack-message-cancel-edit
    ",s" 'slack-message-send-from-buffer
    ",2" 'slack-message-embed-mention
    ",3" 'slack-message-embed-channel))

;; (use-package alert
;;   :commands (alert)
;;   :init
;;   (setq alert-default-style 'notifier))

;; https://endlessparentheses.com/keep-your-slack-distractions-under-control-with-emacs.html
(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier)
  :config
  (add-to-list 'alert-user-configuration
             '(((:category . "slack")) libnotify nil))

  ;; (add-to-list 'alert-user-configuration
  ;; 	       '(((:category . "slack")) ignore nil))
  ;; (add-to-list
  ;;  'alert-user-configuration
  ;;  '(((:title . "\\(graphql\\|fun-sponge\\|dick-heads\\)") 
  ;;     (:category . "slack"))
  ;;    libnotify nil))

  ;; (add-to-list
  ;;  'alert-user-configuration
  ;;  '(((:message . "@adam.hall\\|Adam Hall")
  ;;     (:title . "\\(graphql-releases\\|pages-apps\\|pages-lib\\)")
  ;;     (:category . "slack"))
  ;;    libnotify nil))
  )

(use-package org-jira
  :disabled
  :config
  ;; https://cbsjira.bskyb.com/secure/RapidBoard.jspa?rapidView=9630&selectedIssue=DCP-506
  (setq jiralib-url "https://cbsjira.bskyb.com")
  (setq org-jira-custom-jqls
	'(
	  (:jql "project = DCP 
AND issuetype in (Epic) 
AND status != Archive 
AND status != Done 
AND component = GraphQL 
AND (labels not in (to_refine) OR labels is EMPTY) 
ORDER BY priority DESC"
		:filename "skyport-epics"
		)
	  (:jql "project = DCP 
AND issuetype in (Story, Epic) 
AND status != Archive 
AND resolution = Unresolved 
AND component = GraphQL 
AND labels in (to_refine, question) 
AND 'Epic Link' is EMPTY 
ORDER BY priority DESC"
		:filename "skyport-to-refine")
	  ))
  )

(provide 'init-misc)
