;; https://github.com/chiply/spot4e
(require 'spot4e "~/.emacs.d/spot4e")
(setq spot4e-refresh-token (getenv "SPOTIFY_TOKEN"))
(run-with-timer 0 (* 60 59) 'spot4e-refresh)

(use-package wakatime-mode
  :config
  (setq wakatime-cli-path "/usr/local/bin/wakatime")
  (global-wakatime-mode))

(provide 'init-misc)
