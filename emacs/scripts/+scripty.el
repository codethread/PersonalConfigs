;;; +scripty.el --- Bootstrap an emacs script -*- lexical-binding: t -*-

;;; Commentary:

;; Import this at the top of a script, and it should have all the needed libs to crack on, e.g:
;; 
;; (add-to-list 'load-path "~/PersonalConfigs/emacs/scripts")
;; (require '+scripty)
;; 
;; (--> '(1 2 3 4)
;;      (-reduce #'+ it))
;; 
;; can use the yassnippet 'script' to setup boilerplate

;;; Code:
(require '+constants)
(require '+bootstrap)

(use-package dash)
(require 'dash)


(use-package s)
(require 's)

(use-package f)
(require 'f)

(provide '+scripty)

;;; +scripty.el ends here
