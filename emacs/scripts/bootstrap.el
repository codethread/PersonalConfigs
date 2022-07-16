;;; bootstrap.el --- script -*- lexical-binding: t -*-

;;; Commentary:

;; Bootstraps my dotfiles into the home directory

;;; Code:

(add-to-list 'load-path "~/PersonalConfigs/emacs/scripts")
(require '+scripty)

(print
 (--> '(1 2 3 4)
      (-reduce #'+ it)))

;;; bootstrap.el ends here
