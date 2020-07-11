(require-package 'epl)
(require 'epl)

;;; Make custom themes available
(setq custom-theme-directory "~/.emacs.d/site-lisp/")

;; Suppress startup messages about Conda environments
(setq conda-message-on-environment-switch 0)

(provide 'init-preload-local)
