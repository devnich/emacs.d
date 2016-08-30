(require-package 'epl)
(require 'epl)

;; Make custom themes available
(add-to-list 'load-path "~/ethemes/color-theme-sanityinc-tomorrow")
(add-to-list 'load-path "~/ethemes/emacs-color-theme-roc")
(add-to-list 'custom-theme-load-path "~/ethemes/color-theme-sanityinc-tomorrow")
(add-to-list 'custom-theme-load-path "~/ethemes/emacs-color-theme-roc")

(provide 'init-preload-local)
