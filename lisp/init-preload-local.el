(require-package 'epl)
(require 'epl)

;; Make custom themes available
(when
    (file-directory-p "~/emacs-themes/color-theme-sanityinc-tomorrow")
  (add-to-list 'load-path "~/emacs-themes/color-theme-sanityinc-tomorrow")
  (add-to-list 'custom-theme-load-path "~/emacs-themes/color-theme-sanityinc-tomorrow"))

(provide 'init-preload-local)
