;;; init-eglot.el --- LSP support via eglot          -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(when (maybe-require-package 'eglot)
  (maybe-require-package 'consult-eglot))

(with-eval-after-load 'eglot
  (setq eglot-events-buffer-size 0)
  (setq eglot-connect-timeout 600)
  (add-to-list 'eglot-stay-out-of 'flymake))


(provide 'init-eglot)
;;; init-eglot.el ends here
