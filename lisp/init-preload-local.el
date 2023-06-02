;;; init-preload-local.el --- Early startup settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Make custom themes available
(setq custom-theme-directory "~/.emacs.d/site-lisp/")

;; Suppress startup messages about Conda environments
(setq conda-message-on-environment-switch 0)

(provide 'init-preload-local)
