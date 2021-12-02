;;; init-ess.el --- Support for the R language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'ess))

(add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode))

(provide 'init-ess)
;;; init-ess.el ends here
