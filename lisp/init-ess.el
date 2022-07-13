;;; init-ess.el --- Support for the R language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'ess)
  (add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode)))

;;; ESS uses flymake for syntax checking by default; this produces errors
(setq ess-use-flymake nil)

(maybe-require-package 'polymode)
(when (maybe-require-package 'poly-R)
  ;; associate the new polymode to Rmd files:
  (add-to-list 'auto-mode-alist
               '("\\.[rR]md\\'" . poly-gfm+r-mode))
  ;; uses braces around code block language strings:
  (setq markdown-code-block-braces t))

(provide 'init-ess)
;;; init-ess.el ends here
