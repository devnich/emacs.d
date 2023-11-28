;;; init-ess.el --- Support for the R language -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'ess)
  (add-to-list 'auto-mode-alist '("\\.R\\'" . ess-r-mode)))

(maybe-require-package 'polymode)
(when (maybe-require-package 'poly-R)
  ;; associate the new polymode to Rmd files:
  (add-to-list 'auto-mode-alist
               '("\\.[rR]md\\'" . poly-gfm+r-mode))
  ;; uses braces around code block language strings:
  (setq markdown-code-block-braces t))

(with-eval-after-load 'polymode
  (add-to-list 'polymode-run-these-after-change-functions-in-other-buffers 'eglot--after-change)
  (add-to-list 'polymode-run-these-before-change-functions-in-other-buffers 'eglot--before-change))

;;; ESS uses flymake for syntax checking by default; this produces errors
;; (setq ess-use-flymake nil)

;;; --------------------------
;;; Debug slow input in ESS
;;; --------------------------

;; By default, company-mode echoes contextual help to the minibuffer. This
;; dramatically slows input speed, so we disable it here. cf:
;;   https://emacs.stackexchange.com/a/62170
;;   https://github.com/emacs-ess/ESS/issues/1062
;; (setq ess-r--no-company-meta t)

;;; Disable flycheck by default
;; (setq-default flycheck-disabled-checkers '(r-lintr))

;;; Consider disabling native fontification in Org blocks
;; (setq org-src-fontify-natively nil)

;;; Consider disabling flyspell in Org mode

;;; Disable pop-up help
;; (company-quickhelp-mode -1)


(provide 'init-ess)
;;; init-ess.el ends here
