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

;;; Disable annoying and non-performant options
(setq ess-eval-visibly nil)
(setq ess-indent-with-fancy-comments nil)
(setq ess-use-eldoc 'script-only)
(setq ess-use-flymake nil)

;;; --------------------------
;;; Usage Notes
;;; --------------------------
;; Manual: https://ess.r-project.org/Manual/ess.html

;; To source a file, use ess-load-file (C-c M-l). This will default to the
;; current buffer.

;; To send a break to the inferior R process, try:
;;  1. C-g C-c C-c
;;  2. (setq comint-prompt-read-only nil) so that C-c C-c will succeed

;;; --------------------------
;;; Debug slow input in ESS
;;; --------------------------

;; By default, company-mode echoes contextual help to the minibuffer. This
;; dramatically slows input speed, so we disable it here. cf:
;;   https://emacs.stackexchange.com/a/62170
;;   https://github.com/emacs-ess/ESS/issues/1062
;; (setq ess-r--no-company-meta t)
;; (company-quickhelp-mode -1)

;;; Disable eldoc entirely
;; (setq ess-use-eldoc nil)


(provide 'init-ess)
;;; init-ess.el ends here
