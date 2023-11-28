;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(require-package 'wgrep)
(with-eval-after-load 'grep
  (dolist (key (list (kbd "C-c C-q") (kbd "w")))
    (define-key grep-mode-map key 'wgrep-change-to-wgrep-mode)))

(when (and (executable-find "ag")
           (maybe-require-package 'ag))
  (require-package 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (global-set-key (kbd "M-?") 'ag-project))

;;; Customized configuration for rg (DD)
;;;   cf. https://rgel.readthedocs.io/en/latest/usage.html
(when (and (executable-find "rg")
           (maybe-require-package 'rg))
  (global-set-key (kbd "<f5>") 'rg-project))

;;; javascript.el has hard-coded references to rg, so switching here for consistency
;; (maybe-require-package 'deadgrep))
;; (global-set-key (kbd "<f5>") #'deadgrep))


(provide 'init-grep)
;;; init-grep.el ends here
