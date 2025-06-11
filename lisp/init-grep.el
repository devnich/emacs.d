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
  (rg-enable-default-bindings)
  ;; Leaving isearch disabled for the time being (DD)
  ;; (require 'rg-isearch)
  ;; (define-key isearch-mode-map "\M-sr" 'rg-isearch-menu)
  )

(provide 'init-grep)
;;; init-grep.el ends here
