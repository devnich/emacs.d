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

;;; Customized configuration for rg (DD)
;;;   cf. https://rgel.readthedocs.io/en/latest/usage.html
(when (and (executable-find "rg")
           (maybe-require-package 'rg))
  (rg-enable-default-bindings)
  ;; (global-set-key (kbd "M-?") 'rg-project)
  )

(provide 'init-grep)
;;; init-grep.el ends here
