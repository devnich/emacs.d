;;; init-julia.el --- Julia editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;;; Julia prerequisites:
;;;   Parsers package

(maybe-require-package 'julia-repl)
(maybe-require-package 'eglot-jl)

(when (maybe-require-package 'julia-mode)
  (add-hook 'julia-mode-hook 'julia-repl-mode)
  (add-hook 'julia-mode-hook 'eglot-jl-init)
  (add-hook 'julia-mode-hook 'eglot-ensure))

(provide 'init-julia)
;;; init-julia.el ends here
