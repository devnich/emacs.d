;;; init-eshell.el --- Customize eshell -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(maybe-require-package 'eshell-prompt-extras)

(with-eval-after-load "esh-opt"
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

;;; Sends "visual" commands to external term buffer
(with-eval-after-load "em-term"
  (add-to-list 'eshell-visual-options '("git" "--help" "--paginate"))
  (add-to-list 'eshell-visual-subcommands '("git" "log" "diff" "show"))
  )

;;; Wrap long lines
(maybe-require-package 'adaptive-wrap)
(add-hook 'eshell-mode-hook 'visual-line-mode)
(add-hook 'eshell-mode-hook
          (lambda () (setq adaptive-wrap-extra-indent 2)))
(add-hook 'eshell-mode-hook 'adaptive-wrap-prefix-mode)

;;; Eshell aliases; see more examples at  https://olddeuteronomy.github.io/post/eshell-aliases-and-prompt/
(add-hook 'eshell-mode-hook
          (lambda ()
            (eshell/alias "gl" "git log --color --graph --oneline --abbrev-commit --decorate --all --date=short --pretty=format:\"%C(cyan)%h %C(yellow)%ad %C(bold green)%d %C(reset)%s\"")))


(provide 'init-eshell)
;;; init-eshell.el ends here
