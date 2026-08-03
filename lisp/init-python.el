;;; init-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; I use nix + direnv instead of virtualenv/pyenv/pyvenv, and it is an
;; approach which extends to other languages too. I recorded a
;; screencast about this: https://www.youtube.com/watch?v=TbIHRHy7_JM


(setq auto-mode-alist
      (append '(("SConstruct\\'" . python-mode)
                ("SConscript\\'" . python-mode))
              auto-mode-alist))

;; (setq python-shell-interpreter "python3")

(require-package 'pip-requirements)

;; (when (maybe-require-package 'flymake-ruff)
;;   (defun sanityinc/flymake-ruff-maybe-enable ()
;;     (when (executable-find flymake-ruff-program)
;;       (flymake-ruff-load)))
;;   (add-hook 'python-mode-hook 'sanityinc/flymake-ruff-maybe-enable))

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'reformatter)
  (reformatter-define black :program "black" :args '("-")))


;;; Automatic support for Conda virtual environments (DD)
;;; To manually activate or deactivate a conda environment:
;;;   M-x conda-env-activate
;;;   M-x conda-env-deactivate
(when (maybe-require-package 'conda)
  ;; Find Anaconda and environments on MacOS
  (when *is-a-mac*
    (setq conda-env-home-directory (expand-file-name "~/miniforge3")))

  ;; if you want interactive shell support, include:
  (conda-env-initialize-interactive-shells)

  ;; if you want eshell support, include:
  (conda-env-initialize-eshell)

  ;; Auto-activate environment if we find an environment.yml file. This produces
  ;; annoying message spam every time a file opens:
  ;; (conda-env-autoactivate-mode t)
  )

;;; New versions of Python look for pyrepl, which doesn't work in the current
;;; Emacs run-python (DD)
(with-eval-after-load 'python
  (setenv "PYTHON_BASIC_REPL" "1"))


(provide 'init-python)
;;; init-python.el ends here
