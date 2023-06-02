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

(setq python-shell-interpreter "python3")

(require-package 'pip-requirements)

(when (maybe-require-package 'toml-mode)
  (add-to-list 'auto-mode-alist '("poetry\\.lock\\'" . toml-mode)))

(when (maybe-require-package 'reformatter)
  (reformatter-define black :program "black" :args '("-")))


;;; Automatic support for Conda virtual environments (DD)
(when (maybe-require-package 'conda)
  ;; Find Anaconda and environments on MacOS
  (when *is-a-mac*
    (setq conda-anaconda-home (expand-file-name "~/opt/anaconda3"))
    (setq conda-env-home-directory (expand-file-name "~/opt/anaconda3")))

  ;; if you want interactive shell support, include:
  ;; (conda-env-initialize-interactive-shells)

  ;; if you want eshell support, include:
  ;; (conda-env-initialize-eshell)

  ;; Auto-activate environment if we find an environment.yml file. This produces
  ;; annoying message spam every time a file opens:
  ;; (conda-env-autoactivate-mode t)
  )

(provide 'init-python)
;;; init-python.el ends here
