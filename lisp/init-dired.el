;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default dired-dwim-target t)

;; Prefer g-prefixed coreutils version of standard utilities when available
(let ((gls (executable-find "gls")))
  (when gls (setq insert-directory-program gls)))

(when (maybe-require-package 'diredfl)
  (after-load 'dired
    (diredfl-global-mode)
    (require 'dired-x)))

;; Hook up dired-x global bindings without loading it up-front
(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

(after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

(when (maybe-require-package 'diff-hl)
  (after-load 'dired
    (add-hook 'dired-mode-hook 'diff-hl-dired-mode)))

;; Allow browsing directories as sudo
;; (when (maybe-require-package 'dired-toggle-sudo)
;;   (define-key dired-mode-map (kbd "C-c C-s") 'dired-toggle-sudo)
;;   (eval-after-load 'tramp
;;     '(progn
;;        ;; Allow to use: /sudo:user@host:/path/to/file
;;        (add-to-list 'tramp-default-proxies-alist
;;                     '(".*" "\\`.+\\'" "/ssh:%h:")))))


;; (defun sudired ()
;;   (interactive)
;;   (require 'tramp)
;;   (let ((dir (expand-file-name default-directory)))
;;     (if (string-match "^/sudo:" dir)
;;         (user-error "Already in sudo")
;;       (dired (concat "/sudo::" dir)))))
;; (define-key dired-mode-map (kbd "C-c C-s") 'sudired)

(provide 'init-dired)
;;; init-dired.el ends here
