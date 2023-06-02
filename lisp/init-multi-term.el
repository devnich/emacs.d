;;; init-multi-term.el --- Run a better terminal inside Emacs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'multi-term)
(require-package 'fish-mode)

;;; Use Fish Shell in multi-term if available
(if (executable-find "fish")
    (progn (setq multi-term-buffer-name "~fish")
           (if *is-a-mac*
               (setq multi-term-program "~/.nix-profile/bin/fish")
             (setq multi-term-program "/usr/bin/fish")))
  (progn (setq multi-term-buffer-name "~bash")
         (setq multi-term-program "/bin/bash")))

;;; Create new terminal buffer
(global-set-key (kbd "C-c T") 'multi-term)

;;; Switch to next terminal buffer
(global-set-key (kbd "C-c t") 'multi-term-next)

;; Allow some keys to bypass Emacs and go to the underlying term process
(add-hook 'term-mode-hook
          (lambda () (define-key term-raw-map (kbd "ESC") 'term-send-esc)))
(add-hook 'term-mode-hook
          (lambda () (define-key term-raw-map (kbd "C-y") 'term-paste)))
(add-hook 'term-mode-hook
          (lambda () (define-key term-raw-map (kbd "C-c C-j") 'term-line-mode)))
(add-hook 'term-mode-hook
          (lambda () (define-key term-raw-map (kbd "C-c C-k") 'term-char-mode)))

(provide 'init-multi-term)
