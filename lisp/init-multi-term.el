(require-package 'multi-term)
(require-package 'fish-mode)

;;; Use Fish Shell in multi-term
(setq multi-term-buffer-name "~fish")
(setq multi-term-program "/usr/bin/fish")

;;; Create new terminal buffer
(global-set-key (kbd "C-c T") 'multi-term)

;;; Switch to next terminal buffer
(global-set-key (kbd "C-c t") 'multi-term-next)

;; Allow some keys to bypass Emacs and go to the underlying term process
(add-hook 'term-mode-hook
          (lambda () (define-key term-raw-map (kbd "ESC") 'term-send-esc)))
(add-hook 'term-mode-hook
          (lambda () (define-key term-raw-map (kbd "C-y") 'term-paste)))

;;; Define keys to toggle shell modes
(add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
(add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))

(provide 'init-multi-term)
