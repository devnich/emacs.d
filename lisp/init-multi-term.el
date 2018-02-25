(require-package 'multi-term)
(require-package 'fish-mode)

;;; Use Fish Shell in multi-term
(setq multi-term-buffer-name "~fish")
(setq multi-term-program "/usr/bin/fish")       ;; use fish in terminal

;;; Keyboard shortcuts
(global-set-key (kbd "C-c T") 'multi-term)      ;; create new terminal buffer
(global-set-key (kbd "C-c t") 'multi-term-next) ;; switch terminals

(provide 'init-multi-term)
