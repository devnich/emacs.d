;; -----------------------------------
;; Derek's customizations
;; -----------------------------------

;; Use a nice font
(set-frame-font "DejaVu Sans Mono-13")

;; Scroll single line
(setq scroll-step 1)

;; Invoke eshell on startup
(setq eshell-buffer-name "~$")
(add-hook 'emacs-startup-hook 'eshell)

;; Invoke terminal with multi-term
(require-package 'multi-term)
(setq multi-term-buffer-name "~zsh")
(setq multi-term-program "/bin/zsh")            ;; use zsh in terminal
(global-set-key (kbd "C-c T") 'multi-term)      ;; create new terminal buffer
(global-set-key (kbd "C-c t") 'multi-term-next) ;; switch terminals

;; Get root access to files on demand. This function advises ido-find-file, so
;; it's invoked via C-x C-f
;; cf. http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;; Move cursor to the most recently visited buffer in ibuffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;; Add some keyboard shortcuts
(defalias 'qrr 'query-replace-regexp)
(global-set-key [f5] 'call-last-kbd-macro)

;; Add syntax highlighting for Drupal PHP files
(add-to-list 'auto-mode-alist '("\\.module$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.install$" . php-mode))

;; Use python 3 as the default interpreter
(setq python-shell-interpreter "python3")

;;; Suppress yes/no prompt when quitting shells
(add-hook 'comint-exec-hook
          (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

;;; Suppress yes/no prompt when quitting terminals
(add-hook 'term-exec-hook
          (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

;; -----------------------------------
;; Customizations to add as necessary
;; -----------------------------------

;; Find keyboard eval shortcut for scratch buffer
;;C-j 'eval-print-last-sexp

;; Add keyboard bindings for terminal if necessary
;;  1. Mimic shell mode toggle
;; (add-to-list 'term-bind-key-alist '("C-c C-j" . term-line-mode))
;; (add-to-list 'term-bind-key-alist '("C-c C-k" . term-char-mode))
;;  2. Define additional keys that bypass Emacs and go to the remote terminal
;; (defun term-send-esc ()
;;   "Send ESC in term mode"
;;   (interactive)
;;   (term-send-raw-string "\e"))

;; (add-hook 'term-mode-hook
;;           (lambda () (define-key term-raw-map (kbd "ESC") 'term-send-esc)))
;; (add-hook 'term-mode-hook
;;           (lambda () (define-key term-raw-map (kbd "C-y") 'term-paste)))

;; Add syntax highlighting for drush make if necessary
;; (define-generic-mode
;;     'info-mode
;;   '(";")  ;; comment
;;   nil
;;   '(("projects" . 'font-lock-builtin-face)
;;     ("libraries" . 'font-lock-builtin-face)
;;     ("core" . 'font-lock-builtin-face)
;;     ("api" . 'font-lock-builtin-face)) ;; operators and builtins
;;   '(".info\\'" ".make\\'")
;;   nil
;;   "A mode for Drush .make and .info files"
;;   )

;; Add syntax highlighting for PHP-like files if necessary
;; (add-to-list 'auto-mode-alist '("\\.profile$" . php-mode))
;; (add-to-list 'auto-mode-alist '("\\.engine$" . php-mode))

(provide 'init-local)
