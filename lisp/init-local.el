;;; -----------------------------------
;;; Mode additions
;;; -----------------------------------
;;; Call additional init files
(require 'init-web)
(require 'init-multi-term)
;; (require 'init-slack)

;; Add syntax highlighting for Drupal PHP files
(add-to-list 'auto-mode-alist '("\\.module$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.install$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.theme$" . php-mode))

;;; Use python 3 as the default python interpreter
(setq python-shell-interpreter "python3")

;;; -----------------------------------
;;; User interface
;;; -----------------------------------
;;; Use our own theme, based on Python IDLE. The path to this theme is set in
;;; init-preload.el.
(setq-default custom-enabled-themes '(sanityinc-tomorrow-idle))

;;; Use a nice font
(setq font-use-system-font nil)
(set-frame-font "DejaVu Sans Mono-14")

;;; Scroll single line
(setq scroll-step 1)

;;; Add menu bar back
(menu-bar-mode 1)

;;; Set default column width
(setq-default fill-column 80)

;;; Add some keyboard shortcuts
(defalias 'qrr 'query-replace-regexp)
(global-set-key [f5] 'call-last-kbd-macro)

;;; Defer fontifying until user is inactive
(setq jit-lock-stealth-time 10)

;;; Save desktop on exit
(desktop-save-mode 1)
;; (setq desktop-save t)

;;; -----------------------------------
;;; Shells
;;; -----------------------------------
;;; Invoke eshell on startup
(setq eshell-buffer-name "~$")
(add-hook 'emacs-startup-hook 'eshell)

;;; Suppress yes/no prompt when quitting shells
(add-hook 'comint-exec-hook
          (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

;;; Suppress yes/no prompt when quitting terminals
(add-hook 'term-exec-hook
          (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

;;; -----------------------------------
;;; File navigation
;;; -----------------------------------
;;; Get root access to files on demand. This function advises ido-find-file, so
;;; it's invoked via C-x C-f
;;; cf. http://emacsredux.com/blog/2013/04/21/edit-files-as-root/
(defadvice ido-find-file (after find-file-sudo activate)
  "Find file as root if necessary."
  (unless (and buffer-file-name
               (file-writable-p buffer-file-name))
    (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

;;; Move cursor to the most recently visited buffer in ibuffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
           "Open ibuffer with cursor pointed to most recent buffer name"
           (let ((recent-buffer-name (buffer-name)))
             ad-do-it
             (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;;; Set which flags are passed to ls for dired display
(setq dired-listing-switches "-al --block-size=1M --group-directories-first")
;; (setq dired-listing-switches "-ag --block-size=1M --no-group --group-directories-first")

(provide 'init-local)


;; -----------------------------------
;; Customizations to add as necessary
;; -----------------------------------

;;; Send function keys through to underlying term process
;; http://stackoverflow.com/questions/2396680/let-emacs-send-fn-keys-to-programs-in-ansi-term

;; Find keyboard eval shortcut for scratch buffer
;;C-j 'eval-print-last-sexp

;; Add keyboard bindings for terminal if necessary
;;  Define additional keys that bypass Emacs and go to the remote terminal
;; (defun term-send-esc ()
;;   "Send ESC in term mode"
;;   (interactive)
;;   (term-send-raw-string "\e"))

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

;; (set-face-attribute 'mode-line nil :font "Cantarell-12")
;; (set-face-attribute 'mode-line nil :font "DejaVu Serif Condensed:italic:medium-12")
;; (set-face-attribute 'default nil :font "DejaVu Sans Mono-13")
