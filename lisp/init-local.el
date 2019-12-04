;;; -----------------------------------
;;; Useful keyboard shortcuts
;;; -----------------------------------
;;; f1: help
;;; f3: kmacro-start-macro-or-insert-counter
;;; f4: kmacro-end-or-call-macro
;;; f5: deadgrep
;;; need alterate keybinding for paredit-splice-sexp. See this comment in init-paredit.el:
;;;    Suppress certain paredit keybindings to avoid clashes, including
;;;    my global binding of M-?

;;; -----------------------------------
;;; Mode additions
;;; -----------------------------------

;;; Call additional init files
(require 'init-web)
(require 'init-multi-term)

;;; Use python 3 as the default python interpreter
(setq python-shell-interpreter "python3")

;;; Use sqlite3 as default sqlite
(setq sql-sqlite-program "/usr/bin/sqlite3")

;;; Run rustfmt on .rs files on save
(setq rust-format-on-save t)

;;; -----------------------------------
;;; User interface
;;; -----------------------------------

;;; Use our own theme, based on Python IDLE. The path to this theme is set in
;;; init-preload.el.
(setq-default custom-enabled-themes '(sanityinc-tomorrow-idle))

;;; Use a nice font
(setq font-use-system-font nil)
;; (set-frame-font "DejaVu Sans Mono-10")
(cond
 ((string-equal system-type "gnu/linux")
  (set-face-attribute 'default nil :font "DejaVu Sans Mono-10"))
 ((string-equal system-type "windows-nt")
  (set-face-attribute 'default nil :font "Consolas-12"))
 ((string-equal system-type "darwin")
  (set-face-attribute 'default nil :font "Monaco-10"))
 )
(set-face-attribute 'mode-line nil :weight 'bold)
(set-face-attribute 'variable-pitch nil :font "Arial-10")

;;; Alternate versions
;; (when (member "DejaVu Sans Mono" (font-family-list))
;;   (set-face-attribute 'default nil :font "DejaVu Sans Mono-10"))
;; (set-face-attribute 'mode-line nil :font "Gillius ADF Bold")

;;; Scroll single line
(setq scroll-step 1)

;;; Add menu bar back
(menu-bar-mode 1)

;;; Set default column width
(setq-default fill-column 80)

;;; Add some keyboard shortcuts
(defalias 'qrr 'query-replace-regexp)

;;; Defer fontifying when input is pending
(setq jit-lock-defer-time 0)

;;; Defer fontifying until user is inactive. This appears to be redundant with the previous setting.
;; (setq jit-lock-stealth-time 10)
;; (setq jit-lock-stealth-nice 2)
;; (setq jit-lock-stealth-verbose t)

;;; Save desktop on exit
(desktop-save-mode 1)
;; (setq desktop-save t)

;;; Use system trash instead of immediate delete
(setq delete-by-moving-to-trash t)

;;; Suppress entering debug on error
(setq debug-on-error nil)

;;; Add height and width shortcuts to image-mode
(add-hook 'image-mode-hook
          (lambda ()
            (define-key image-mode-map "h"
              'image-transform-fit-to-height)
            (define-key image-mode-map "w"
              'image-transform-fit-to-width)))

;;; Set man page to open in current buffer. For other buffer, set to 'friendly
(setq Man-notify-method 'pushy)

;;; -----------------------------------
;;; Shells
;;; -----------------------------------

;;; Invoke eshell on startup
(setq eshell-buffer-name "~$")
(add-hook 'emacs-startup-hook 'eshell)
(when (string-equal system-type "windows-nt")
  (cd (getenv "HOME")))

;;; Suppress yes/no prompt when quitting shells
(add-hook 'comint-exec-hook
          (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

;;; Suppress yes/no prompt when quitting terminals
(add-hook 'term-exec-hook
          (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

;;; -----------------------------------
;;; File navigation
;;; -----------------------------------

;;; Move cursor to the most recently visited buffer in ibuffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
           "Open ibuffer with cursor pointed to most recent buffer name"
           (let ((recent-buffer-name (buffer-name)))
             ad-do-it
             (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;;; Return buffer list in a constant order; use in ibuffer-vc.el to maintain sort order of VC groups.
;; (sort (buffer-list) '(lambda (a b) (string< (buffer-name a) (buffer-name b))))

;;; Set which flags are passed to ls for dired display
(when (not (string-equal system-type "windows-nt"))
  (setq dired-listing-switches "-al --block-size=1M --group-directories-first"))
;; (setq dired-listing-switches "-ag --block-size=1M --no-group --group-directories-first")

;;; Look for .org files to include in agenda
(setq org-agenda-files (quote ("~/Dropbox/Library")))

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

;; (global-set-key [f5] 'call-last-kbd-macro)
