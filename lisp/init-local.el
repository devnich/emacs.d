;;; init-local.el --- Custom settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Useful keyboard shortcuts

;;; f1: help
;;; f2: 2-column commands
;;; f3: kmacro-start-macro-or-insert-counter
;;; f4: kmacro-end-or-call-macro
;;; f5: ripgrep
;;; f6: compile (make -k)
;;; f7: see most recent buffer in other window
;;; ( : Dired Hide Details mode

;;; Add some keyboard shortcuts
(defalias 'qrr 'query-replace-regexp)
(define-key lisp-interaction-mode-map (kbd "C-c C-c") 'eval-print-last-sexp)
(define-key emacs-lisp-mode-map (kbd "C-c C-c") 'eval-buffer)
(define-key emacs-lisp-mode-map (kbd "C-c b") 'emacs-lisp-byte-compile-and-load)

;;; Need alterate keybinding for paredit-splice-sexp. See this comment in init-paredit.el:
;;;    "Suppress certain paredit keybindings to avoid clashes, including
;;;    my global binding of M-?"


;;; User interface

;;; Use a nice monospace font (NB: mode-line font height is set in the theme)
(setq font-use-system-font nil)
(cond ((member "DejaVu Sans Mono" (font-family-list))
       (set-face-attribute 'default nil :font "DejaVu Sans Mono-14"))
      (*is-windows*
       (set-face-attribute 'default nil :font "Consolas-14"))
      (*is-a-mac*
       (set-face-attribute 'default nil :font "Menlo-14")))

;;; Use a nice variable-pitch font. By default, Hoefler will only be available
;;; when *is-a-mac* is true.
(if (member "Hoefler Text" (font-family-list))
    ;; Hoefler Text for serif, Helvetica Neue for sans-serif
    (set-face-attribute 'variable-pitch nil :font "Hoefler Text-16")
  (set-face-attribute 'variable-pitch nil :font "Arial-14"))

;;; Toggle line spacing at will
(defun toggle-line-spacing ()
  (interactive)
  (if line-spacing
      (setq line-spacing nil)
    (setq line-spacing 0.2))
  (redraw-frame (selected-frame)))

;;; Add menu bar back
(menu-bar-mode 1)

;;; Set default column width
(setq-default fill-column 80)

;;; Defer fontifying when input is pending. Setting this to 0 might cause severe input lag?
(setq jit-lock-defer-time 0.2)  ;; nil by default
;; (setq redisplay-skip-fontification-on-input t)

;;; Defer fontifying until user is inactive
(setq jit-lock-stealth-time 0.5)
;; (setq jit-lock-stealth-nice 0.5)  ;; this is set automatically

;;; Set man page to open in current buffer. For other buffer, set to 'friendly
(setq Man-notify-method 'pushy)

;;; Use system trash instead of immediate delete.
;;; On MacOS this requires additional configuration to support automatic undo. See:
;;;   https://www.emacswiki.org/emacs/SystemTrash
;;;   https://github.com/ali-rantakari/trash
(setq delete-by-moving-to-trash t)
(when *is-a-mac*
  (setq trash-directory "~/.Trash"))

;;; Suppress entering debug on error
(setq debug-on-error nil)

;;; Debug global TAG completion
(setq debug-on-message "Making tags completion table")
(setq debug-on-message "Key sequence C-c ESC starts with non-prefix key C-c")

;;; Save desktop on exit
(desktop-save-mode 1)
;; (setq desktop-save t)


;;; Working with images

;;; Open .svg files as images instead of XML
(add-to-list 'auto-mode-alist '("\\.svg\\'" . image-mode))

;;; Add height and width shortcuts to image-mode
(add-hook 'image-mode-hook
          (lambda ()
            (define-key image-mode-map "h"
                        'image-transform-fit-to-height)
            (define-key image-mode-map "w"
                        'image-transform-fit-to-width)))


;;; Shells

;;; Invoke eshell on startup
;; (setq eshell-buffer-name "~$")
;; (add-hook 'emacs-startup-hook 'eshell)
;; (cd (getenv "HOME"))

;;; Suppress yes/no prompt when quitting shells
(add-hook 'comint-exec-hook
          (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))

;;; Suppress yes/no prompt when quitting terminals
(add-hook 'term-exec-hook
          (lambda () (set-process-query-on-exit-flag (get-buffer-process (current-buffer)) nil)))


;;; File navigation

;;; Move cursor to the most recently visited buffer in ibuffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
           "Open ibuffer with cursor pointed to most recent buffer name"
           (let ((recent-buffer-name (buffer-name)))
             ad-do-it
             (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;;; Set which flags are passed to ls for dired display
;;;   1. Use GNU coreutils if present
;;;   2. Emulate the ls command on Windows using the ls-lisp library
;;;   3. Use BSD-safe switchs on Macs without coreutils installed
(cond((or (executable-find "coreutils") (eq system-type 'gnu/linux))
      (setq dired-listing-switches "-al --block-size=1M --group-directories-first"))
     (*is-windows*
      (progn (setq ls-lisp-dirs-first t)
             (setq ls-lisp-ignore-case t)
             (setq ls-lisp-UCA-like-collation t)
             ;; mimic "-v"
             (setq ls-lisp-use-string-collate nil)))
     (*is-a-mac*
      (setq dired-listing-switches "-alh")))

;; Always use BSD-safe switches in TRAMP; root does not have access to the
;; coreutils version of ls
(if *is-a-mac* (add-hook 'dired-mode-hook
                         (lambda ()
                           (when (file-remote-p dired-directory)
                             (setq-local dired-actual-switches "-alhv")))))

;;; Look for .org files to include in agenda
(if *is-a-mac* (setq org-agenda-files (quote ("~/Documents"))))

(provide 'init-local)


;; Customizations to add as necessary

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
