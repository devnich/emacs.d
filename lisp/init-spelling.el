;;; init-spelling.el --- Spell check settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'ispell)

;;; Add Windows support for ispell available via hunspell (DD):
;;;   https://github.com/hunspell/hunspell
;;; Windows binary available from:
;;;   https://sourceforge.net/projects/ezwinports/files/
;;; Copy binary into the `C:/Program Files/hunspell' directory.
(if *is-windows*
    (setq ispell-program-name "C:/Program Files/hunspell/bin/hunspell"))

(when (executable-find ispell-program-name)
  ;; Add spell-checking in comments for all programming language modes
  (add-hook 'prog-mode-hook 'flyspell-prog-mode)

  (with-eval-after-load 'flyspell
    (define-key flyspell-mode-map (kbd "C-;") nil)
    (add-to-list 'flyspell-prog-text-faces 'nxml-text-face)))

(provide 'init-spelling)
;;; init-spelling.el ends here
