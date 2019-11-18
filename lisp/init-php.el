;;; init-php.el --- Support for working with PHP -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'php-mode)
  (maybe-require-package 'smarty-mode)

  (when (maybe-require-package 'company-php)
    (after-load 'company
      (push 'company-ac-php-backend company-backends))))

;; Add syntax highlighting for Drupal PHP files
(add-to-list 'auto-mode-alist '("\\.module$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.install$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.theme$" . php-mode))
;; (add-to-list 'auto-mode-alist '("\\.profile$" . php-mode)) interferes with .profile file


(provide 'init-php)
;;; init-php.el ends here
