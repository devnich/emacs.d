;;; init-pdf.el --- Display pdfs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'pdf-tools)

;;; Docs https://github.com/vedang/pdf-tools

;;; After upgrading Poppler, run M-x pdf-tools-install to rebuild.

;;; Activate at startup
;; (pdf-tools-install)

;;; Activate on demand
(pdf-loader-install)


(provide 'init-pdf)
