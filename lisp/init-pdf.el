;;; init-pdf.el --- Display pdfs -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require-package 'pdf-tools)

;;; Documentation available at https://github.com/vedang/pdf-tools
;;; This package has two non-Emacs dependencies:
;;;   1. poppler (PDF viewer)
;;;   2. fontconfig (allow poppler to use all installed system fonts)

;;; After upgrading poppler, run (pdf-tools-install) in Emacs to recompile
;;; poppler with appropriate settings.

;;; Activate at startup
;; (pdf-tools-install)

;;; Activate on demand
(pdf-loader-install)


(provide 'init-pdf)
