(require-package 'pdf-tools)

;; Supports retinal display, which uses more memory; see https://github.com/politza/pdf-tools/issues/51

(setq pdf-view-use-scaling t
      pdf-view-use-imagemagick nil)

(provide 'init-pdf)
