;;; init-ibuffer.el --- ibuffer settings -*- lexical-binding: t -*-
;;; Commentary:

;; TODO: enhance ibuffer-fontification-alist
;;   See http://www.reddit.com/r/emacs/comments/21fjpn/fontifying_buffer_list_for_emacs_243/

;;; Code:

;;; This closes all other frames
;; (require-package 'fullframe)
;; (after-load 'ibuffer
;;   (fullframe ibuffer ibuffer-quit))

(require-package 'ibuffer-vc)
;; (unless (site-lisp-library-loadable-p 'ibuffer-vc)
;;   (display-warning 'site-lisp "/site-lisp/ibuffer-vc does not exist or conflicts with a (M)ELPA package.\n  Clone from git@github.com:devnich/ibuffer-vc.git\n  Follow installation instructions in /site-lisp/README"))
;; (require 'ibuffer-vc)

;; (define-ibuffer-sorter filename/path)
;;; https://emacs.stackexchange.com/questions/10621/how-to-get-ibuffer-to-use-directory-tree-as-filter-groups

;; Sorting by filename/process gives stable sorting of files but not
;; directories. To get stable sorting of directories, we need to define a new
;; ibuffer sorter that followers filename/process as the default sort, and
;; falls through to alphabetic for everything else. Once this is done, we
;; probably no longer need to maintain a custome version of ibuffer-vc, since
;; it's sorting stability was ephemeral. Here are the relevant sorters from
;; ibuf-ext.el:
;; ;;;###autoload (autoload 'ibuffer-do-sort-by-filename/process "ibuf-ext")
;; (define-ibuffer-sorter filename/process
;;   "Sort the buffers by their file name/process name."
;;   (:description "file name")
;;   (string-lessp
;;    ;; FIXME: For now just compare the file name and the process name
;;    ;; (if it exists).  Is there a better way to do this?
;;    (or (buffer-file-name (car a))
;;        (let ((pr-a (get-buffer-process (car a))))
;;          (and (processp pr-a) (process-name pr-a))))
;;    (or (buffer-file-name (car b))
;;        (let ((pr-b (get-buffer-process (car b))))
;;          (and (processp pr-b) (process-name pr-b))))))

;; ;;;###autoload (autoload 'ibuffer-do-sort-by-alphabetic "ibuf-ext")
;; (define-ibuffer-sorter alphabetic
;;   "Sort the buffers by their names.
;; Ordering is lexicographic."
;;   (:description "buffer name")
;;   (string-lessp
;;    (buffer-name (car a))
;;    (buffer-name (car b))))


(defun ibuffer-set-up-preferred-filters ()
  (ibuffer-vc-set-filter-groups-by-vc-root)
  (unless (eq ibuffer-sorting-mode 'filename/process)
    (ibuffer-do-sort-by-filename/process)))

(add-hook 'ibuffer-hook 'ibuffer-set-up-preferred-filters)

(setq-default ibuffer-show-empty-filter-groups nil)


(after-load 'ibuffer
  ;; Use human readable Size column instead of original one
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (file-size-human-readable (buffer-size))))


;; Modify the default ibuffer-formats (toggle with `)
(setq ibuffer-formats
      '((mark modified read-only vc-status-mini " "
              (name 30 30 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 12 12 :left :elide)
              " "
              vc-relative-file)
        (mark modified read-only vc-status-mini " "
              (name 30 30 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 14 14 :left :elide)
              " "
              (vc-status 12 12 :left)
              " "
              vc-relative-file)))

(setq ibuffer-filter-group-name-face 'font-lock-doc-face)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
