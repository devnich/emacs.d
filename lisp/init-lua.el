;;; init-lua.el --- Support for Lua programming -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (maybe-require-package 'lua-mode)
  (add-auto-mode 'lua-mode "\\.lua\\'"))

(setq-default lua-indent-level 2)

(reformatter-define lua-format
  :program "lua-format"
  :args '("--indent-width=2" "--no-use-tab")
  :lighter "LuaFmt ")


(provide 'init-lua)
;;; init-lua.el ends here
