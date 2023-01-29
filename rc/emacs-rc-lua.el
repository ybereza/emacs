;; emacs-rc-lua.el ---
;; Copyright (c) 2022 Yury Bereza

(defun my-lua-mode-hook()
  (setq indent-tabs-mode nil)
  (setq lua-indent-level 2)
  (setq tab-width 2))

(add-hook 'lua-mode-hook 'my-lua-mode-hook)
