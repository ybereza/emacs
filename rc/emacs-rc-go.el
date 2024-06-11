;; emacs-rc-go.el ---
;; Copyright (c) 2024 Yury Bereza

(defun my-go-mode-hook()
  (setq indent-tabs-mode t)
  (setq tab-width 4))

(add-hook 'go-mode-hook 'my-go-mode-hook)
