;; emacs-rc-go.el ---
;; Copyright (c) 2024 Yury Bereza

(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))

(defun my-go-mode-hook()
  (setq indent-tabs-mode t)
  (setq tab-width 4))

(add-hook 'go-mode-hook 'my-go-mode-hook)
;; (add-hook 'go-mode-hook 'lsp-deferred)
