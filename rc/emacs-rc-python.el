;; emacs-rc-python.el ---
;; Copyright (c) 2020 Yury Bereza

(defun electric-pair ()
  "Insert character pair without sournding spaces"
  (interactive)
  (let (parens-require-spaces)
    (insert-pair)))

(defun my-python-mode-hook()
  (define-key python-mode-map "\C-m" 'newline-and-indent)
  (setq indent-tabs-mode nil)
  (setq python-continuation-offset 4)
  (setq python-indent-offset 4)
  (setq tab-width 4))

(add-hook 'python-mode-hook 'my-python-mode-hook)
