;; emacs-rc-bash.el ---
;; Copyright (c) 2021 Yury Bereza

(require 'sh-mode)

(defun my-ss-mode-hook()
  (setq indent-tabs-mode nil)
  (setq tab-width 4)
  (c-toggle-electric-state -1))

(defun my-c++-mode-hook ()
  (setq c-indent-level 4)
  (setq c-basic-offset 4)
  (define-key c++-mode-map "\C-m"  'reindent-then-newline-and-indent)
  (define-key c++-mode-map "\C-d"  'kill-word)
  (define-key c++-mode-map "\C-co" 'ff-find-other-file)
  (c-toggle-electric-state -1)
  (c-set-offset 'namespace-open  0)
  (c-set-offset 'namespace-close  0)
  (c-set-offset 'innamespace 0))

(add-hook 'c-mode-hook 'my-c-mode-hook)
(add-hook 'c++-mode-hook 'my-c++-mode-hook)
