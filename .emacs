(when (fboundp 'window-system)
  (when (window-system)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (when (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :font "JetBrains Mono-12"))
    (when (eq system-type 'darwin)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'meta)
    (set-face-attribute 'default nil :font "Fira Code-14"))
    (when (eq system-type 'gnu/linux)
      (set-face-attribute 'default nil :font "JetBrains Mono-12"))))

;;Packages repos
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

;;History
(require 'savehist)
(require 'recentf)
(setq recentf-max-menu-items 25)
;;(savehist-load)
(recentf-mode 1)

;;Temporary Files
(defvar user-temporary-file-directory "~/tmp")
(setq backup-by-copying t)
(setq backup-directory-alist
      `(("." . ,user-temporary-file-directory)
        (,tramp-file-name-regexp nil)))
(setq auto-save-list-file-prefix
      (concat user-temporary-file-directory ".auto-saves-"))
(setq auto-save-file-name-transforms
      `((".*" ,user-temporary-file-directory t)))
(make-directory user-temporary-file-directory t)

;;Autocomplete
;; (require 'auto-complete-config)
;; (setq ac-auto-start t)
;; (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;; (ac-config-default)

(require 'auto-complete)

;;system clipboard. use shift-insert to paste from windows/mac clipboard
(require 'simpleclip)
(simpleclip-mode 1)

;;Defaults
(setq user-full-name "Yury Bereza")
(setq user-mail-address "ybereza@gmail.com")

(setq frame-title-format "emacs - %b")
(setq inhibit-startup-message t)
(setq mark-even-if-inactive t)
(setq visible-bell nil)
(setq next-line-add-newlinens nil)
(setq suggest-key-bindings nil)
(setq show-trailing-whitespace nil)
(setq show-paren-style 'mixed)
(setq truncate-partial-width-windows nil)
(setq default-truncate-lines t)
(setq line-spacing 1)
(setq default-tab-width 4)
(setq left-fringe-width 0)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq kill-whole-line t)
(setq indent-tabs-mode nil)
(setq ring-bell-function 'ignore)

(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(auto-revert-mode t)
(column-number-mode)
(ffap-bindings)
(transient-mark-mode)
(show-paren-mode)
(iswitchb-mode)
(abbrev-mode)
(menu-bar-mode 0)
(blink-cursor-mode t)
(auto-complete-mode 0)
(global-font-lock-mode t)
(global-auto-complete-mode 0)
(global-display-line-numbers-mode t)

(load "~/emacs/rc/emacs-rc-ccmode.el")
(load "~/emacs/rc/emacs-rc-cmake.el")
(load "~/emacs/rc/emacs-rc-python.el")

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minibuffer-prompt ((t (:foreground "default")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (simpleclip cmake-mode auto-complete))))
