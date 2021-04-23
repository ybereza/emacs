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

;;Local packages
(let ((default-directory "~/emacs/packages/"))
  (normal-top-level-add-subdirs-to-load-path))

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
(setq truncate-partial-width-windows t)
(setq default-truncate-lines t)
(setq line-spacing 1)
(setq default-tab-width 4)
(setq left-fringe-width 0)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq kill-whole-line t)
(setq indent-tabs-mode nil)
(setq ring-bell-function 'ignore)
(setq-default truncate-lines t)

(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(auto-revert-mode t)
(column-number-mode)
(ffap-bindings)
(transient-mark-mode)
(show-paren-mode)
(ido-mode)
(abbrev-mode)
(menu-bar-mode 0)
(blink-cursor-mode t)
(auto-complete-mode 0)
(global-font-lock-mode t)
(global-auto-complete-mode 0)
(global-display-line-numbers-mode t)
(windmove-default-keybindings)

(load "~/emacs/rc/emacs-rc-ccmode.el")
(load "~/emacs/rc/emacs-rc-cmake.el")
(load "~/emacs/rc/emacs-rc-python.el")

;;Keybindings
;;Simpleclip keybinding
(global-set-key (kbd "C-C C-w") 'simpleclip-cup)
(global-set-key (kbd "C-C M-w") 'simpleclip-copy)
(global-set-key (kbd "C-C C-y") 'simpleclip-paste)
;;Navigation keybindings
(global-set-key (kbd "C-C C-g") 'goto-line)
;;Search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
;;Comments
(global-set-key (kbd "C-C C-c") 'comment-region)
(global-set-key (kbd "C-C C-u") 'uncomment-region)
;;Misc
(global-set-key (kbd "C-C C-SPC") 'global-whitespace-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(magit-diff-added ((t (:background "blue" :foreground "white"))))
 '(magit-diff-added-highlight ((t (:background "blue" :foreground "white"))))
 '(magit-diff-base ((t (:background "magenta" :foreground "white"))))
 '(magit-diff-base-highlight ((t (:background "magenta" :foreground "white"))))
 '(magit-diff-removed ((t (:background "magenta" :foreground "white"))))
 '(magit-diff-removed-highlight ((t (:background "magenta" :foreground "white"))))
 '(minibuffer-prompt ((t (:foreground "default")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (magit markdown-mode simpleclip cmake-mode auto-complete))))
(put 'dired-find-alternate-file 'disabled nil)
