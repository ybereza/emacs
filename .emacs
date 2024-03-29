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
;;(require 'auto-complete)

;;system clipboard. use shift-insert to paste from windows/mac clipboard
(require 'simpleclip)
(simpleclip-mode 1)

;;Defaults
(setq user-full-name "Yury Bereza")
(setq user-mail-address "ybereza@gmail.com")

(setenv "LANG" "ru_RU.UTF-8")

(setq frame-title-format "%b - %f")
(setq inhibit-startup-message t)
(setq mark-even-if-inactive t)
(setq visible-bell nil)
(setq next-line-add-newlinens nil)
(setq suggest-key-bindings nil)
(setq show-trailing-whitespace t)
(setq whitespace-line-column 120)
(setq show-paren-style 'mixed)
(setq truncate-partial-width-windows t)
(setq default-truncate-lines t)
(setq line-spacing 1)
(setq default-tab-width 4)
(setq tab-width 4)
(setq left-fringe-width 0)
(setq scroll-up-aggressively 0.01)
(setq scroll-down-aggressively 0.01)
(setq kill-whole-line t)
(setq indent-tabs-mode nil)
(setq ring-bell-function 'ignore)
;; (setq ido-enable-flex-matching t)
;; (setq ido-everywhere t)
(setq-default truncate-lines t)
(setq-default magit-log-margin '(t "%Y-%m-%d " magit-log-margin-width t 18))

(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode t)
(column-number-mode)
(ffap-bindings)
(transient-mark-mode)
(show-paren-mode)
;;(ido-mode)
(abbrev-mode)
(menu-bar-mode 0)
(blink-cursor-mode t)
;; (auto-complete-mode 0)
;; (global-auto-complete-mode 0)
(global-font-lock-mode t)
(global-display-line-numbers-mode t)
(windmove-default-keybindings)

(load "~/emacs/rc/emacs-rc-ccmode.el")
(load "~/emacs/rc/emacs-rc-cmake.el")
(load "~/emacs/rc/emacs-rc-python.el")
(load "~/emacs/rc/emacs-rc-lua.el")
(load "~/emacs/rc/emacs-rc-custom.el")

;;Keybindings
;;Simpleclip keybinding
(global-set-key (kbd "C-C C-w") 'simpleclip-copy)
(global-set-key (kbd "C-C C-y") 'simpleclip-paste)
;;Navigation keybindings
(global-set-key (kbd "C-x C-g") 'goto-line)
(global-set-key (kbd "M-z") 'visual-line-mode)
;;Search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'query-replace-regexp)
(global-set-key (kbd "C-S-s") 'rgrep)
;;Comments
(global-set-key (kbd "C-C ;") 'comment-or-uncomment-region)
(global-set-key (kbd "C-C C-u") 'uncomment-region)
;;Misc
(global-set-key (kbd "C-C C-SPC") 'global-whitespace-mode)
(global-set-key (kbd "C-c z") 'show-file-name)
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)
;;Switch windows
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)

;;GUI specific settings
(when (fboundp 'window-system)
  (when (window-system)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (when (eq system-type 'windows-nt)
      (set-face-attribute 'default nil :font "JetBrains Mono-16"))
    (when (eq system-type 'darwin)
      (setq mac-command-modifier 'control)
      (setq mac-option-modifier 'meta)
      (set-face-attribute 'default nil :font "JetBrains Mono-16"))
    (when (eq system-type 'gnu/linux)
      (set-face-attribute 'default nil :font "JetBrains Mono-18")
      (load-theme 'solarized-light-high-contrast t))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(custom-safe-themes
   '("c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "0fffa9669425ff140ff2ae8568c7719705ef33b7a927a0ba7c5e2ffcfac09b75" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" default))
 '(global-whitespace-mode nil)
 '(package-selected-packages
   '(ein python-black systemd lua-mode docker-compose-mode dockerfile-mode solarized-theme magit markdown-mode simpleclip cmake-mode auto-complete))
 '(solarized-high-contrast-mode-line t))
(put 'dired-find-alternate-file 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
