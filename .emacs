;; Packages repos
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list 'package-archives
               '("melpa" . "https://melpa.org/packages/"))
  (package-initialize))

;; Local packages
(let ((default-directory "~/emacs/packages/"))
  (normal-top-level-add-subdirs-to-load-path))

;; try to fucking set this environment variables
(setenv "PATH" (concat (getenv "PATH") ":/opt/go/bin:/opt/llvm/bin:/home/ybereza/go/bin"))
(add-to-list 'exec-path "/opt/go/bin")
(add-to-list 'exec-path "/opt/llvm/bin")
(add-to-list 'exec-path "/home/ybereza/go/bin")

;; Solarized package with themes
(require 'solarized)

;; git-link for coping links to gitlab repos
(require 'git-link)

;; History
(require 'savehist)
(require 'recentf)
(setq recentf-max-menu-items 25)
;; save recent opened files
(recentf-mode 1)
;; save cursor position in files
(save-place-mode 1)

(ido-mode 1)
(ido-everywhere 1)

;; Temporary Files
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

;;system clipboard. use shift-insert to paste from windows/mac clipboard
(require 'simpleclip)
(simpleclip-mode 1)

;;Defaults
(setq user-full-name "Yury Bereza")
(setq user-mail-address "ybereza@gmail.com")

(setenv "LANG" "en_US.UTF-8")

;; Maximized without borders
(add-to-list 'default-frame-alist '(undecorated . t))
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq use-dilog-box nil)
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
(setq-default truncate-lines t)
(setq-default magit-log-margin '(t "%Y-%m-%d " magit-log-margin-width t 18))

(setq custom-file "~/emacs/custom.el")

;; git-link
(setq git-link-open-in-browser t)
(setq git-link-add-to-kill-ring nil)

(put 'eval-expression 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'set-goal-column 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)

(global-auto-revert-mode t)
(column-number-mode)
(ffap-bindings)
(transient-mark-mode)
(show-paren-mode)
(abbrev-mode)
(menu-bar-mode 0)
(blink-cursor-mode t)
(global-font-lock-mode t)
(global-display-line-numbers-mode nil)
(windmove-default-keybindings)

(global-font-lock-mode t)
(global-display-line-numbers-mode 0)
(windmove-default-keybindings)

(load "~/emacs/rc/emacs-rc-ccmode.el")
(load "~/emacs/rc/emacs-rc-cmake.el")
(load "~/emacs/rc/emacs-rc-python.el")
(load "~/emacs/rc/emacs-rc-lua.el")
(load "~/emacs/rc/emacs-rc-custom.el")
(load "~/emacs/rc/emacs-rc-go.el")

;; Keybindings
;; Simpleclip keybinding
(global-set-key (kbd "C-C C-w") 'simpleclip-copy)
(global-set-key (kbd "C-C C-y") 'simpleclip-paste)
;; Navigation keybindings
(global-set-key (kbd "C-x C-g") 'goto-line)
(global-set-key (kbd "M-z") 'visual-line-mode)
;; Search
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'query-replace-regexp)
(global-set-key (kbd "C-S-s") 'rgrep)
;; Comments
(global-set-key (kbd "C-C C-/") 'comment-or-uncomment-region)
(global-set-key (kbd "C-C C-u") 'uncomment-region)
;; Misc
(global-set-key (kbd "C-C C-SPC") 'global-whitespace-mode)
(global-set-key (kbd "C-c z") 'show-file-name)
(global-set-key (kbd "<f5>") 'revert-buffer-no-confirm)
;; Switch windows
(global-set-key [M-left] 'windmove-left)
(global-set-key [M-right] 'windmove-right)
(global-set-key [M-up] 'windmove-up)
(global-set-key [M-down] 'windmove-down)
;; git-link
(global-set-key (kbd "C-c g p") 'git-link)

;;GUI specific settings
(when (fboundp 'window-system)
  (when (window-system)
    (scroll-bar-mode -1)
    (tool-bar-mode -1)
    (when (eq system-type 'windows-nt)
      (set-face-attribute 'default nil :font "Consolas-14"))
    (when (eq system-type 'darwin)
      (setq mac-command-modifier 'meta)
      (setq mac-option-modifier 'meta)
      (set-face-attribute 'default nil :font "Consolas-16"))
    (when (eq system-type 'gnu/linux)
      (set-face-attribute 'default nil :font "monospace-13"))))

(setq solarized-use-variable-pitch nil
      solarized-scale-org-headlines nil)

(load-file custom-file)
(put 'dired-find-alternate-file 'disabled nil)
