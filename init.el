;; Copyright (c) 2020, Chris Hyndman
;; SPDX-License-Identifier: BSD-3-Clause

;; Core
(menu-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(show-paren-mode t)
(column-number-mode t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq-default indent-tabs-mode nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      visible-bell nil
      ring-bell-function 'ignore
      inhibit-startup-screen t
      auto-save-default nil
      mouse-wheel-progressive-speed nil)
(global-unset-key (kbd "C-x C-z"))

;; Font
(when (display-graphic-p)
  (let ((font "Julia Mono"))
    (when (find-font (font-spec :name font))
      (set-face-attribute 'default nil :family font)
      (set-face-attribute 'fixed-pitch nil :family font)))
  (set-face-attribute 'default nil :height 110))

;; Theming
(setq onehalf-light-colors
      '((black . "#383a42")
        (red . "#e45649")
        (green . "#50a14f")
        (yellow . "#c18401")
        (blue . "#0184bc")
        (magenta . "#a626a4")
        (cyan . "#0997b3")
        (white . "#fafafa")
        (comment . "#a0a1a7")
        (foreground . "#383a42")
        (background . "#fafafa")
        (selection . "#bfceff")))
(defun init-bg-color (face color)
  (set-face-background face (alist-get color onehalf-light-colors)))
(defun init-fg-color (face color)
  (set-face-foreground face (alist-get color onehalf-light-colors)))
(when (display-graphic-p)
  (init-bg-color 'default 'background)
  (init-fg-color 'default 'foreground))
(init-fg-color 'font-lock-comment-delimiter-face 'comment)
(init-fg-color 'font-lock-comment-face 'comment)
(init-fg-color 'font-lock-doc-face 'comment)
(init-fg-color 'font-lock-builtin-face 'magenta)
(init-fg-color 'font-lock-constant-face 'cyan)
(init-fg-color 'font-lock-function-name-face 'blue)
(init-fg-color 'font-lock-keyword-face 'red)
(init-fg-color 'font-lock-preprocessor-face 'yellow)
(init-fg-color 'font-lock-string-face 'green)
(init-fg-color 'font-lock-type-face 'yellow)
(init-fg-color 'font-lock-variable-name-face 'foreground)
(init-bg-color 'region 'selection)

;; C/C++
(setq c-default-style "stroustrup")

;; Start server
(server-start)

;; contrib load paths
(let ((default-directory (concat user-emacs-directory "contrib")))
  (normal-top-level-add-to-load-path
   '("async"
     "popup"
     "helm"
     "markdown-mode"
     "go-mode")))

;; Helm
(require 'helm-bookmark)
(require 'helm-ring)
(require 'helm-command)
(require 'helm-mode)
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-s o") 'helm-occur)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(helm-mode 1)

;; Markdown
(require 'markdown-mode)
(setq markdown-header-scaling t)
(set-face-attribute 'markdown-header-face nil :inherit '(fixed-pitch-face font-lock-function-name-face))

;; Go
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook (lambda () (setq-local tab-width 4)))
