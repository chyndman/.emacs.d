;; Copyright (c) 2020, Chris Hyndman
;; SPDX-License-Identifier: BSD-3-Clause

;; Core
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
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
(defun init-font (fonts)
  (when fonts
    (if (not (find-font (font-spec :name (car fonts))))
        (init-font (cdr fonts))
      (set-face-attribute 'default nil :family (car fonts))
      (set-face-attribute 'fixed-pitch nil :family (car fonts)))))
(init-font '("IBM Plex Mono"
             "Cascadia Code"))
(set-face-attribute 'default nil :height 110)

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

;; Colors
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
(setq colors onehalf-light-colors)
(defun init-bg-color (face color)
  (set-face-background face (alist-get color colors color)))
(defun init-fg-color (face color)
  (set-face-foreground face (alist-get color colors color)))

;; Syntax Theme
(init-bg-color 'default "white")
(init-fg-color 'default "black")
(init-fg-color 'font-lock-builtin-face "coral4")
(init-fg-color 'font-lock-comment-delimiter-face "gray50")
(init-fg-color 'font-lock-comment-face "gray50")
(init-fg-color 'font-lock-constant-face "DarkOliveGreen4")
(init-fg-color 'font-lock-doc-face "gray50")
(init-fg-color 'font-lock-function-name-face "DarkGoldenrod4")
(init-fg-color 'font-lock-keyword-face "NavyBlue")
(init-fg-color 'font-lock-preprocessor-face "NavyBlue")
(init-fg-color 'font-lock-string-face "coral3")
(init-fg-color 'font-lock-type-face "DarkCyan")
(init-fg-color 'font-lock-variable-name-face "SeaGreen4")

;; UI Theme
;; (init-bg-color 'region 'selection)
;; (init-bg-color 'show-paren-match 'black)
;; (init-fg-color 'show-paren-match 'white)
;; (init-bg-color 'show-paren-mismatch 'yellow)
;; (init-fg-color 'show-paren-mismatch 'white)
