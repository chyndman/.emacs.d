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

;; Font
(defun init-font (fonts)
  (when fonts
    (if (not (find-font (font-spec :name (car fonts))))
        (init-font (cdr fonts))
      (set-face-attribute 'default nil :family (car fonts))
      (set-face-attribute 'fixed-pitch nil :family (car fonts)))))
(init-font '("MonoLisa"
             "Cascadia Code"))

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
     "markdown-mode")))

;; Helm
(require 'helm-command)
(require 'helm-mode)
(require 'helm)
(helm-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "M-s o") 'helm-occur)

;; Markdown
(require 'markdown-mode)
(setq markdown-header-scaling t)
(set-face-attribute 'markdown-header-face nil :inherit '(fixed-pitch-face font-lock-function-name-face))
