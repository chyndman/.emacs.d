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

;; Theme
(load-theme 'deeper-blue t)

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

;; package setup
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))
(unless (package-installed-p 'use-package) (package-install 'use-package))
(require 'use-package)

;; Helm
(use-package helm
  :ensure t
  :bind
  (("M-x" . helm-M-x)
   ("C-x r b" . helm-filtered-bookmarks)
   ("C-x C-f" . helm-find-files)
   ("C-x C-b" . helm-buffers-list)
   ("M-s o" . helm-occur))
  :config
  (helm-mode 1))

;; Markdown
(use-package markdown-mode
  :ensure t
  :init
  (setq markdown-header-scaling t)
  :config
  (set-face-attribute 'markdown-header-face nil :inherit '(fixed-pitch-face font-lock-function-name-face)))
