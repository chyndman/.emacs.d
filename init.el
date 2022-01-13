;; Copyright (c) 2020, Chris Hyndman
;; SPDX-License-Identifier: BSD-3-Clause

;; Core UI/behavior (mostly selected from better-defaults)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(horizontal-scroll-bar-mode -1)
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)
(show-paren-mode t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq-default indent-tabs-mode nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory "backups"))))
(setq visible-bell nil
      ring-bell-function 'ignore
      inhibit-startup-screen t
      auto-save-default nil)

;; Font
(set-face-attribute 'default nil :family "MonoLisa")

;; package bootstrap
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; use-package install
(unless (package-installed-p 'use-package) (package-install 'use-package))

;; Use packages if use-package is available
(when (package-installed-p 'use-package)
  (require 'use-package)

  (use-package ivy
    :ensure t
    :config
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t
          enable-recursive-minibuffers t))

  (use-package counsel
    :after ivy
    :ensure t
    :config
    (counsel-mode 1))

  (use-package swiper
    :after ivy
    :ensure t
    :bind
    (("C-s" . swiper)
     ("C-r" . swiper)))

  (use-package darktooth-theme
    :ensure t
    :config
    (load-theme 'darktooth t))

  (use-package markdown-mode
    :ensure t
    :init
    (setq markdown-header-scaling t))

  (when (package-installed-p 'cider)
    (use-package cider)))

;; C/C++
(setq c-default-style "stroustrup")

;; Start server
(server-start)
