;; Copyright (c) 2020, Chris Hyndman
;; SPDX-License-Identifier: BSD-3-Clause

;; Start with better-defaults
(require 'better-defaults "~/.emacs.d/better-defaults/better-defaults.el")

;; Set more in-box variables
(setq inhibit-startup-screen t
      auto-save-default nil
      c-default-style "stroustrup")
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

  (use-package solarized-theme
    :ensure t
    :config
    (load-theme 'solarized-light t))

  (when (package-installed-p 'cider)
    (use-package cider)))

;; Start server
(server-start)
