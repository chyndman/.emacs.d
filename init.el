;; Copyright (c) 2020, Chris Hyndman
;; SPDX-License-Identifier: BSD-3-Clause

;; Start with better-defaults
(require 'better-defaults "~/.emacs.d/better-defaults/better-defaults.el")

;; Set more in-box variables
(setq inhibit-startup-screen t
      auto-save-default nil
      c-default-style "stroustrup")
(add-to-list 'default-frame-alist '(font . "MonoLisa-10"))

;; package bootstrap
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
(unless package-archive-contents (package-refresh-contents))

;; Install packages
(dolist (package '(solarized-theme))
  (unless (package-installed-p package) (package-install package)))

;; Use installed theme
(load-theme 'solarized-light t)

;; Start server
(server-start)
