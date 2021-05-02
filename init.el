;; Copyright (c) 2020, Chris Hyndman
;; SPDX-License-Identifier: BSD-3-Clause

(add-to-list 'load-path "~/.emacs.d/better-defaults/")
(require 'better-defaults)

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

(unless package-archive-contents (package-refresh-contents))

(dolist (package '(solarized-theme))
  (unless (package-installed-p package) (package-install package)))

(add-to-list 'default-frame-alist '(font . "MonoLisa-10"))
(load-theme 'solarized-light t)
(setq inhibit-startup-screen t)

(setq c-default-style "stroustrup")

(server-start)
