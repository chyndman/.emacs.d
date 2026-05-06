;; GC
(setq gc-cons-threshold (* 32 1024 1024))

;; UI
(setq frame-inhibit-implied-resize t
      inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)
(menu-bar-mode -1)
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))

;; Theme
(setq modus-vivendi-palette-overrides
      '((bg-main "unspecified-bg")))
(load-theme 'modus-vivendi t)
(set-face-attribute 'fixed-pitch nil :family 'unspecified)
