;;; -*- lexical-binding: t -*-

;; GC
(setq gc-cons-threshold (* 32 1024 1024))

;; UI
(setq frame-inhibit-implied-resize t
      inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)
(menu-bar-mode -1)
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))
(setq-default mode-line-end-spaces nil)
(set-display-table-slot standard-display-table
                        'vertical-border 
                        (make-glyph-code ?│))

;; Theme
(setq modus-themes-common-palette-overrides
      '((bg-main "unspecified-bg")
        (border-mode-line-active unspecified)
        (border-mode-line-inactive unspecified)))
(load-theme 'modus-operandi-tinted t)
(set-face-attribute 'fixed-pitch nil :family 'unspecified)
