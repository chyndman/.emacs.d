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
(setq modus-themes-common-palette-overrides
      '((bg-main "unspecified-bg")
        (fg-main "unspecified-fg")))
(load-theme 'modus-operandi-tinted t)
(set-face-attribute 'fixed-pitch nil :family 'unspecified)
