;; UI
(setq frame-inhibit-implied-resize t
      inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)
(menu-bar-mode -1)
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))
(if (functionp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (functionp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(if (functionp 'xterm-mouse-mode) (xterm-mouse-mode t))

;; Theme
(load-theme 'deeper-blue t)
(set-face-attribute 'font-lock-comment-face nil :foreground "#808080")
(set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "#808080")
