;; UI
(setq frame-inhibit-implied-resize t
      inhibit-startup-screen t
      initial-scratch-message nil
      initial-major-mode 'fundamental-mode)
(menu-bar-mode -1)
(if (functionp 'tool-bar-mode) (tool-bar-mode -1))
(if (functionp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (functionp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(when (functionp 'xterm-mouse-mode)
  (xterm-mouse-mode t)
  (when (eq system-type 'darwin)
    (global-set-key (kbd "<wheel-up>") 'scroll-down-line)
    (global-set-key (kbd "<wheel-down>") 'scroll-up-line)))

;; Homebrew
(let ((default-directory "/opt/homebrew/share/emacs/site-lisp/"))
  (when (file-directory-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))
