;; Core
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(show-paren-mode t)
(column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      auto-save-default nil
      make-backup-files nil
      visible-bell nil
      ring-bell-function 'ignore
      mouse-wheel-progressive-speed nil
      c-default-style "stroustrup")

;; Keymap
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;; Font
(ignore-errors (set-frame-font "Intel One Mono"))
(set-face-attribute 'fixed-pitch nil :family 'unspecified)

;; Packages
(unless (package-installed-p 'counsel)
  (package-refresh-contents)
  (package-install 'markdown-mode)
  (package-install 'counsel))

;; Ivy/Counsel/Swiper
(ivy-mode 1)
(counsel-mode 1)
(global-set-key (kbd "C-s") 'swiper)
(global-set-key (kbd "C-r") 'swiper-backward)
