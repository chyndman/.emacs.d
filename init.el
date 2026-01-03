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
      server-kill-new-buffers nil
      flymake-fringe-indicator-position 'right-fringe
      flymake-margin-indicator-position 'right-margin)

;; Keymap
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;; Font
(ignore-errors (set-frame-font "Intel One Mono"))
(set-face-attribute 'fixed-pitch nil :family 'unspecified)

;; C/C++
(add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode))
(setq c-default-style "stroustrup")

;; Packages
(unless (package-installed-p 'markdown-mode)
  (package-refresh-contents)
  (package-install 'counsel)
  (package-install 'markdown-mode))

;; Ivy/Counsel/Swiper
(ivy-mode 1)
(counsel-mode 1)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-isearch-C-r)
