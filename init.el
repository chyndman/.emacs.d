;; Theme/Font
(load-theme 'modus-vivendi t)
(unless (display-graphic-p)
  (set-face-attribute 'default nil :background "unspecified-bg"))
(when (display-graphic-p)
  (ignore-errors (set-frame-font "Intel One Mono")))
(set-face-attribute 'fixed-pitch nil :family 'unspecified)

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
      isearch-lazy-count t
      flymake-fringe-indicator-position 'right-fringe
      flymake-margin-indicator-position 'right-margin)
(fido-vertical-mode 1)
(if (functionp 'which-key-mode) (which-key-mode 1))

;; Keymap
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;; Org
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-archive-location "archive.org::"
      org-archive-file-header-format nil
      org-archive-save-context-info nil
      org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

;; C/C++
(add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode))
(setq c-default-style "stroustrup")
(require 'cmake-mode nil 'noerror)

;; Markdown
(when (>= emacs-major-version 29)
  (use-package markdown-mode
    :ensure t
    :init
    (setq markdown-header-scaling t)))
