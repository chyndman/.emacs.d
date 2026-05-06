;; Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file) (load custom-file 't))

;; Homebrew
(when (eq system-type 'darwin)
  (let ((default-directory "/opt/homebrew/share/emacs/site-lisp/"))
    (when (file-directory-p default-directory)
      (normal-top-level-add-subdirs-to-load-path))))

;; Core
(global-hl-line-mode t)
(show-paren-mode t)
(column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq auto-save-default nil
      make-backup-files nil
      visible-bell nil
      ring-bell-function 'ignore
      mouse-wheel-progressive-speed nil
      uniquify-buffer-name-style 'post-forward-angle-brackets
      isearch-lazy-count t
      server-kill-new-buffers nil
      flymake-fringe-indicator-position 'right-fringe
      flymake-margin-indicator-position 'right-margin)

;; Input
(global-set-key (kbd "C-x k") 'kill-current-buffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(when (functionp 'xterm-mouse-mode)
  (xterm-mouse-mode t)
  (when (eq system-type 'darwin)
    (global-set-key (kbd "<wheel-up>") 'scroll-down-line)
    (global-set-key (kbd "<wheel-down>") 'scroll-up-line)))
(when (functionp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))
(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;; Org
(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c c") #'org-capture)
(global-set-key (kbd "C-c m") #'org-cycle-agenda-files)
(setq org-directory "~/Org/"
      org-M-RET-may-split-line '((default . nil))
      org-insert-heading-respect-content t
      org-blank-before-new-entry '((heading . nil) (plain-list-item . nil)))

;; C/C++
(add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode))
(setq c-default-style "stroustrup")
(require 'cmake-mode nil 'noerror)

;; MCT
(use-package mct
  :ensure t
  :config
  (mct-mode 1))

;; Markdown
(use-package markdown-mode
  :ensure t)
