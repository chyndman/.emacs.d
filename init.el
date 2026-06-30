;; Custom
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 't)

;; site-lisp
(let ((default-directory (concat user-emacs-directory "site-lisp")))
  (when (file-directory-p default-directory)
    (normal-top-level-add-subdirs-to-load-path)))
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

;; Keymap
(define-key global-map (kbd "C-x C-b") 'ibuffer)
(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;; Mouse
(when (functionp 'xterm-mouse-mode)
  (xterm-mouse-mode t)
  (when (eq system-type 'darwin)
    (define-key global-map (kbd "<wheel-up>") 'scroll-down-line)
    (define-key global-map (kbd "<wheel-down>") 'scroll-up-line)))
(when (functionp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;; C/C++
(add-to-list 'auto-mode-alist '("\\.cppm\\'" . c++-mode))
(setq c-default-style "stroustrup")
(require 'cmake-mode nil 'noerror)

;; Org
(use-package org
  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture))
  :custom
  (org-M-RET-may-split-line '((default . nil)))
  (org-insert-heading-respect-content t)
  (org-agenda-files (list (concat org-directory "/bridge.org")
                          (concat org-directory "/lobby.org")
                          (concat org-directory "/deepstorage.org")))
  (org-refile-targets '((org-agenda-files :maxlevel . 2)
                        (nil :maxlevel . 2)))
  (org-refile-use-outline-path 'file)
  (org-refile-allow-creating-parent-nodes 'confirm)
  (org-default-notes-file (concat org-directory "/shuttlebay.org")))

;; MCT
(use-package mct
  :ensure t
  :config
  (mct-mode 1))

;; TMR
(use-package tmr
  :ensure t
  :bind-keymap
  ("C-c t" . tmr-prefix-map)
  :config
  (tmr-mode-line-mode 1)
  :custom
  (tmr-timer-finished-functions
   '(tmr-print-message-for-finished-timer tmr-acknowledge-minibuffer)))

;; Markdown
(use-package markdown-mode
  :ensure t)
