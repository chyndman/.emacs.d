;;; -*- lexical-binding: t -*-

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
(global-auto-revert-mode t)
(fido-mode t)
(setq-default indent-tabs-mode nil)
(setq auto-save-default nil
      make-backup-files nil
      visible-bell nil
      ring-bell-function 'ignore
      mouse-wheel-progressive-speed nil
      uniquify-buffer-name-style 'post-forward-angle-brackets
      isearch-lazy-count t
      server-kill-new-buffers nil)

;; Mouse
(when (functionp 'xterm-mouse-mode)
  (xterm-mouse-mode t)
  (when (eq system-type 'darwin)
    (define-key global-map (kbd "<wheel-up>") #'scroll-down-line)
    (define-key global-map (kbd "<wheel-down>") #'scroll-up-line)))
(when (functionp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t))

;; Keymap
(define-key global-map (kbd "C-x C-b") #'ibuffer)
(define-key global-map (kbd "C-c a") #'org-agenda)
(define-key global-map (kbd "C-c c") #'org-capture)
(define-key global-map (kbd "M-g x") #'xref-find-apropos)

;; Flymake
(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "M-n") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") #'flymake-goto-prev-error))
(setq flymake-fringe-indicator-position 'right-fringe
      flymake-margin-indicator-position 'right-margin)

;; Org
(setq org-priority-lowest ?H
      org-priority-default ?H
      org-M-RET-may-split-line '((default . nil))
      org-insert-heading-respect-content t
      org-directory (file-truename "~/org")
      org-agenda-files (list (concat org-directory "/bridge.org")
                             (concat org-directory "/lobby.org")
                             (concat org-directory "/deepstorage.org"))
      org-refile-targets '((org-agenda-files :maxlevel . 2)
                           (nil :maxlevel . 2))
      org-refile-use-outline-path 'file
      org-refile-allow-creating-parent-nodes 'confirm
      org-default-notes-file (concat org-directory "/shuttlebay.org"))

;; treesit
(require 'treesit)
(setq treesit-language-source-alist
      '((c          . ("https://github.com/tree-sitter/tree-sitter-c" "v0.24.2"))
        (cpp        . ("https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4"))
        (cmake      . ("https://github.com/uyha/tree-sitter-cmake" "v0.7.5"))
        (markdown   . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.5.3" "tree-sitter-markdown/src"))
        (markdown-inline . ("https://github.com/tree-sitter-grammars/tree-sitter-markdown" "v0.5.3" "tree-sitter-markdown-inline/src"))
        (bash       . ("https://github.com/tree-sitter/tree-sitter-bash" "v0.25.1"))
        (json       . ("https://github.com/tree-sitter/tree-sitter-json" "v0.24.8"))
        (python     . ("https://github.com/tree-sitter/tree-sitter-python" "v0.25.0"))))
(dolist (lang-source treesit-language-source-alist)
  (let ((lang (car lang-source)))
    (unless (treesit-language-available-p lang)
      (treesit-install-language-grammar lang))))
(setq major-mode-remap-alist
      '((c-mode          . c-ts-mode)
        (c++-mode        . c++-ts-mode)
        (c-or-c++-mode   . c-or-c++-ts-mode)
        (cmake-mode      . cmake-ts-mode)
        (markdown-mode   . markdown-ts-mode)
        (sh-mode         . bash-ts-mode)
        (json-mode       . json-ts-mode)
        (js-json-mode    . json-ts-mode)
        (python-mode     . python-ts-mode)))
(when (= emacs-major-version 31)
  (require 'markdown-ts-mode-x nil 'noerror)
  (dolist (re '("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'"))
    (add-to-list 'auto-mode-alist (cons re 'markdown-ts-mode))))
