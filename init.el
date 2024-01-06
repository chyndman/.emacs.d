;; Core
(menu-bar-mode -1)
(when (display-graphic-p)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (horizontal-scroll-bar-mode -1))
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(show-paren-mode t)
(column-number-mode t)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq-default indent-tabs-mode nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      visible-bell nil
      ring-bell-function 'ignore
      inhibit-startup-screen t
      auto-save-default nil
      mouse-wheel-progressive-speed nil)
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))
(setq frame-background-mode 'dark)

;; Flymake
(require 'flymake)
(define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
(define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)

;; Font
(when (display-graphic-p)
  (defun init-font (fonts)
    (when fonts
      (if (not (find-font (font-spec :name (car fonts))))
          (init-font (cdr fonts))
        (set-face-attribute 'default nil :family (car fonts))
        (set-face-attribute 'fixed-pitch nil :family (car fonts)))))
  (init-font '("IntelOneMono"
               "Intel One Mono"
               "CascadiaCode"
               "Cascadia Code"
               "NotoMono"
               "Noto Mono")))

;; contrib load paths
(let ((default-directory (concat user-emacs-directory "contrib")))
  (normal-top-level-add-to-load-path
   '("swiper"
     "markdown-mode"
     "go-mode"
     "rust-mode")))

;; GUI Theme
(when (display-graphic-p)
  (load-theme 'deeper-blue t))

;; Terminal Theme
(when (not (display-graphic-p))
  (set-face-foreground 'font-lock-comment-delimiter-face "brightblack")
  (set-face-foreground 'font-lock-comment-face "brightblack")
  (set-face-foreground 'font-lock-doc-face "brightblack")
  (set-face-foreground 'font-lock-builtin-face "cyan")
  (set-face-foreground 'font-lock-constant-face "magenta")
  (set-face-foreground 'font-lock-function-name-face "cyan")
  (set-face-foreground 'font-lock-keyword-face "blue")
  (set-face-foreground 'font-lock-preprocessor-face "blue")
  (set-face-foreground 'font-lock-string-face "green")
  (set-face-foreground 'font-lock-type-face "yellow")
  (set-face-foreground 'font-lock-variable-name-face "red"))

;; Ivy
(require 'counsel)
(ivy-mode 1)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(global-set-key (kbd "M-y") 'counsel-yank-pop)
(global-set-key (kbd "<f1> f") 'counsel-describe-function)
(global-set-key (kbd "<f1> v") 'counsel-describe-variable)
(global-set-key (kbd "<f1> l") 'counsel-find-library)
(global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
(global-set-key (kbd "<f2> u") 'counsel-unicode-char)
(global-set-key (kbd "<f2> j") 'counsel-set-variable)
(global-set-key (kbd "C-x b") 'ivy-switch-buffer)
(global-set-key (kbd "C-c v") 'ivy-push-view)
(global-set-key (kbd "C-c V") 'ivy-pop-view)

;; C/C++
(setq c-default-style "stroustrup")

;; Markdown
(require 'markdown-mode)
(setq markdown-header-scaling t)
(set-face-attribute 'markdown-header-face nil :inherit '(fixed-pitch-face font-lock-function-name-face))

;; Go
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook (lambda () (setq-local tab-width 4)))

;; Rust
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
