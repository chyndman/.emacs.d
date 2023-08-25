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
  (init-font '("JuliaMono"
               "Julia Mono"
               "CascadiaCode"
               "Cascadia Code"
               "NotoMono"
               "Noto Mono")))

;; Theming
(setq onehalf-light-swatches
      '((red . "#e45649")
        (green . "#50a14f")
        (yellow . "#c18401")
        (blue . "#0184bc")
        (magenta . "#a626a4")
        (cyan . "#0997b3")
        (comment . "#a0a1a7")
        (selection . "#bfceff")
        (foreground . "#383a42")
        (background . "#fafafa")))
(setq terminal-base-swatches
      '((red . "red")
        (green . "green")
        (yellow . "yellow")
        (blue . "blue")
        (magenta . "magenta")
        (cyan . "cyan")
        (comment . "brightblack")
        (selection . "brightblue")))
(setq swatches
      (if (display-graphic-p) onehalf-light-swatches terminal-base-swatches))
(defun swatch-to-color (swatch) (alist-get swatch swatches))
(defun init-face-bg (face swatch)
  (let ((color (swatch-to-color swatch)))
    (when color (set-face-background face color))))
(defun init-face-fg (face swatch)
  (let ((color (swatch-to-color swatch)))
    (when color (set-face-foreground face color))))

;; Faces
(init-face-bg 'default 'background)
(init-face-fg 'default 'foreground)
(init-face-fg 'font-lock-comment-delimiter-face 'comment)
(init-face-fg 'font-lock-comment-face 'comment)
(init-face-fg 'font-lock-doc-face 'comment)
(init-face-fg 'font-lock-builtin-face 'magenta)
(init-face-fg 'font-lock-constant-face 'cyan)
(init-face-fg 'font-lock-function-name-face 'blue)
(init-face-fg 'font-lock-keyword-face 'red)
(init-face-fg 'font-lock-preprocessor-face 'yellow)
(init-face-fg 'font-lock-string-face 'green)
(init-face-fg 'font-lock-type-face 'yellow)
(set-face-foreground 'font-lock-variable-name-face nil)
(init-face-bg 'region 'selection)
(when (not (display-graphic-p))
  (init-face-fg 'minibuffer-prompt 'blue))

;; C/C++
(setq c-default-style "stroustrup")

;; contrib load paths
(let ((default-directory (concat user-emacs-directory "contrib")))
  (normal-top-level-add-to-load-path
   '("async"
     "popup"
     "helm"
     "markdown-mode"
     "go-mode"
     "rust-mode")))

;; Helm
(require 'helm-bookmark)
(require 'helm-ring)
(require 'helm-command)
(require 'helm-mode)
(require 'helm)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x r b") 'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-s o") 'helm-occur)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-h SPC") 'helm-all-mark-rings)
(helm-mode 1)
(when (not (display-graphic-p))
  (set-face-background 'helm-selection "brightgreen")
  (set-face-background 'helm-selection-line "brightgreen")
  (set-face-background 'helm-source-header "brightblue"))

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
