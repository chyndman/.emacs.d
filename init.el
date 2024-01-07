;; Ensure early-init
(when (version< emacs-version "27")
  (load (concat user-emacs-directory "early-init.el")))

;; add-contrib-path
(defun add-contrib-path (name)
  (add-to-list 'load-path (concat user-emacs-directory "contrib/" name)))

;; Core
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(show-paren-mode t)
(column-number-mode t)
(setq-default indent-tabs-mode nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      auto-save-default nil
      visible-bell nil
      ring-bell-function 'ignore
      mouse-wheel-progressive-speed nil)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-unset-key (kbd "C-x C-z"))
(global-unset-key (kbd "C-z"))

;; Theme
(let ((color-count (display-color-cells)))
  (if (<= 256 color-count)
      (progn
        (load-theme 'deeper-blue t)
        (if (= 256 color-count)
            (set-face-attribute 'default nil :background "black")))
    (when (<= 16 color-count)
      (set-face-attribute 'mode-line-inactive nil :foreground "brightblack"))))

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

;; Flymake
(with-eval-after-load "flymake"
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error))

;; Ivy
(add-contrib-path "swiper")
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
(add-contrib-path "markdown-mode")
(autoload 'gfm-mode "markdown-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.md\\'" . gfm-mode))

;; Go
(add-contrib-path "go-mode")
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-to-list 'auto-mode-alist '("go\\.mod\\'" . go-mode))
(add-hook 'go-mode-hook (lambda () (setq-local tab-width 4)))

;; Rust
(add-contrib-path "rust-mode")
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
