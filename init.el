;; Ensure early-init
(when (version< emacs-version "27")
  (load (concat user-emacs-directory "early-init.el")))

;; Core
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)
(show-paren-mode t)
(column-number-mode t)
(recentf-mode t)
(setq-default indent-tabs-mode nil)
(setq custom-file (expand-file-name "custom.el" user-emacs-directory)
      backup-directory-alist `(("." . ,(concat user-emacs-directory "backups")))
      auto-save-default nil
      visible-bell nil
      ring-bell-function 'ignore
      confirm-kill-emacs #'y-or-n-p
      mouse-wheel-progressive-speed nil)

;; Keymap
(global-unset-key (kbd "C-z"))
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "C-x C-r") 'recentf-open)

;; Theme
(load-theme 'deeper-blue t)
(set-face-attribute 'font-lock-comment-face nil :foreground "#808080")
(set-face-attribute 'font-lock-comment-delimiter-face nil :foreground "#808080")

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

;; take-pkg
(defun take-pkg (pkg file)
  (unless (package-installed-p pkg)
    (package-install-file (concat user-emacs-directory "contrib/" file))))

;; Ivy
(take-pkg 'ivy "ivy-0.14.2.tar")
(take-pkg 'swiper "swiper-0.14.2.tar")
(take-pkg 'counsel "counsel-0.14.2.tar")
(ivy-mode 1)
(counsel-mode 1)
(global-set-key (kbd "C-s") 'swiper-isearch)
(global-set-key (kbd "C-r") 'swiper-backward)

;; C/C++
(setq c-default-style "stroustrup")

;; Markdown
(take-pkg 'markdown-mode "markdown-mode-2.6.tar")

;; Go
(take-pkg 'go-mode "go-mode-1.6.0.tar")
(add-hook 'go-mode-hook (lambda () (setq-local tab-width 4)))

;; Rust
(take-pkg 'rust-mode "rust-mode-1.0.6.tar")
