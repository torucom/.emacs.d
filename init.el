;; Package repository
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(doom-modeline-buffer-file-name-style (quote truncate-with-project) t)
 '(doom-modeline-icon t t)
 '(doom-modeline-major-mode-icon nil t)
 '(doom-modeline-minor-modes nil t)
 '(package-selected-packages
   (quote
    (doom-themes powerline rainbow-delimiters which-key tabbar helm projectile auto-complete helm-config dashboard markdown-mode mark use-package doom-modeline nyan-mode atom-one-dark-theme all-the-icons-dired all-the-icons neotree emmet-mode monokai-theme)))
 '(save-place-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Set theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/atom-one-dark-theme/")
;; (load-theme 'atom-one-dark t)
(use-package doom-themes
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  :config
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))


;; use-package
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.001)

;; helm
(require 'helm-config)
(helm-mode 1)

;; tabbar
(require 'tabbar)
(tabbar-mode 0)

;; which-key
(require 'which-key)
(which-key-mode)

;; neotree setting
(require 'all-the-icons)
(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-show-hidden-files t)
(setq neo-persist-show t)
(setq neo-smart-open t)
(global-set-key [f9] 'neotree-toggle)

;; doom-modeline
(require 'doom-modeline)
(doom-modeline-mode 1)

;; nyan-mode
(require 'nyan-mode)
(nyan-mode)
(nyan-start-animation)


;; markdown-mode
(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; dashboard
(require 'dashboard)
(setq dashboard-startup-banner "~/.emacs.d/images/exite.gif")
(setq dashboard-banner-logo-title
  (concat "GNU Emacs " emacs-version " kernel "
    (car (split-string (shell-command-to-string "uname -r")))  " x86_64 Mac OS X "
    (car(split-string (shell-command-to-string "sw_vers -productVersion") "-"))
  )
)

(setq dashboard-items '((recents  . 15)
      (bookmarks . 5)))

(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(dashboard-modify-heading-icons '((recents . "file-text")
                                  (bookmarks . "book")))
(dashboard-setup-startup-hook)

;;reload
(global-set-key [f12] 'eval-buffer)
(setq ring-bell-function 'ignore)

;; ---------------  ;;

;; (projectile-mode +1)
;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(add-hook 'foo-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
