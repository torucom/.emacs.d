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
    (markdown-mode mark use-package doom-modeline nyan-mode atom-one-dark-theme all-the-icons-dired all-the-icons neotree emmet-mode monokai-theme)))
 '(save-place-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;use-package
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.001)

;; helm
(require 'helm-config)
(helm-mode 1)

;; powerline
(require 'powerline)
(powerline-center-theme)

;; tabbar
(require 'tabbar)
(tabbar-mode 0)

;; which-key
(add-to-list 'load-path "~/.emacs.d/elpa/which-key-20190529.114/which-key.el")
(require 'which-key)
(which-key-mode)

;; Set theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/atom-one-dark-theme/")
(load-theme 'atom-one-dark t)

;; neotree setting
(require 'all-the-icons)
(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-show-hidden-files t)
(setq neo-persist-show t)
(setq neo-smart-open t)
(global-set-key [f9] 'neotree-toggle)

;;doom-modeline
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

;;reload
(global-set-key [f12] 'eval-buffer)
