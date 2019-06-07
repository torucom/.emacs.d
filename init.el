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

 ;; Packages
 '(package-selected-packages
   (quote(all-the-icons-dired all-the-icons neotree emmet-mode monokai-theme)))

 ;; Options
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
(load-theme 'monokai t)

;; neotree setting
(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-persist-show t)
(setq neo-smart-open t)
(global-set-key [f9] 'neotree-toggle)

;;reload
(global-set-key [f12] 'eval-buffer)
