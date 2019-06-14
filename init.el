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
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(package-selected-packages
   (quote
    (golden-ratio company ivy counsel projectile-rails doom-themes powerline rainbow-delimiters which-key tabbar helm projectile auto-complete helm-config dashboard markdown-mode mark use-package doom-modeline nyan-mode atom-one-dark-theme all-the-icons-dired all-the-icons neotree emmet-mode monokai-theme)))
 '(save-place-mode t)
 '(show-paren-mode t)
 '(tool-bar-mode nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar ((t (:background "#6272a4")))))
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
(tabbar-mode)
(setq tabbar-buffer-groups-function nil)
(setq tabbar-separator '(1.0))

(dolist (btn '(tabbar-buffer-home-button
               tabbar-scroll-left-button
               tabbar-scroll-right-button))
  (set btn (cons (cons "" nil)
                 (cons "" nil))))

(set-face-attribute
 'tabbar-default nil
 :background "brightblack"
 :foreground "white"
 )
(set-face-attribute
 'tabbar-selected nil
 :background "#B696F3"
 :foreground "brightwhite"
 :box nil
 )
(set-face-attribute
 'tabbar-modified nil
 :background "#F8F7F3"
 :foreground "brightwhite"
 :box nil
 )
(global-set-key (kbd "<f8>") 'tabbar-forward-tab)
(global-set-key (kbd "<f7>") 'tabbar-backward-tab)

;; which-key
(require 'which-key)
(which-key-mode)

;; neotree setting
(require 'all-the-icons)
(require 'neotree)
;; 隠しファイルをデフォルトで表示
(setq neo-show-hidden-files t)
;; cotrol + q でneotreeを起動
(global-set-key [f9] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

;; (golden-rati
(golden-ratio-mode 1)
(setq golden-ratio-exclude-modes '(calendar-mode))
(setq golden-ratio-exclude-buffer-names '(" *Org tags*" " *Org todo*"))
(setq golden-ratio-exclude-buffer-regexp '("\\*anything" "\\*helm"))
(setq golden-ratio-extra-commands
      '(windmove-left windmove-right windmove-down windmove-up))

;; doom-modeline
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-icon t)

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

;; projectile


;;reload
(global-set-key [f12] 'eval-buffer)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)

;; ---------------  ;;

;; (projectile-mode +1)
;; (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(add-hook 'foo-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; whitespace
(require 'whitespace);;
;; (setq whitespace-line-column 80)
(setq whitespace-style
      '(face
        trailing
        tabs
        spaces
        lines-tail
        newline
        empty
        space-before-tab
        space-after-tab
        space-mark
        tab-mark
        newline-mark))
(setq whitespace-display-mappings
      '((space-mark   ?\u3000 [?□])
        (newline-mark ?\n     [?\xAB ?\n])
        ))
(setq whitespace-space-regexp "\\(\u3000+\\)")
(setq whitespace-global-modes '(not dired-mode tar-mode))
(setq whitespace-action '(auto-cleanup))

(global-whitespace-mode 1)


(global-linum-mode 1)
(setq linum-format "%3d ")

(setq-default tab-width 2 indent-tabs-mode nil)

(setq-default line-spacing 2)

(setq make-backup-files nil)
(setq auto-save-default nil)

;; emmet
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; マークアップ言語全部で使う
(add-hook 'css-mode-hook  'emmet-mode) ;; CSSにも使う
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent はスペース2個
(eval-after-load "emmet-mode"
  '(define-key emmet-mode-keymap (kbd "C-j") nil)) ;; C-j は newline のままにしておく
(keyboard-translate ?\C-e ?\H-e) ;;C-e と Tabの被りを回避
(define-key emmet-mode-keymap (kbd "H-e") 'emmet-expand-line) ;; C-e で展開

;; company
(require 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-transformers '(company-sort-by-backend-importance)) ;; ソート順
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 3) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る
(setq completion-ignore-case t)
(setq company-dabbrev-downcase nil)
(global-set-key (kbd "C-M-i") 'company-complete)
(define-key company-active-map (kbd "C-n") 'company-select-next) ;; C-n, C-pで補完候補を次/前の候補を選択
(define-key company-active-map (kbd "C-p") 'company-select-previous)
(define-key company-search-map (kbd "C-n") 'company-select-next)
(define-key company-search-map (kbd "C-p") 'company-select-previous)
(define-key company-active-map (kbd "C-s") 'company-filter-candidates) ;; C-sで絞り込む
(define-key company-active-map (kbd "C-i") 'company-complete-selection) ;; TABで候補を設定
(define-key company-active-map [tab] 'company-complete-selection) ;; TABで候補を設定
(define-key company-active-map (kbd "C-f") 'company-complete-selection) ;; C-fで候補を設定
(define-key emacs-lisp-mode-map (kbd "C-M-i") 'company-complete) ;; 各種メジャーモードでも C-M-iで company-modeの補完を使う

;; ウィンドウ移動
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)
