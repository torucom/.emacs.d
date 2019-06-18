(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(package-selected-packages
   (quote
    (rainbow-delimiters nyan-mode doom-modeline powerline all-the-icons-ivy neotree emmet-mode dashboard yasnippet-snippets yasnippet company use-package doom-themes counsel ivy)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar ((t (:background "#e4007f")))))


;; (setq make-backup-files nil);; バックアップファイルを作らない（*.~）
;; (setq auto-save-default nil) ;; バックアップファイルを作らない（.#*）

;; 自動作成するファイルの設定
(setq backup-directory-alist '((".*" . "~/.emacs.d/bak"))) ;;バックアップファイルを”~/.emacs.d/bak” ディレクトリに作成
(setq version-control     t)
(setq kept-new-versions   5)
(setq kept-old-versions   1)
(setq delete-old-versions t)

(setq auto-save-file-name-transforms   '((".*" "~/.emacs.d/tmp/" t))) ;;復帰のための自動保存ファイルは"~/.emacs.d/tmp/"に作成
(setq create-lockfiles nil) ;;ファイルの前に’.#’が付いたロックファイルは作成しない

;; 行間調整
(setq-default line-spacing 2)

;; 閉じカッコの自動挿入
(electric-pair-mode 1)

;; ウィンドウ移動
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)

;;F12でeval-buffer
(global-set-key [f12] 'eval-buffer)
(setq ring-bell-function 'ignore)
(setq inhibit-startup-message t)

;; --------------------------------------------------------------------- ;;

;; use-package設定
(eval-when-compile (require 'use-package))
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.001)

;; dashboard設定
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

;; doom-theme設定
(use-package doom-themes
  :custom
  (doom-themes-enable-italic nil)
  (doom-themes-enable-bold t)
  :config
  (load-theme 'doom-Iosvkem t)
  ;; (load-theme 'doom-city-lights t)
  ;; (load-theme 'doom-molokai t)
  ;; (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config)
  :custom-face
  (doom-modeline-bar ((t (:background "#e4007f"))))
  )

; (load-theme 'material t)

; doom-modeline設定
;; (use-package doom-modeline
;;   :custom
;;     (doom-modeline-buffer-file-name-style 'truncate-all)
;;     (doom-modeline-icon t)
;;     (doom-modeline-major-mode-icon t)
;;     (doom-modeline-minor-modes t)
;;   :hook
;;     (after-init . doom-modeline-mode)
;;   :config
;;     (line-number-mode 0)
;;     (column-number-mode 0)
;; )
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-icon t)

;; nyan-mode設定
(require 'nyan-mode)
(nyan-mode)
(nyan-start-animation)

;; rainbow-delimiters設定
(add-hook 'foo-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

;; ivy設定
(require 'ivy)
(require 'all-the-icons-ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-height 24)
(setq ivy-extra-directories nil)
(setq ivy-re-builders-alist
      '((t . ivy--regex-plus)))

;; counsel設定
(require 'counsel)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(setq counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))

;; swiper設定
(global-set-key "\C-s" 'swiper)
(setq swiper-include-line-number-in-search t) ;; line-numberでも検索可能

;; company設定
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

;; yasnippet設定
(require 'yasnippet)
(require 'yasnippet-snippets)
(yas-global-mode 1)

;; company yasnippet 連携
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; emmet-mode設定
(require 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; マークアップ言語全部で使う
(add-hook 'css-mode-hook  'emmet-mode) ;; CSSにも使う
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent はスペース2個
(eval-after-load "emmet-mode"
  '(define-key emmet-mode-keymap (kbd "C-j") nil)) ;; C-j は newline のままにしておく
(keyboard-translate ?\C-e ?\H-e) ;;C-e と Tabの被りを回避
(define-key emmet-mode-keymap (kbd "H-e") 'emmet-expand-line) ;; C-e で展開
(with-eval-after-load 'emmet-mode
  (puthash "html:4s" "!!!4s+doc4[lang=ja]" (gethash "aliases" (gethash "html" emmet-snippets)))
  (puthash "html:4t" "!!!4t+doc4[lang=ja]" (gethash "aliases" (gethash "html" emmet-snippets)))
  (puthash "html:5" "!!!+doc[lang=ja]" (gethash "aliases" (gethash "html" emmet-snippets)))
  (puthash "html:xml" "html[xmlns=http://www.w3.org/1999/xhtml]" (gethash "aliases" (gethash "html" emmet-snippets)))
  (puthash "html:xs" "!!!xs+doc4[xmlns=http://www.w3.org/1999/xhtml xml:lang=ja]" (gethash "aliases" (gethash "html" emmet-snippets)))
  )

;; neotree設定
(require 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-persist-show t) ;; delete-other-window で neotree ウィンドウを消さない
(setq neo-smart-open t) ;; neotree ウィンドウを表示する毎に current file のあるディレクトリを表示する
(setq neo-smart-open t)
(global-set-key "\C-q" 'neotree-toggle)
