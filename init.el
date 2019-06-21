(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(custom-set-variables '(tool-bar-mode nil)) ;;ツールバー非表示

;; 自動作成するファイルの設定
(setq backup-directory-alist '((".*" . "~/.emacs.d/bak"))) ;;バックアップファイルの場所
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/tmp/" t))) ;;自動保存ファイルの場所
(setq version-control     t) ;;バックアップファイルを作る
(setq kept-new-versions   5) ;;最新の保持数
(setq kept-old-versions   1) ;;最古の保持数
(setq delete-old-versions t) ;;上記外を削除
(setq create-lockfiles nil) ;;ロックファイルは作成しない

(setq-default line-spacing 2) ;;行間調整
(setq-default indent-tabs-mode nil) ;;タブではなくスペース
(setq tab-width 2) ; タブではスペース2
(setq ring-bell-function 'ignore) ;;アラートのベルを消す
(setq scroll-conservatively 35 scroll-margin 0 scroll-step 1) ;; 1行ずつスクロール
(global-hl-line-mode t) ;; カーソルのある行をハイライト\
(electric-pair-mode 1);; 閉じカッコ挿入
(scroll-bar-mode 0) ;;スクロールバー非表示
(blink-cursor-mode 0) ;;カーソル点滅オフ
(global-set-key [f12] 'eval-buffer) ;;F12でeval-buffer

;; ウィンドウ移動
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)

;;---------------------------------------------------------------------------;;

;;dashboard
(straight-use-package 'dashboard)
(dashboard-setup-startup-hook)

(setq dashboard-startup-banner "~/.emacs.d/images/exite.gif")
(setq dashboard-banner-logo-title
  (concat "GNU Emacs " emacs-version " kernel "
    (car (split-string (shell-command-to-string "uname -r")))  " x86_64 Mac OS X "
    (car(split-string (shell-command-to-string "sw_vers -productVersion") "-"))
  )
)
(setq dashboard-items '((recents  . 15)))
(setq dashboard-set-heading-icons t)
(setq dashboard-set-file-icons t)
(dashboard-setup-startup-hook)

;; iflipb
(straight-use-package 'iflipb)
(setq iflipb-wrap-around t)
(setq iflipb-ignore-buffers (list "^[*]" "^magit"))
(global-set-key (kbd "C-<tab>") 'iflipb-next-buffer)
(global-set-key (kbd "C-S-<tab>") 'iflipb-previous-buffer)

;;doom-themes
(straight-use-package 'doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-Iosvkem t)
(doom-themes-neotree-config)
(doom-themes-org-config)

;;doom-modeline
(straight-use-package 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-icon t)

;;nyan-mode
(straight-use-package 'nyan-mode)
(nyan-mode)
(nyan-start-animation)

;;neotree
(straight-use-package 'neotree)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))
(setq neo-persist-show t)
(setq neo-smart-open t) 
(setq neo-smart-open t)
(global-set-key "\C-q" 'neotree-toggle)

;; ivy
(straight-use-package 'ivy)
(straight-use-package 'all-the-icons-ivy)
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
(setq ivy-height 24)
(setq ivy-extra-directories nil)
(setq ivy-re-builders-alist '((t . ivy--regex-plus)))

;; counsel設定
(straight-use-package 'counsel)
(global-set-key (kbd "M-x") 'counsel-M-x)
(global-set-key (kbd "C-x C-f") 'counsel-find-file)
(setq counsel-find-file-ignore-regexp (regexp-opt '("./" "../")))
(global-set-key "\C-s" 'swiper)
(setq swiper-include-line-number-in-search t)

;; company設定
(straight-use-package 'company)
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

;;yasnippet
(straight-use-package 'yasnippet)
(straight-use-package 'yasnippet-snippets)
(yas-global-mode 1)

;; company with yasnippet
(defvar company-mode/enable-yas t "Enable yasnippet for all backends.")
(defun company-mode/backend-with-yas (backend)
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))
(setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))

;; highlight-indent-guides
(straight-use-package 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'character)

;; web-mode
(straight-use-package 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(defun web-mode-hook ()
  (setq web-mode-html-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq indent-tabs-mode nil)
  (setq tab-width 2))
(add-hook 'web-mode-hook 'web-mode-hook)

;;emmet-mode
(straight-use-package 'emmet-mode)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; マークアップ言語全部で使う
(add-hook 'css-mode-hook  'emmet-mode) ;; CSSにも使う
(add-hook 'emmet-mode-hook (lambda () (setq emmet-indentation 2))) ;; indent はスペース2個
(with-eval-after-load 'emmet-mode
  (puthash "html:4s" "!!!4s+doc4[lang=ja]" (gethash "aliases" (gethash "html" emmet-snippets)))
  (puthash "html:4t" "!!!4t+doc4[lang=ja]" (gethash "aliases" (gethash "html" emmet-snippets)))
  (puthash "html:5" "!!!+doc[lang=ja]" (gethash "aliases" (gethash "html" emmet-snippets)))
  (puthash "html:xml" "html[xmlns=http://www.w3.org/1999/xhtml]" (gethash "aliases" (gethash "html" emmet-snippets)))
  (puthash "html:xs" "!!!xs+doc4[xmlns=http://www.w3.org/1999/xhtml xml:lang=ja]" (gethash "aliases" (gethash "html" emmet-snippets)))
)

;; sass-mode
(straight-use-package 'scss-mode)
(add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
(defun scss-custom ()
  "scss-mode-hook"
  (and
   (set (make-local-variable 'css-indent-offset) 2)
   (set (make-local-variable 'scss-compile-at-save) nil)))
(add-hook 'scss-mode-hook '(lambda() (scss-custom)))

;; php-mode
(straight-use-package 'php-mode)
(straight-use-package 'company-php)
(add-hook 'php-mode-hook
          '(lambda ()
             (company-mode t)
             (ac-php-core-eldoc-setup)
             (make-local-variable 'company-backends)
             (add-to-list 'company-backends 'company-ac-php-backend)))
