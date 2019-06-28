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

(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(custom-set-variables '(tool-bar-mode nil)) ;;ツールバー非表示

;; 自動作成するファイルの設定
(setq backup-directory-alist '((".*" . "~/.emacs.d/bak"))) ;;バックアップファイルの場所
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/tmp/" t))) ;;自動保存ファイルの場所
(setq version-control     t) ;;バックアップファイルを作る
(setq kept-new-versions   5) ;;最新の保持数
(setq kept-old-versions   1) ;;最古の保持数
(setq delete-old-versions t) ;;上記外を削除
(setq create-lockfiles nil) ;;ロックファイルは作成しない

(setq-default line-spacing 1) ;;行間調整
(setq-default indent-tabs-mode nil) ;;タブではなくスペース
(setq tab-width 2) ; タブではスペース2
(setq ring-bell-function 'ignore) ;;アラートのベルを消す
(setq scroll-conservatively 35 scroll-margin 0 scroll-step 1) ;; 1行ずつスクロール
(global-hl-line-mode t) ;; カーソルのある行をハイライト\
(electric-pair-mode 1);; 閉じカッコ挿入
(scroll-bar-mode 0) ;;スクロールバー非表示
(blink-cursor-mode 1) ;;カーソル点滅オフ
(add-to-list 'default-frame-alist '(cursor-type . bar))
(global-set-key [f12] 'eval-buffer) ;;F12でeval-buffer

;; ウィンドウ移動
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <down>")  'windmove-down)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <right>") 'windmove-right)


;;---------------------------------------------------------------------------;;
;;  _    _ _______ _____ _      _____ _________     __
;; | |  | |__   __|_   _| |    |_   _|__   __\ \   / /
;; | |  | |  | |    | | | |      | |    | |   \ \_/ / 
;; | |  | |  | |    | | | |      | |    | |    \   /  
;; | |__| |  | |   _| |_| |____ _| |_   | |     | |   
;;  \____/   |_|  |_____|______|_____|  |_|     |_|   
;;---------------------------------------------------------------------------;;

;; exec-path-from-shell
(use-package exec-path-from-shell)
(exec-path-from-shell-initialize)

;; iflipb
(use-package iflipb
  :init
  (setq iflipb-wrap-around t)
  (setq iflipb-ignore-buffers (list "^[*]" "^magit"))
  :bind (("C-<tab>" . iflipb-next-buffer)
         ("C-S-<tab>" . iflipb-previous-buffer)))

;; ivy
(use-package all-the-icons-ivy)
(use-package ivy
  :init
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-height 24)
  (setq ivy-extra-directories nil)
  (setq ivy-re-builders-alist '((t . ivy--regex-plus))))
  :config
  (ivy-mode 1)
  
;; counsel
(use-package counsel
  :config
  (setq swiper-include-line-number-in-search t)
  :bind (("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-s" . swiper)))

;; projectile
(use-package projectile
  :config
  (projectile-global-mode +1)
  (setq projectile-enable-caching t))

;; ounsel-projectile
(use-package counsel-projectile
  :bind (
    ("C-c p f" . counsel-projectile-find-file) ;; Switch project
    :map projectile-mode-map
    ("C-c p a" . projectile-command-map)))

;; company
(use-package company
    :init
    (setq company-selection-wrap-around t)
    :config
    (global-company-mode)
    :bind (("C-M-i" . company-complete)
      :map company-active-map
          ("M-n" . nil)
          ("M-p" . nil)
          ("C-n" . company-select-next)
          ("C-p" . company-select-previous)
          ("C-h" . nil)
          ("<tab>" . company-complete-selection)))

;;yasnippet
(use-package yasnippet)
(use-package yasnippet-snippets
  :config
  (yas-global-mode 1))

;; highlight-indent-guides
(use-package highlight-indent-guides
  :init
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  :config
  (setq highlight-indent-guides-method 'character))

;; js2-mode whith tern ,company tern
(use-package company-tern)
(use-package js2-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js2-mode-hook 'tern-mode)
  (add-to-list 'company-backends 'company-tern))

;;web-mode
(use-package web-mode
  :commands web-mode
  :mode ("\\.hbs\\'"
         "\\.jsx\\'"
         "\\.vue\\'"
         "/\\([Vv]iews\\|[Hh]tml\\|[Tt]emplates\\)/.*\\.php\\'"
         "\\.erb\\'"
         "\\.blade\\.php\\'")
  :config
  (setq sgml-basic-offset 2)
  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-markup-indent-offset 2
        web-mode-attr-indent-offset 2
        web-mode-enable-current-element-highlight t)

  (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
  (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil))

  (setq web-mode-ac-sources-list
        '(("php" . (ac-source-yasnippet))
          ("html" . (ac-source-emmet-html-aliases ac-source-emmet-html-snippets))
          ("css" . (ac-source-css-property ac-source-emmet-css-snippets))))

  (setq web-mode-content-types-alist
        '(("jsx" . "/\\(container\\|component\\)[s]?/.*\\.js[x]?\\'")))

  (subword-mode)
  (emmet-mode)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)

  (add-hook 'web-mode-before-auto-complete-hooks
            '(lambda ()
               (let ((web-mode-cur-language (web-mode-language-at-pos)))
                 (if (string= web-mode-cur-language "php")
                     (yas-activate-extra-mode 'php-mode)
                   (yas-deactivate-extra-mode 'php-mode))
                 (if (string= web-mode-cur-language "javascript")
                     (yas-activate-extra-mode 'js2-mode)
                   (yas-deactivate-extra-mode 'js2-mode))
                 (if (string= web-mode-cur-language "css")
                     (setq emmet-use-css-transform t)
                   (setq emmet-use-css-transform nil)))))

  (add-hook 'html-mode-hook 'web-mode))

;;emmet-mode
(use-package emmet-mode
  :diminish (emmet-mode . "ε")
  :bind* (("C-)" . emmet-next-edit-point)
          ("C-(" . emmet-prev-edit-point))
  :commands (emmet-mode
             emmet-next-edit-point
             emmet-prev-edit-point)
  :init
  (setq emmet-indentation 2)
  (setq emmet-move-cursor-between-quotes t)
  :bind (
  :map emmet-mode-keymap
    ("C-j" . nil)
    ("C-e" . emmet-expand-line)))
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'web-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (with-eval-after-load 'emmet-mode
    (puthash "html:4s" "!!!4s+doc4[lang=ja]" (gethash "aliases" (gethash "html" emmet-snippets)))
    (puthash "html:4t" "!!!4t+doc4[lang=ja]" (gethash "aliases" (gethash "html" emmet-snippets)))
    (puthash "html:5" "!!!+doc[lang=ja]" (gethash "aliases" (gethash "html" emmet-snippets)))
    (puthash "html:xml" "html[xmlns=http://www.w3.org/1999/xhtml]" (gethash "aliases" (gethash "html" emmet-snippets)))
    (puthash "html:xs" "!!!xs+doc4[xmlns=http://www.w3.org/1999/xhtml xml:lang=ja]" (gethash "aliases" (gethash "html" emmet-snippets)))
  )

;; sass-mode
(use-package scss-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.scss$" . scss-mode))
  (defun scss-custom ()
    "scss-mode-hook"
    (and
     (set (make-local-variable 'css-indent-offset) 2)
     (set (make-local-variable 'scss-compile-at-save) nil)))
  (add-hook 'scss-mode-hook '(lambda() (scss-custom))))

;; php-mode
(use-package php-mode)
(use-package company-php
:init
(add-hook 'php-mode-hook
  '(lambda ()
     (company-mode t)
     (ac-php-core-eldoc-setup)
     (make-local-variable 'company-backends)
     (add-to-list 'company-backends 'company-ac-php-backend))))


;;---------------------------------------------------------------------------;;
;; __      _______ ________          _______ 
;; \ \    / /_   _|  ____\ \        / / ____|
;;  \ \  / /  | | | |__   \ \  /\  / / (___  
;;   \ \/ /   | | |  __|   \ \/  \/ / \___ \ 
;;    \  /   _| |_| |____   \  /\  /  ____) |
;;     \/   |_____|______|   \/  \/  |_____/ 
;;---------------------------------------------------------------------------;;

(straight-use-package
 '(icons-in-terminal :type git :host github :repo "seagle0128/icons-in-terminal.el"))

;;dashboard
(use-package dashboard
  :init
  (setq dashboard-startup-banner "~/.emacs.d/images/exite.gif")
  (setq dashboard-banner-logo-title
    (concat "GNU Emacs " emacs-version " kernel "
      (car (split-string (shell-command-to-string "uname -r")))  " x86_64 Mac OS X "
      (car(split-string (shell-command-to-string "sw_vers -productVersion") "-"))
    )
  )
  (setq dashboard-items '((recents  . 5)
                         (projects . 5)))
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)

  :config
  (dashboard-setup-startup-hook))

;;doom-themes
(use-package doom-themes
  :init
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t)
  :config
  (doom-themes-neotree-config)
  (doom-themes-org-config))
  (load-theme 'doom-Iosvkem t)

;;doom-modeline
(use-package doom-modeline
  :init
  (setq doom-modeline-icon t)
  :config
  (doom-modeline-mode 1))

;;nyan-mode
(use-package nyan-mode
  :init
  (nyan-mode)
  (nyan-start-animation))

;;neotree
(use-package neotree
  :init
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-persist-show t)
  (setq neo-smart-open t) 
  (setq neo-smart-open t)
  :bind ("C-q" . neotree-toggle))
