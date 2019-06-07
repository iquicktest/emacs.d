;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

(package-initialize)
;; This is only needed once, near the top of the file
;;(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))

;; package manangement

(require 'cl)
(require 'package)
(add-to-list 'package-archives (cons "melpa" "http://melpa.org/packages/") t)
(require 'company)                                   ; load company mode
; No delay in showing suggestions.
(setq company-idle-delay 0)
(setq company-minimum-prefix-length 1)
(setq company-selection-wrap-around t)



; Use tab key to cycle through suggestions.
; ('tng' means 'tab and go')
(company-tng-configure-default)
(add-hook 'dired-mode-hook 'auto-revert-mode)


;; Add package that you want to install before launch your emacs
;; Find Executable Path on OS X

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(when (string= system-type "darwin")       
  (setq dired-use-ls-dired nil))

(setq use-package-always-ensure t)



;; load env
;; load $PATH env variable into emacs
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))


;;;; == better defaults ==
;; Help 
;; jump file
(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-h C-v") 'find-variable)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)

;; Quick Open Configuration
;; open init file
(defun open-init-file()
    (interactive)
    (find-file "~/.emacs.d/init.el"))
(global-set-key (kbd "<f2>") 'open-init-file)
;; Hungry-delete.el - hungry delete minor mode

(require 'hungry-delete)
(global-hungry-delete-mode)

;; Font
(setq default-frame-alist '((font . "Iosevka-16")))



;; Auto Revert
(diminish 'auto-revert-mode)
(global-auto-revert-mode 1)

;; Use y/n over yes/no

(defalias 'yes-or-no-p 'y-or-n-p)

;; Add smartparens config
(use-package smartparens-config
    :ensure smartparens
    :config
    (progn
      (show-smartparens-global-mode t)))

;;(add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
(add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)

;; highlight global line
(global-hl-line-mode t)


;; recentf config

(require 'recentf)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)


;; company mode config

(global-company-mode 1)
;; change company C-n C-p to adjust up and down
(with-eval-after-load 'company
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "C-n") #'company-select-next)
  (define-key company-active-map (kbd "C-p") #'company-select-previous))


;;;; Ivy config
;;; flx
(use-package flx)
;; Counsel
  (use-package counsel
    :diminish ivy-mode
    :bind
    (("C-c C-r" . ivy-resume)
     ("M-x" . counsel-M-x)
     ("C-c i" . counsel-menu)
     ("C-x b" . ivy-switch-buffer)
     ("C-x B" . ivy-switch-buffer-other-window)
     ("C-x k" . kill-buffer)
     ("C-x C-f" . counsel-find-file)
     ("C-x j" . counsel-dired-jump)
     ("C-x l" . counsel-locate)
     ;; git file
     ("C-c j" . counsel-git)
     ;; get recentf 
     ("C-c f" . counsel-recentf)
     ("M-y" . counsel-yank-pop)
     :map help-map
     ("f" . counsel-describe-function)
     ("v" . counsel-describe-variable)
     ("l" . counsel-info-lookup-symbol)
     :map ivy-minibuffer-map
     ("C-d" . ivy-dired)
     ("C-o" . ivy-occur)
     ("<return>" . ivy-alt-done)
     ("M-<return>" . ivy-immediate-done)
     :map read-expression-map
     ("C-r" . counsel-expression-history))
    :init
    (add-hook 'after-init-hook 'ivy-mode)
    :config
    (setq counsel-find-file-at-point t)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (setq ivy-initial-inputs-alist nil)
    (setq ivy-use-selectable-prompt t)
    (setq ivy-re-builders-alist
          '((ivy-switch-buffer . ivy--regex-plus)
            (swiper . ivy--regex-plus)
            (t . ivy--regex-fuzzy))) 
    (ivy-set-actions
     t
     '(("I" insert "insert")))
    (ivy-set-occur 'ivy-switch-buffer 'ivy-switch-buffer-occur))
    (setq counsel-rg-base-command "rg -S --no-heading --line-number --color never %s . | cut -c -200")
;; Swiper

  (use-package swiper
    :bind
    (("C-s" . swiper)
     ("C-r" . swiper)
     ("C-c C-s" . counsel-grep-or-swiper)
     :map swiper-map
     ("M-q" . swiper-query-replace)
     ("C-l". swiper-recenter-top-bottom)
     ("C-." . swiper-mc)
     ("C-'" . swiper-avy))
    :config
    (setq counsel-grep-swiper-limit 20000)
    (setq counsel-grep-base-command
          "rg -i -M 120 --no-heading --line-number --color never '%s' %s"))
;; wgrep
  (use-package wgrep)
;; rg
  (use-package rg
    :bind* (("M-s" . rg)))

;; load theme
(load-theme 'gruvbox-dark-soft 1)


;; swap meta and super key 
;; swap meta and super key and change swith language
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)


;; Replace region when type
;; swap meta and super key and change swith language
(delete-selection-mode 1)


;;;; UI Settings
;; 显示行号
(global-linum-mode -1)

;; turn on nyan mode
(nyan-mode 1)

;; hide tool bar
(tool-bar-mode -1)
;; turn on full screen 
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;; 关闭启动帮助画面
(setq inhibit-splash-screen 1)

;; 关闭文件滑动控件
(scroll-bar-mode -1)
(tooltip-mode -1)

;; 更改显示字体大小 16pt
;; http://stackoverflow.com/questions/294664/how-to-set-the-font-size-in-emacs
(set-face-attribute 'default nil :height 145)

(put 'scroll-left 'disabled nil)

;; turn off backup files
(setq make-backup-files nil)


;; Add hook for elisp
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(define-advice show-paren-function (:around (fn) fix-show-paren-function)
  "Highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
	(t (save-excursion
	     (ignore-errors (backward-up-list))
	     (funcall fn)))))


;; flycheck
(use-package flycheck
  :diminish flycheck-mode
  :init (global-flycheck-mode)
  (add-hook 'after-init-hook #'global-flycheck-mode)
  :config
  (setq flycheck-highlighting-mode 'lines))

;; ace-window
(use-package ace-window
  :config
  (setq aw-swap-invert t))

;; golden ratio
(use-package golden-ratio
  :diminish golden-ratio-mode
  :init
  (add-hook 'after-init-hook 'golden-ratio-mode)
  :config
  (add-to-list 'golden-ratio-extra-commands 'ace-window))

;; rainbow stuff
(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))
(use-package rainbow-identifiers
  :init
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))


;;;; Editing Text
;; expand-region

(use-package expand-region
  :bind (("C-=" . er/expand-region)))
   
;; jump to definition
(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g x" . dumb-jump-go-prefer-external)
         ("M-g z" . dumb-jump-go-prefer-external-other-window))
  :config (setq dumb-jump-selector 'ivy)
          (setq dumb-jump-force-searcher 'rg))

;; Make TAB work
(setq tab-always-indent 'complete)


;; ace-jump-mode
(use-package avy
  :ensure t
  :bind ("C-;" . avy-goto-char)
  :bind ("M-g l" . avy-goto-line))

;; indent 
;; aggressive-indent config
;;(use-package aggressive-indent
;;  :diminish aggressive-indent-mode
;;  :init
;;  (add-hook 'after-init-hook 'aggressive-indent-global-mode))

;; Font size auto adjust
  (global-set-key (kbd "C-M-0")
                  '(lambda () (interactive)
                     (global-text-scale-adjust (- text-scale-mode-amount))
                     (global-text-scale-mode -1)))
  (global-set-key (kbd "M-=")
                  'text-scale-increase)
  (global-set-key (kbd "M--")
                  'text-scale-decrease)

(use-package restclient
  :config
  (eval-after-load "restclient"
    '(add-to-list 'company-backends 'company-restclient)))


;;;; Project Management
;; Projectile
    (global-set-key (kbd "C-x g") 'magit-status) 

    ;; projectile config
    (setq projectile-project-search-path '("~/go/src/" "~/Working/"))

    (use-package projectile
      ;; show only the project name in mode line
      :delight '(:eval (concat " " (projectile-project-name)))
      :init
      (add-hook 'after-init-hook 'projectile-mode)
      :config
      (setq projectile-enable-caching t)
      ;; https://emacs.stackexchange.com/questions/32634/how-can-the-list-of-projects-used-by-projectile-be-manually-updated/3
      (when (require 'magit nil t)
        (mapc #'projectile-add-known-project
              (mapcar #'file-name-as-directory (magit-list-repos)))
        ;; Optionally persist
        (projectile-save-known-projects))
      (use-package counsel-projectile
        :bind (("C-c b" . counsel-projectile-switch-to-buffer)
               ("C-c s" . counsel-projectile-rg)
	       ))
      ;; use git grep to ignore files
      (setq projectile-use-git-grep t)
      ;; use ivy as completion system
      (setq projectile-completion-system 'ivy))

  (eval-after-load "projectile"
    '(setq magit-repository-directories (mapcar #'directory-file-name
                                                (cl-remove-if-not (lambda (project)
                                                                    (file-directory-p (concat project "/.git/")))
                                                                  (projectile-relevant-known-projects)))

           magit-repository-directories-depth 1))

;;;; Modeline
;; Smart mode line
(use-package smart-mode-line
  :init
  (add-hook 'after-init-hook 'sml/setup)
  :config 
  (setq sml/theme 'respectful)
  (setq sml/name-width 24)
  (setq sml/shorten-directory t)
  (setq sml/shorten-modes t)
  (setq sml/mode-width 'full)
  (setq sml/replacer-regexp-list
        '(("^~/\\.emacs\\.d/" ":ED:"))))

(defmacro diminish-minor-mode (filename mode &optional abbrev)
  `(eval-after-load (symbol-name ,filename)
     '(diminish ,mode ,abbrev)))

(defmacro diminish-major-mode (mode-hook abbrev)
  `(add-hook ,mode-hook
             (lambda () (setq mode-name ,abbrev))))

(diminish-minor-mode 'abbrev 'abbrev-mode)
(diminish-minor-mode 'simple 'auto-fill-function)
(diminish-minor-mode 'company 'company-mode)
(diminish-minor-mode 'eldoc 'eldoc-mode)
(diminish-minor-mode 'flycheck 'flycheck-mode)
(diminish-minor-mode 'flyspell 'flyspell-mode)
(diminish-minor-mode 'global-whitespace 'global-whitespace-mode)
(diminish-minor-mode 'projectile 'projectile-mode)
(diminish-minor-mode 'ruby-end 'ruby-end-mode)
(diminish-minor-mode 'subword 'subword-mode)
(diminish-minor-mode 'undo-tree 'undo-tree-mode)
(diminish-minor-mode 'yard-mode 'yard-mode)
(diminish-minor-mode 'yasnippet 'yas-minor-mode)
(diminish-minor-mode 'wrap-region 'wrap-region-mode)

(diminish-minor-mode 'paredit 'paredit-mode " π")

(diminish-major-mode 'emacs-lisp-mode-hook "el")
(diminish-major-mode 'haskell-mode-hook "λ=")
(diminish-major-mode 'lisp-interaction-mode-hook "λ")
(diminish-major-mode 'python-mode-hook "Py")


;; JSON 
(require 'json-mode)

;; Evil Mode
  (evil-mode t)
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
    (evil-define-key 'normal java-mode-map
    (kbd "g d") 'meghanada-jump-declaration)
    (evil-define-key 'normal go-mode-map
    (kbd "g d") 'godef-jump)
    (evil-define-key 'normal java-mode-map
    (kbd "g o") 'meghanada-back-jump)
    (evil-define-key 'normal java-mode-map
      (kbd "C-o") 'dumb-jump-back)
    (evil-define-key 'normal python-mode-map
    (kbd "g d") 'elpy-goto-definition)



  (global-evil-leader-mode)
  (evil-leader/set-leader "<SPC>")
  (evil-leader/set-key
    "ff" 'find-file
    "pd" 'counsel-projectile-find-dir
    "pf" 'counsel-projectile-find-file
    "pb" 'counsel-projectile-switch-to-buffer
    "pp" 'projectile-switch-project
    "ps" 'counsel-projectile-rg
    "wv" 'split-window-right
    "ws" 'split-window-below
    "ww" 'ace-window
    "wd" 'ace-delete-window
    "wm" 'ace-maximize-window
    "bb" 'ivy-switch-buffer
    "l" 'avy-goto-line
    )


;; python env
(use-package elpy
  :ensure t
  :init
  (elpy-enable))

(setq elpy-rpc-python-command "python3")

;; lang, golang setting
(require 'go-mode)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode))
(add-hook 'go-mode-hook (lambda ()
                          (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))



;; for golang autocomplete
(require 'company-go)   


;; java language config
(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode on
            (meghanada-mode t)
            ;; enable telemetry
            (meghanada-telemetry-enable t)
            (flycheck-mode +1)
            (setq c-basic-offset 2)
            ;; use code format
            (add-hook 'before-save-hook 'meghanada-code-beautify-before-save)))
(cond
   ((eq system-type 'windows-nt)
    (setq meghanada-java-path (expand-file-name "bin/java.exe" (getenv "JAVA_HOME")))
    (setq meghanada-maven-path "mvn.cmd"))
   (t
    (setq meghanada-java-path "java")
    (setq meghanada-maven-path "mvn")))


;; docker setting
(use-package docker
  :ensure t
  :bind ("C-c d" . docker))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" default)))
 '(package-selected-packages
   (quote
    (groovy-mode docker meghanada go-autocomplete company-go go-mode magit elpy use-package smartparens rg nyan-mode hungry-delete gruvbox-theme flx exec-path-from-shell diminish counsel company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
