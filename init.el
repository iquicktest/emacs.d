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


(when (eq system-type 'darwin) (ns-auto-titlebar-mode))



; Use tab key to cycle through suggestions.
; ('tng' means 'tab and go')
(company-tng-configure-default)
(add-hook 'dired-mode-hook 'auto-revert-mode)


;;;;Org mode configuration
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-agenda-files (directory-files-recursively "~/org" "\\.org$"))
(setq org-log-done t)
;; The above is the default in recent emacsen
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

(use-package direnv
 :config
 (direnv-mode))

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

(define-advice select-window (:after (window &optional no-record) golden-ratio-resize-window)
    (golden-ratio)
    nil)


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


(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

;; If you want to pull in the Evil compatibility package.
(use-package kubernetes-evil
  :ensure t
  :after kubernetes)

;;;; Project Management
;; Projectile
    (global-set-key (kbd "C-x g") 'magit-status) 
    (global-set-key (kbd "C-c k") 'kubernetes-overview) 

    (require 'evil-magit)

    ;; projectile config
    (setq projectile-project-search-path '("~/go/src/" "~/Working/" "~/org/" "~/Learning/" "~/spike/spikes/" "~/working/py3/"))

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
;;(use-package smart-mode-line
;;  :init
;;  (add-hook 'after-init-hook 'sml/setup)
;;  :config 
;;  (setq sml/theme 'respectful)
;;  (setq sml/name-width 24)
;;  (setq sml/shorten-directory t)
;;  (setq sml/shorten-modes t)
;;  (setq sml/mode-width 'full)
;;  (setq sml/replacer-regexp-list
;;        '(("^~/\\.emacs\\.d/" ":ED:"))))
;;
;;(defmacro diminish-minor-mode (filename mode &optional abbrev)
;;  `(eval-after-load (symbol-name ,filename)
;;     '(diminish ,mode ,abbrev)))
;;
;;(defmacro diminish-major-mode (mode-hook abbrev)
;;  `(add-hook ,mode-hook
;;             (lambda () (setq mode-name ,abbrev))))
;;
;;(diminish-minor-mode 'abbrev 'abbrev-mode)
;;(diminish-minor-mode 'simple 'auto-fill-function)
;;(diminish-minor-mode 'company 'company-mode)
;;(diminish-minor-mode 'eldoc 'eldoc-mode)
;;(diminish-minor-mode 'flycheck 'flycheck-mode)
;;(diminish-minor-mode 'flyspell 'flyspell-mode)
;;(diminish-minor-mode 'global-whitespace 'global-whitespace-mode)
;;(diminish-minor-mode 'projectile 'projectile-mode)
;;(diminish-minor-mode 'ruby-end 'ruby-end-mode)
;;(diminish-minor-mode 'subword 'subword-mode)
;;(diminish-minor-mode 'undo-tree 'undo-tree-mode)
;;(diminish-minor-mode 'yard-mode 'yard-mode)
;;(diminish-minor-mode 'yasnippet 'yas-minor-mode)
;;(diminish-minor-mode 'wrap-region 'wrap-region-mode)
;;
;;(diminish-minor-mode 'paredit 'paredit-mode " π")
;;
;;(diminish-major-mode 'emacs-lisp-mode-hook "el")
;;(diminish-major-mode 'haskell-mode-hook "λ=")
;;(diminish-major-mode 'lisp-interaction-mode-hook "λ")
;;(diminish-major-mode 'python-mode-hook "Py")


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
    "po" 'org-projectile-project-todo-completing-read
    "pp" 'projectile-switch-project
    "ps" 'counsel-projectile-rg
    "wv" 'split-window-right
    "ws" 'split-window-below
    "ww" 'ace-window
    "wd" 'ace-delete-window
    "wm" 'ace-maximize-window
    "bb" 'ivy-switch-buffer
    "l" 'avy-goto-line
    ">" 'org-narrow-to-subtree
    "<" 'widen
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
(setq evil-emacs-state-modes (append evil-emacs-state-modes '(docker-container-mode docker-image-mode docker-volume-mode docker-network-mode docker-machine-mode)))


(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))

(require 'org-projectile)
(org-projectile-per-project)
(setq org-projectile-per-project-filepath "project_todo.org")
(setq org-agenda-files (append org-agenda-files (org-projectile-todo-files)))
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c n p") 'org-projectile-project-todo-completing-read)


;; leetcode setting
(setq leetcode-prefer-language "python3")

;; neotree
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)



;; doome theme################3
(use-package doom-themes
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

 ;;doom-bar
 ;;How tall the mode-line should be. It's only respected in GUI.
 ;;If the actual char height is larger, it respects the actual height.

;; load theme
(load-theme 'doom-one 1)

(setq doom-modeline-height 25)

;;How wide the mode-line bar should be. It's only respected in GUI.
(setq doom-modeline-bar-width 3)

;; How to detect the project root.
;; The default priority of detection is `ffip' > `projectile' > `project'.
;; nil means to use `default-directory'.
;; The project management packages have some issues on detecting project root.
;; e.g. `projectile' doesn't handle symlink folders well, while `project' is unable
;; to hanle sub-projects.
;; You can specify one if you encounter the issue.
(setq doom-modeline-project-detection 'projectile)


(add-to-list 'load-path "~/.emacs.d/multi-magit/")
(load "multi-magit")
(magit-add-section-hook 'magit-status-sections-hook
                        'multi-magit-insert-repos-overview
                         nil t)


;; Determines the style used by `doom-modeline-buffer-file-name'.
;;
;; Given ~/Projects/FOSS/emacs/lisp/comint.el
;;   truncate-upto-project => ~/P/F/emacs/lisp/comint.el
;;   truncate-from-project => ~/Projects/FOSS/emacs/l/comint.el
;;   truncate-with-project => emacs/l/comint.el
;;   truncate-except-project => ~/P/F/emacs/l/comint.el
;;   truncate-upto-root => ~/P/F/e/lisp/comint.el
;;   truncate-all => ~/P/F/e/l/comint.el
;;   relative-from-project => emacs/lisp/comint.el
;;   relative-to-project => lisp/comint.el
;;   file-name => comint.el
;;   buffer-name => comint.el<2> (uniquify buffer name)
;;
;; If you are experiencing the laggy issue, especially while editing remote files
;; with tramp, please try `file-name' style.
;; Please refer to https://github.com/bbatsov/projectile/issues/657.
(setq doom-modeline-buffer-file-name-style 'truncate-upto-project)

;; Whether display icons in mode-line. Respects `all-the-icons-color-icons'.
;; While using the server mode in GUI, should set the value explicitly.
(setq doom-modeline-icon (display-graphic-p))

;; Whether display the icon for `major-mode'. Respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Whether display the colorful icon for `major-mode'.
;; Respects `doom-modeline-major-mode-icon'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

;; Whether display the modification icon for the buffer.
;; Respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)

;; Whether to use unicode as a fallback (instead of ASCII) when not using icons.
(setq doom-modeline-unicode-fallback nil)

;; Whether display the minor modes in mode-line.
(setq doom-modeline-minor-modes nil)

;; If non-nil, a word count will be added to the selection-info modeline segment.
(setq doom-modeline-enable-word-count nil)

;; Major modes in which to display word count continuously.
;; Also applies to any derived modes. Respects `doom-modeline-enable-word-count'.
;; If it brings the sluggish issue, disable `doom-modeline-enable-word-count' or
;;remove the modes from `doom-modeline-continuous-word-count-modes'.
(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

;; Whether display the buffer encoding.
(setq doom-modeline-buffer-encoding t)

;; Whether display the indentation information.
(setq doom-modeline-indent-info nil)

;; If non-nil, only display one number for checker information if applicable.
(setq doom-modeline-checker-simple-format t)

;; The maximum number displayed for notifications.
(setq doom-modeline-number-limit 99)

;; The maximum displayed length of the branch name of version control.
(setq doom-modeline-vcs-max-length 12)

;; Whether display the perspective name. Non-nil to display in mode-line.
(setq doom-modeline-persp-name t)

;; If non nil the default perspective name is displayed in the mode-line.
(setq doom-modeline-display-default-persp-name nil)

;; Whether display the `lsp' state. Non-nil to display in mode-line.
(setq doom-modeline-lsp t)

;; Whether display the modal state icon.
;; Including `evil', `overwrite', `god', `ryo' and `xah-fly-keys', etc.
(setq doom-modeline-modal-icon t)

;; Whether display the mu4e notifications. It requires `mu4e-alert' package.
(setq doom-modeline-mu4e nil)

;; Whether display the gnus notifications.
(setq doom-modeline-gnus t)

;;Wheter gnus should automatically be updated and how often (set to nil to disable)
(setq doom-modeline-gnus-timer 2)

;; Whether display the IRC notifications. It requires `circe' or `erc' package.
(setq doom-modeline-irc t)

;; Function to stylize the irc buffer names.
(setq doom-modeline-irc-stylize 'identity)

;; Whether display the environment version.
(setq doom-modeline-env-version t)
;; Or for individual languages
(setq doom-modeline-env-enable-python t)
(setq doom-modeline-env-enable-ruby t)
(setq doom-modeline-env-enable-go t)

;; Change the executables to use for the language version string
(setq doom-modeline-env-python-executable "python") ; or `python-shell-interpreter'
(setq doom-modeline-env-go-executable "go")

;; What to dispaly as the version while a new one is being loaded
(setq doom-modeline-env-load-string "...")

;; Hooks that run before/after the modeline version string is updated
(setq doom-modeline-before-update-env-hook nil)
(setq doom-modeline-after-update-env-hook nil)

(setq org-hide-emphasis-markers t)
(setq magit-repository-directories
      '(("~/Working/" . 1)))

(use-package doom-modeline
      :ensure t
      :hook (after-init . doom-modeline-mode))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files
   (quote
    ("/Users/sgjerry.zhao/org/goals/goals_2019.org" "/Users/sgjerry.zhao/org/goals/project_todo.org" "/Users/sgjerry.zhao/org/snippets/infra_notes.org" "/Users/sgjerry.zhao/org/projects.org")))
 '(package-selected-packages
   (quote
    (evil-magit spinner leetcode ns-auto-titlebar restclient-test company-restclient monokai-pro-theme x509-mode projectile-direnv direnv org-projectile k8s-mode kubernetes-evil kubernetes org-bullets groovy-mode docker meghanada go-autocomplete company-go go-mode magit elpy use-package smartparens rg nyan-mode hungry-delete gruvbox-theme flx exec-path-from-shell diminish counsel company)))
 '(pdf-view-midnight-colors (quote ("#fdf4c1" . "#32302f")))
 '(x509-openssl-cmd "/usr/bin/openssl"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
