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

(eval-when-compile
  ;; Following line is not needed if use-package.el is in ~/.emacs.d
  (require 'use-package))

(setq use-package-always-ensure t)

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

(bind-keys
 :map smartparens-mode-map
 ("C-M-a" . sp-beginning-of-sexp)
 ("C-M-e" . sp-end-of-sexp)

 ("C-<down>" . sp-down-sexp)
 ("C-<up>"   . sp-up-sexp)
 ("M-<down>" . sp-backward-down-sexp)
 ("M-<up>"   . sp-backward-up-sexp)

 ("C-M-f" . sp-forward-sexp)
 ("C-M-b" . sp-backward-sexp)

 ("C-M-n" . sp-next-sexp)
 ("C-M-p" . sp-previous-sexp)

 ("C-S-f" . sp-forward-symbol)
 ("C-S-b" . sp-backward-symbol)

 ("C-<right>" . sp-forward-slurp-sexp)
 ("M-<right>" . sp-forward-barf-sexp)
 ("C-<left>"  . sp-backward-slurp-sexp)
 ("M-<left>"  . sp-backward-barf-sexp)

 ("C-M-t" . sp-transpose-sexp)
 ("C-M-k" . sp-kill-sexp)
 ("C-k"   . sp-kill-hybrid-sexp)
 ("M-k"   . sp-backward-kill-sexp)
 ("C-M-w" . sp-copy-sexp)
 ("C-M-d" . delete-sexp)

 ("M-<backspace>" . backward-kill-word)
 ("C-<backspace>" . sp-backward-kill-word)
 ([remap sp-backward-kill-word] . backward-kill-word)

 ("M-[" . sp-backward-unwrap-sexp)
 ("M-]" . sp-unwrap-sexp)

 ("C-x C-t" . sp-transpose-hybrid-sexp)

 ("C-c ("  . wrap-with-parens)
 ("C-c ["  . wrap-with-brackets)
 ("C-c {"  . wrap-with-braces)
 ("C-c '"  . wrap-with-single-quotes)
 ("C-c \"" . wrap-with-double-quotes)
 ("C-c _"  . wrap-with-underscores)
 ("C-c `"  . wrap-with-back-quotes))


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
(load-theme 'hc-zenburn 1)


;; Replace region when type
;; swap meta and super key and change swith language
(delete-selection-mode 1)


;;;; UI Settings
;; 
(global-linum-mode -1)

;; hide tool bar
(menu-bar-mode -1)
(tool-bar-mode -1)
;; turn on full screen 
(setq initial-frame-alist (quote ((fullscreen . maximized))))

;;
(setq inhibit-splash-screen 1)

;; 关闭文件滑动控件
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


;; Scroll other window


  (defun scroll-other-window-up ()
    "Scroll the other window one line up."
    (interactive)
    (scroll-other-window -1)
  )
  (defun scroll-other-window-down ()
    "Scroll the other window one line down."
    (interactive)
    (scroll-other-window 1)
  )
  (global-set-key (kbd "C-,") 'scroll-other-window-up)
  (global-set-key (kbd "C-.") 'scroll-other-window-down)


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
;; Git
;; diff-hl config
(use-package diff-hl
  :config
  (add-hook 'prog-mode-hook 'turn-on-diff-hl-mode)
  (add-hook 'vc-dir-mode-hook 'turn-on-diff-hl-mode))

;;(use-package smerge-mode
;;  :bind (("C-c m" . jethro/hydra-smerge/body))
;;  :init
;;  (progn
;;    (defun jethro/enable-smerge-maybe ()
;;      "Auto-enable `smerge-mode' when merge conflict is detected."
;;      (save-excursion
;;        (goto-char (point-min))
;;        (when (re-search-forward "^<<<<<<< " nil :noerror)
;;          (smerge-mode 1))))
;;    (add-hook 'find-file-hook #'jethro/enable-smerge-maybe :append))
;;  :config 
;;  (defalias 'smerge-keep-upper 'smerge-keep-mine)
;;  (defalias 'smerge-keep-lower 'smerge-keep-other)
;;  (defalias 'smerge-diff-base-upper 'smerge-diff-base-mine)
;;  (defalias 'smerge-diff-upper-lower 'smerge-diff-mine-other)
;;  (defalias 'smerge-diff-base-lower 'smerge-diff-base-other)
;;  (defhydra jethro/hydra-smerge (:color pink
;;                                        :hint nil
;;                                        :pre (smerge-mode 1)
;;                                        ;; Disable `smerge-mode' when quitting hydra if
;;                                        ;; no merge conflicts remain.
;;                                        :post (smerge-auto-leave))
;;    "
;;   ^Move^       ^Keep^               ^Diff^                 ^Other^
;;   ^^-----------^^-------------------^^---------------------^^-------
;;   _n_ext       _b_ase               _<_: upper/base        _C_ombine
;;   _p_rev       _u_pper              _=_: upper/lower       _r_esolve
;;   ^^           _l_ower              _>_: base/lower        _k_ill current
;;   ^^           _a_ll                _R_efine
;;   ^^           _RET_: current       _E_diff
;;   "
;;    ("n" smerge-next)
;;    ("p" smerge-prev)
;;    ("b" smerge-keep-base)
;;    ("u" smerge-keep-upper)
;;    ("l" smerge-keep-lower)
;;    ("a" smerge-keep-all)
;;    ("RET" smerge-keep-current)
;;    ("\C-m" smerge-keep-current)
;;    ("<" smerge-diff-base-upper)
;;    ("=" smerge-diff-upper-lower)
;;    (">" smerge-diff-base-lower)
;;    ("R" smerge-refine)
;;    ("E" smerge-ediff)
;;    ("C" smerge-combine-with-next)
;;    ("r" smerge-resolve)
;;    ("k" smerge-kill-current)
;;    ("q" nil "cancel" :color blue)))

;;;; Tools

;; Rest Client

(use-package restclient
  :config
  (eval-after-load "restclient"
    '(add-to-list 'company-backends 'company-restclient)))



;; RSS
(global-set-key (kbd "C-x w") 'elfeed)
;; Somewhere in your .emacs file


;;;; Project Management
  
;; Projectile
    (global-set-key (kbd "C-x g") 'magit-status) 

    ;; projectile config
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
               ("C-c s" . counsel-projectile-rg)))
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


;;;; Languages

;; Language Servers


(use-package lsp-mode
  :config
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook 'lsp-enable-imenu))

(use-package lsp-ui
  :after lsp-mode
  :init
  (add-hook 'lsp-mode-hook #'lsp-ui-mode)
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references))

(use-package company-lsp
  :after company lsp-mode
  :config
  (add-to-list 'company-backends 'company-lsp))

;; Go

(use-package go-mode
  :mode ("\\.go\\'" . go-mode)
  :config
  (add-hook 'go-mode-hook 'compilation-auto-quit-window)
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode)))
  (add-hook 'go-mode-hook (lambda ()
                            (add-hook 'before-save-hook 'gofmt-before-save)
                            (local-set-key (kbd "M-.") 'godef-jump)))
  (add-hook 'go-mode-hook
            (lambda ()
              (unless (file-exists-p "Makefile")
                (set (make-local-variable 'compile-command)
                     (let ((file (file-name-nondirectory buffer-file-name)))
                       (format "go build %s"
                               file))))))
  (use-package go-dlv
    :config (require 'go-dlv))
  (use-package golint
    :config
    (add-to-list 'load-path (concat (getenv "GOPATH")  "/src/github.com/golang/lint/misc/emacs"))
    (require 'golint))
  (use-package gorepl-mode
    :config (add-hook 'go-mode-hook #'gorepl-mode))
  (use-package company-go
    :config (add-hook 'go-mode-hook (lambda ()
                                      (set (make-local-variable 'company-backends) '(company-go))
                                      (company-mode))))
)

(define-derived-mode protobuf-mode c-mode
  "Protocol Buffer" "Major mode for editing Google Protocol Buffer files."
  (setq fill-column 80
          tab-width 4))

(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))
(provide 'protobuf)


;; Python

    (elpy-enable)
    (setq elpy-rpc-python-command "python3")
    (eval-after-load "python-mode"
      (lambda ()
        (setq python-remove-cwd-from-path t)))

    ;; ipython setting
    (setq python-shell-interpreter "ipython"
	python-shell-interpreter-args "-i --simple-prompt")

    (use-package pytest
      :bind (:map python-mode-map
                  ("C-c a" . pytest-all)
                  ("C-c m" . pytest-module)
                  ("C-c ." . pytest-one)
                  ("C-c d" . pytest-directory)
                  ("C-c p a" . pytest-pdb-all)
                  ("C-c p m" . pytest-pdb-module)
                  ("C-c p ." . pytest-pdb-one)))

    (setq ein:jupyter-default-notebook-directory "/Users/jerryzhao/Envs/notebook/")
    (setq ein:jupyter-default-server-command "/usr/local/bin/jupyter")
    (setq ein:use-auto-complete t)

   ;; (use-package highlight-indent-guides
   ;;   :init
   ;;   (add-hook 'python-mode-hook 'highlight-indent-guides-mode)
   ;;   :config
   ;;   (setq highlight-indent-guides-method 'character))

    (use-package isend-mode
      :bind
      (:map isend-mode-map
            ("C-M-e" . isend-send-defun))
      :init
      (add-hook 'isend-mode-hook 'isend-default-python-setup))

;; Java
;; Google C Style
(use-package google-c-style
  :commands
  (google-set-c-style))
;; Meghanada

;; java configurations
(require 'meghanada)
(add-hook 'java-mode-hook
          (lambda ()
            ;; meghanada-mode oon
            (google-set-c-style)
            (google-make-newline-indent)
            (meghanada-mode t)
            (smartparens-mode t)
            (rainbow-delimiters-mode t)
            (flycheck-mode t) 
            (highlight-symbol-mode t)
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

;;(use-package lsp-java
;;  :after lsp-mode
;;  :init
;;  (add-hook 'java-mode-hook 'lsp-java-enable))


;;;; Markdown
(use-package markdown-mode
  :mode ("\\.md\\'" . markdown-mode)
  :commands (markdown-mode gfm-mode)
  :init
  (setq markdown-fontify-code-blocks-natively t)
  :config 
  (setq markdown-command "multimarkdown --snippet --smart --notes"
        markdown-enable-wiki-links t
        markdown-indent-on-enter 'indent-and-new-item
        markdown-asymmetric-header t
        markdown-live-preview-delete-export 'delete-on-destroy))

;; YAML
(require 'yaml-mode)
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))


;; JSON 

(require 'json-mode)

;; Org Mode
;; org configuation
(require 'org)

;; Evil Mode
  (evil-mode t)
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)
    (evil-define-key 'normal java-mode-map
    (kbd "g d") 'meghanada-jump-declaration)
    (evil-define-key 'normal java-mode-map
    (kbd "g o") 'meghanada-back-jump)
    (evil-define-key 'normal java-mode-map
      (kbd "C-o") 'dumb-jump-back)

  (global-evil-leader-mode)
  (evil-leader/set-key
    "ff" 'find-file
    "pd" 'counsel-projectile-find-dir
    "pf" 'counsel-projectile-find-file
    "pb" 'counsel-projectile-switch-to-buffer
    "ps" 'counsel-projectile-rg
    "wv" 'split-window-right
    "ws" 'split-window-below
    "ww" 'ace-window
    "wd" 'ace-delete-window
    "wm" 'ace-maximize-window
    "bb" 'switch-to-buffer
    )



;; Auto YASnippets

(global-set-key (kbd "C-c r") #'aya-create)
(global-set-key (kbd "C-c e") #'aya-expand)

;; NEO Tree
(require 'dired-x)
(global-set-key [f8] 'mhj/toggle-project-explorer)

(use-package dired-subtree
  :demand
  :bind
  (:map dired-mode-map
    ("<enter>" . mhj/dwim-toggle-or-open)
    ("<return>" . mhj/dwim-toggle-or-open)
    ("<tab>" . mhj/dwim-toggle-or-open)
    ("<down-mouse-1>" . mhj/mouse-dwim-to-toggle-or-open))
  :config
  (progn
    ;; Function to customize the line prefixes (I simply indent the lines a bit)
    (setq dired-subtree-line-prefix (lambda (depth) (make-string (* 2 depth) ?\s)))
    (setq dired-subtree-use-backgrounds nil)))

(defun mhj/dwim-toggle-or-open ()
  "Toggle subtree or open the file."
  (interactive)
  (if (file-directory-p (dired-get-file-for-visit))
      (progn
    (dired-subtree-toggle)
    (revert-buffer))
    (dired-find-file)))

(defun mhj/mouse-dwim-to-toggle-or-open (event)
  "Toggle subtree or the open file on mouse-click in dired."
  (interactive "e")
  (let* ((window (posn-window (event-end event)))
     (buffer (window-buffer window))
     (pos (posn-point (event-end event))))
    (progn
      (with-current-buffer buffer
    (goto-char pos)
    (mhj/dwim-toggle-or-open)))))

(use-package dired
  :ensure nil
  :config
  (progn
    (setq insert-directory-program "/usr/local/opt/coreutils/libexec/gnubin/ls")
    (setq dired-listing-switches "-lXGh --group-directories-first")
    (add-hook 'dired-mode-hook 'dired-omit-mode)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)))

(defun mhj/toggle-project-explorer ()
  "Toggle the project explorer window."
  (interactive)
  (let* ((buffer (dired-noselect (projectile-project-root)))
    (window (get-buffer-window buffer)))
    (if window
    (mhj/hide-project-explorer)
      (mhj/show-project-explorer))))

(defun mhj/show-project-explorer ()
  "Project dired buffer on the side of the frame.
Shows the projectile root folder using dired on the left side of
the frame and makes it a dedicated window for that buffer."
  (let ((buffer (dired-noselect (projectile-project-root))))
    (progn
      (display-buffer-in-side-window buffer '((side . left) (window-width . 0.2)))
      (set-window-dedicated-p (get-buffer-window buffer) t))))

(defun mhj/hide-project-explorer ()
  "Hide the project-explorer window."
  (let ((buffer (dired-noselect (projectile-project-root))))
    (progn
      (delete-window (get-buffer-window buffer))
      (kill-buffer buffer))))

(put 'dired-find-alternate-file 'disabled nil)

;; Blog
(require 'yasnippet)
(yas-global-mode 1)

(setq org-publish-project-alist
      '(
	("iquicktest"
         ;; Path to org files.
         :base-directory "~/iquicktest.github.io/org"
         :base-extension "org"

         ;; Path to Jekyll Posts
         :publishing-directory "~/iquicktest.github.io/_posts/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t
         )
	("51music"
         ;; Path to org files.
         :base-directory "~/Dropbox/1millionDollars/51music.github.com/org/"
         :base-extension "org"

         ;; Path to Jekyll Posts
         :publishing-directory "~/Dropbox/1millionDollars/51music.github.com/_posts/"
         :recursive t
         :publishing-function org-html-publish-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t
         )
	)
      )


(use-package pyim
  :ensure nil
  :config
  ;; 激活 basedict 拼音词库
  (use-package pyim-basedict
    :ensure nil
    :config (pyim-basedict-enable))

  (setq default-input-method "pyim")

  ;; 我使用全拼
  (setq pyim-default-scheme 'quanpin)

  ;; 设置 pyim 探针设置，这是 pyim 高级功能设置，可以实现 *无痛* 中英文切换 :-)
  ;; 我自己使用的中英文动态切换规则是：
  ;; 1. 光标只有在注释里面时，才可以输入中文。
  ;; 2. 光标前是汉字字符时，才能输入中文。
  ;; 3. 使用 M-j 快捷键，强制将光标前的拼音字符串转换为中文。
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-dynamic-english
                  pyim-probe-isearch-mode
                  pyim-probe-program-mode
                  pyim-probe-org-structure-template))

  (setq-default pyim-punctuation-half-width-functions
                '(pyim-probe-punctuation-line-beginning
                  pyim-probe-punctuation-after-punctuation))

  ;; 开启拼音搜索功能
  (pyim-isearch-mode 1)

  ;; 使用 pupup-el 来绘制选词框
  (setq pyim-page-tooltip 'popup)

  ;; 选词框显示5个候选词
  (setq pyim-page-length 5)

  ;; 让 Emacs 启动时自动加载 pyim 词库
  (add-hook 'emacs-startup-hook
            #'(lambda () (pyim-restart-1 t)))
  :bind
  (("M-j" . pyim-convert-code-at-point) ;与 pyim-probe-dynamic-english 配合
   ("C-;" . pyim-delete-word-from-personal-buffer)))

(global-set-key (kbd "C-\\") 'toggle-input-method)
(define-key org-mode-map "\M-q" 'toggle-truncate-lines)



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
    (magit yaml-mode wgrep use-package smartparens smart-mode-line rg restclient rainbow-identifiers rainbow-delimiters pytest pyim meghanada lsp-ui json-mode isend-mode hc-zenburn-theme google-c-style golden-ratio go-mode flx expand-region evil-leader elpy dumb-jump dired-subtree diminish diff-hl counsel-projectile company-lsp ace-window))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
