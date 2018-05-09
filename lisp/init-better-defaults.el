

;; abbrevmode auto complete
(abbrev-mode t)
(define-abbrev-table 'global-abbrev-table '(
					    ;;demo
					    ("8a" "aaaaaa")))


;; set zsh
(setq multi-term-program "/bin/zsh")
(add-hook 'term-mode-hook
          (lambda ()
            (setq term-buffer-maximum-size 10000)))




;; auto save file
(setq auto-save-default nil)

;; C-x C-j go to current directory
(require 'dired-x)

;; avoid lisp to close the "'"
(sp-local-pair '(emacs-lisp-mode lisp-interaction-mode) "'" nil :actions nil)




;; add recent file
(recentf-mode 1)
(setq recentf-max-menu-item 10)





(provide 'init-better-defaults)
