;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

(package-initialize)
;; This is only needed once, near the top of the file
(org-babel-load-file (expand-file-name "~/.emacs.d/config.org"))
(put 'dired-find-alternate-file 'disabled nil)




;;(require 'init-packages)
;;(require 'init-ui)
;;(require 'init-keybinding)
;;(require 'init-better-defaults)
;;(require 'init-org)
;;(require 'init-java)

;;(setq custom-file (expand-file-name "lisp/custom.el" user-emacs-directory))
;;(load-file custom-file)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-idle-delay 0.01)
 '(company-meghanada-prefix-length 2)
 '(company-minimum-prefix-length 1)
 '(custom-safe-themes
   (quote
    ("c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "021720af46e6e78e2be7875b2b5b05344f4e21fad70d17af7acfd6922386b61e" "e11569fd7e31321a33358ee4b232c2d3cf05caccd90f896e1df6cab228191109" "6ac7c0f959f0d7853915012e78ff70150bfbe2a69a1b703c3ac4184f9ae3ae02" "c3d4af771cbe0501d5a865656802788a9a0ff9cf10a7df704ec8b8efj69017c68" default)))
 '(package-selected-packages
   (quote
    (zenburn-theme company-lsp lsp-ui lsp-java gruvbox-theme nyan-mode company hungry-delete swiper counsel smartparens js2-mode nodejs-repl exec-path-from-shell monokai-theme))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
