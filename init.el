;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)

(package-initialize)
;; This is only needed once, near the top of the file
(org-babel-load-file (concat user-emacs-directory "config.org"))
(put 'dired-find-alternate-file 'disabled nil)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
