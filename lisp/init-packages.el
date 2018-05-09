(require 'cl)

;; package management
(require 'package)
(add-to-list 'package-archives (cons "melpa" "http://melpa.org/packages/") t)

;; Add Packages
(defvar my/packages '( 
                ;; --- Auto-completion ---
                company
                ;; --- Better Editor ---
                hungry-delete
                swiper
                counsel
                smartparens
                ;; --- Major Mode ---
                js2-mode
		popwin
                ;; --- Minor Mode ---
                nodejs-repl
                exec-path-from-shell
                ;; --- Themes ---
                gruvbox-theme
                ;; solarized-theme
                ) "Default packages")

(setq package-selected-packages my/packages)

(defun my/packages-installed-p ()
     (loop for pkg in my/packages
           when (not (package-installed-p pkg)) do (return nil)
           finally (return t)))

(unless (my/packages-installed-p)
     (message "%s" "Refreshing package database...")
     (package-refresh-contents)
     (dolist (pkg my/packages)
       (when (not (package-installed-p pkg))
         (package-install pkg))))

;; Find Executable Path on OS X
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))



;; set hungry delete
(require 'hungry-delete)
(global-hungry-delete-mode)



;; add smartparens config 
(smartparens-global-mode t)



;; popwin setting
(require 'popwin)
(popwin-mode t)



;; highlight global line
(global-hl-line-mode t)
(require 'recentf)

;; 开启全局 Company 补全
(global-company-mode 1)



;; ivy setting
(ivy-mode 1)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)



;; load theme
(load-theme 'gruvbox-dark-medium 1)



(provide 'init-packages)
