;;; Configuration --- Project Cybertron
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10) ;Breathing room
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell t)
(display-time-mode t)

(setenv "BROWSER" "firefox")t

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(column-number-mode)
(global-display-line-numbers-mode t)
;; Overwrite when pasting
(delete-selection-mode 1)
;; Wrap lines
(global-visual-line-mode t)

;; Default theme temp
(load-theme 'tango-dark)

;; Quality of Life
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(use-package nyan-mode :ensure t)
;; Which key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

;; Modeline
(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1))

;; Aggressive-indent
(use-package aggressive-indent
  :ensure t
  :config
  )

;; Ivy Mode
(ivy-mode)
(setq ivy-use-virtual-buffers t)
(setq enable-recursive-minibuffers t)
;; enable this if you want `swiper' to use it
;; (setq search-default-mode #'char-fold-to-regexp)
(global-set-key "\C-s" 'swiper)
(global-set-key (kbd "C-c C-r") 'ivy-resume)
(global-set-key (kbd "<f6>") 'ivy-resume)
(global-set-key (kbd "M-x") 'counsel-M-x)

;; Comapany
(use-package company
  :ensure t
  :config
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 3)
  (global-company-mode t))


;; Flycheck
(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))

;; Snippets
(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'clojure-mode-hook #'rainbow-delimiters-mode))
(show-paren-mode t)

;; Initialize package sources
(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(nyan-mode company counsel aggressive-indent rainbow-delimiters flycheck chess which-key doom-modeline ivy use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
