;; -*- lexical-binding: t; -*-
;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Custom file garbage
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

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

(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(set-fringe-mode 10) ;Breathing room
(menu-bar-mode -1)
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil)
(display-time-mode t)
(set-default-coding-systems 'utf-8)
(setq-default default-directory "~/")
(setenv "BROWSER" "firefox")t

;; Quality of Life
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)


(add-to-list 'default-frame-alist '(fullscreen . maximized))
