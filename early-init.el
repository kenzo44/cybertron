;;; early-init.el -*- lexical-binding: t; -*-
;; Emacs 27.1 introduced early-init.el, which is run before init.el, before
;; package and UI initialization happens, and before site files are loaded.
(when (version< emacs-version "25.1")
  (error "Icemacs requires Emacs 25.1 and above!"))
;; Increase the gc-cons-threshold to a very high number to decrease the load time
(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))
;; Add a hook to reset this value after initialization
(add-hook 'icemacs-post-init-hook #'(lambda () (setq gc-cons-threshold 16777216
                                                   gc-cons-percentage 0.1)))
;;; Package settings
(setq package-enable-at-startup nil
      inhibit-startup-message   t
      frame-resize-pixelwise    t  ; fine resize
      package-native-compile    t) ; native compile packages
;; UI - Disable visual cruft
;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(scroll-bar-mode -1)
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq initial-major-mode 'emacs-lisp-mode)
(set-default-coding-systems 'utf-8)
(setq-default default-directory "~/")
(setq default-frame-alist
      (append (list
               '(internal-border-width . 20)
               '(left-fringe    . 0)
               '(right-fringe   . 0)
               '(tool-bar-lines . 0)
               '(menu-bar-lines . 0)
               '(vertical-scroll-bars . nil)
               '(horizontal-scroll-bars . nil)
               '(height . 45)
               '(width . 85)
               )))

(add-to-list 'default-frame-alist '(undecorated . t))
;; echo buffer
;; Don't display any message
;; https://emacs.stackexchange.com/a/437/11934
(defun display-startup-echo-area-message ()
  (message ""))
;; And bury the scratch buffer, don't kill it
(defadvice kill-buffer (around kill-buffer-around-advice activate)
  (let ((buffer-to-kill (ad-get-arg 0)))
    (if (equal buffer-to-kill "*scratch*")
        (bury-buffer)
      ad-do-it)))
