;;; early-init.el -*- lexical-binding: t; -*-

;; Optimize garbage collection during startup
(setq gc-cons-threshold #x40000000)  ; Temporarily disable GC by setting it to ~1GB
(setq read-process-output-max (* 1024 1024)) ; A Higher Read Process Maximum

;; Temporarily disable the file name handler alist
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Remove UI clutter
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(tooltip-mode -1)

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)

;; Ensure that fullscreen and other frame-related settings are correctly set
(setq default-frame-alist
      (append default-frame-alist
              '((fullscreen . maximized) ; Fullscreen mode
                (internal-border-width . 20) ; Set internal border width
                (background-color . "#000000")
                )))

(when (eq system-type 'darwin)
  ;; macOS-specific settings
  (add-to-list 'default-frame-alist '(ns-appearance . dark)) ; Dark mode appearance
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t)) ; Transparent title bar
  (add-to-list 'default-frame-alist '(alpha . (95 . 95))))

;; Set the initial major mode to fundamental-mode to reduce startup overhead
(setq initial-major-mode 'fundamental-mode)

;; Restore settings and remove UI clutter after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; Restore the file name handler alist
            (setq file-name-handler-alist default-file-name-handler-alist)
            
            ;; Restore garbage collection thresholds
            (setq gc-cons-threshold 100000000  ; 100MB
                  gc-cons-percentage 0.1)
            
            ;; Garbage collect unreferenced objects
            (garbage-collect)

	     ;; Defaults
	    (set-default-coding-systems 'utf-8)
	    (setq-default default-directory "~/")))

;; Native compilation settings
(when (featurep 'native-compile)
  ;; Set the right directory to store the native compilation cache
  (let ((path (expand-file-name "eln-cache/" user-emacs-directory)))
    (setq-default native-comp-eln-load-path       (list path)
                  native-compile-target-directory path)
    (when (fboundp 'startup-redirect-eln-cache)
      (startup-redirect-eln-cache path)))
  (setq-default native-comp-async-report-warnings-errors nil  ;; Silence compiler warnings as they can be pretty disruptive
                native-comp-deferred-compilation         t    ;; Make native compilation happens asynchronously
                package-native-compile                   t)   ;; Compile installed packages
  )
