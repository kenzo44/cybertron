;;; early-init.el -*- lexical-binding: t; -*-

;; Optimize garbage collection during startup
(setq gc-cons-threshold #x40000000)  ; Temporarily disable GC by setting it to ~1GB

;; Temporarily disable the file name handler alist
(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

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

            ;; Remove UI clutter
            (menu-bar-mode -1)
            (tool-bar-mode -1)
            (scroll-bar-mode -1)))

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
