;;; init.el --- Project Cybertron
;; Author: Kenish
;;; Commentary:
;;; The Ultimate Emacs Setup!
;;; Code:

(when (version< emacs-version "25.1")
  (error "Icemacs requires Emacs 25.1 and above!"))

;; Increase the gc-cons-threshold to a very high number to decrease the load time
(eval-and-compile
  (setq gc-cons-threshold 402653184
        gc-cons-percentage 0.6))

;; Add a hook to reset this value after initialization
(add-hook 'icemacs-post-init-hook #'(lambda () (setq gc-cons-threshold 16777216
                                                   gc-cons-percentage 0.1)))

;; Org hook
;; Don't attempt to find/apply special file handlers to files loaded during startup.
(let ((file-name-handler-alist nil))
  ;; If config is pre-compiled, then load that
  (if (file-exists-p (expand-file-name "bwoah.elc" user-emacs-directory))
      (load-file (expand-file-name "bwoah.elc" user-emacs-directory))
    ;; Otherwise use org-babel to tangle and load the configuration
    (require 'org)
    (org-babel-load-file (expand-file-name "bwoah.org" user-emacs-directory))))

;;; init.el ends here