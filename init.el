;;; init.el --- Icemacs
;; Author: Kenish
;;; Commentary:
;;; The Ultimate Emacs Setup!
;;; Code:

;; Custom file garbage
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

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
