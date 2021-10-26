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
(org-babel-load-file (expand-file-name "bwoah.org" user-emacs-directory))

;;; init.el ends here
