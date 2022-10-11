(unless (featurep 'straight)
  ;; Bootstrap straight.el
  (defvar bootstrap-version)
  (defvar comp-deferred-compilation-deny-list ()) ; workaround, otherwise straight shits itself
  (let ((bootstrap-file
         (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
        (bootstrap-version 5))
    (unless (file-exists-p bootstrap-file)
      (with-current-buffer
          (url-retrieve-synchronously
       "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
       'silent 'inhibit-cookies)
    (goto-char (point-max))
    (eval-print-last-sexp)))
    (load bootstrap-file nil 'nomessage)))

;; Use straight.el for use-package expressions
(straight-use-package 'use-package)
(setq straight-use-package-by-default t)

(setq frame-title-format '(buffer-file-name "Autobots, roll out! - %f" ("%b")))
(fset 'yes-or-no-p 'y-or-n-p)
(setq visible-bell nil)
(display-time-mode t)

;; Line numbers
(column-number-mode)
(when (fboundp 'global-display-line-numbers-mode)
  (global-display-line-numbers-mode t))

;; ESCAPAYE
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Comment regions
(global-set-key (kbd "C-/") 'comment-or-uncomment-region)

;; Better way to manage backups
(setq backup-directory-alist `(("." . ,(expand-file-name ".tmp/backups/" user-emacs-directory))))

;; Delete means delete
(setq delete-by-moving-to-trash t)

;; Remove bloated space
(add-hook 'before-save-hook #'whitespace-cleanup)
(setq-default sentence-end-double-space nil)

;; Iterate through camelCase
(global-subword-mode 1)

;; Wrap lines
(global-visual-line-mode t)

;; Overwrite when pasting
(delete-selection-mode t)

;; Cursor covers the entire letter
(setq x-stretch-cursor t)

(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook (lambda () (setq indent-tabs-mode nil)))

(defun sudo-find-file (file-name)
  "Like find file, but opens the file as root."
  (interactive "FSudo Find File: ")
  (let ((tramp-file-name (concat "/sudo::" (expand-file-name file-name))))
    (find-file tramp-file-name)))

(set-frame-parameter (selected-frame) 'alpha '(97 . 97))
(add-to-list 'default-frame-alist '(alpha . (97 . 97)))
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package all-the-icons
  :defer t
  :straight t)
(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-moonlight t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package nyan-mode
  :straight t
  :config
  (setq nyan-wavy-trail 1)
  (nyan-mode))

(set-face-attribute 'default nil :font "Fira Code Retina" :height 160)

(use-package dashboard
  :straight t
  :init
  (setq dashboard-set-init-info t)
  (setq dashboard-show-shortcuts nil)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-banner-logo-title "Welcome to icemacs!")
  (setq dashboard-startup-banner "~/.emacs.d/assets/iceman.png")
  (setq dashboard-projects-switch-function 'counsel-projectile-switch-project-by-name)
  (setq dashboard-set-navigator t)
  (setq dashboard-navigator-buttons
      `(;; line1
    ((,nil
    "Bwoah"
    "It's lights out and away we go!"
    (lambda (&rest _) (find-file "~/.emacs.d/bwoah.org"))
    )))
    )
  :config
  (dashboard-setup-startup-hook))

(use-package "window"
  :straight nil
  :preface
  (defun kenzo/split-and-follow-horizontally ()
    "Split window below."
    (interactive)
    (split-window-below)
    (other-window 1))
  (defun kenzo/split-and-follow-vertically ()
    "Split window right."
    (interactive)
    (split-window-right)
    (other-window 1))
  :config
  (setq split-width-threshold 100)
  (global-set-key (kbd "C-x 2") #'kenzo/split-and-follow-horizontally)
  (global-set-key (kbd "C-x 3") #'kenzo/split-and-follow-vertically))

(use-package switch-window
  :straight t
  :config
    (setq switch-window-input-style 'minibuffer)
    (setq switch-window-increase 4)
    (setq switch-window-threshold 2)
    (setq switch-window-shortcut-style 'qwerty)
    (setq switch-window-qwerty-shortcuts
        '("a" "s" "d" "f" "j" "k" "l" "i" "o"))
  :bind
    ([remap other-window] . switch-window))

(use-package vertico
  :straight t
  :bind (:map vertico-map
              ("C-j" . vertico-next)
              ("C-k" . vertico-previous)
              ("C-f" . vertico-exit)
              :map minibuffer-local-map
              ("M-h" . backward-kill-word))
  :custom
  (vertico-cycle t)
  :init
  (vertico-mode))

;; Saves search history for counsel
(use-package savehist
  :init
  (savehist-mode))

;; Describes each smex function
(use-package marginalia
  :after vertico
  :straight t
  :custom
  (marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
  :init
  (marginalia-mode))

(setq completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; Can't live without this
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch))
  (setq completion-styles '(orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

;; In short enables a right click
(use-package embark
  :straight t
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
           '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
             nil
         (window-parameters (mode-line-format . none)))))
;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :straight t
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(defun dw/get-project-root ()
  (when (fboundp 'projectile-project-root)
    (projectile-project-root)))

(use-package consult
  :straight t
  :demand t
  :bind (("C-s" . consult-line)
     ("C-M-l" . consult-imenu)
     ("C-x b" . consult-buffer)
     ("C-x y" . consult-yasnippet)
     ("C-M-j" . persp-switch-to-buffer*)
     :map minibuffer-local-map
     ("C-r" . consult-history))
  :custom
  (consult-project-root-function #'dw/get-project-root)
  (completion-in-region-function #'consult-completion-in-region))

(use-package treemacs
  :commands treemacs
  :bind
  (:map global-map
        ([f5] . treemacs)
        ("C-<f5>" . treemacs-select-window))
  :config
  (setq treemacs-is-never-other-window t))

(use-package company
  :diminish company-mode
  :defines
  (company-dabbrev-ignore-case company-dabbrev-downcase)
  :bind
  (:map company-active-map
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous)
        ("<tab>" . company-complete-common-or-cycle)
        :map company-search-map
        ("C-p" . company-select-previous)
        ("C-n" . company-select-next))
  :custom
  (company-idle-delay 0)
  (company-echo-delay 0)
  (company-minimum-prefix-length 1)
  :hook
  (after-init . global-company-mode)
  (plantuml-mode . (lambda () (set (make-local-variable 'company-backends)
                                   '((company-yasnippet
                                      ;; company-dabbrev
                                      )))))
  ((go-mode
    c++-mode
    c-mode
    objc-mode) . (lambda () (set (make-local-variable 'company-backends)
                                 '((company-yasnippet
                               company-lsp
                               company-files
                               ;; company-dabbrev-code
                               )))))
  :config
  ;; using child frame
  (use-package company-posframe
    :hook (company-mode . company-posframe-mode))
  ;; Show pretty icons
  (use-package company-box
    :diminish
    :hook (company-mode . company-box-mode)
    :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
    :config
    (setq company-box-backends-colors nil)
    (setq company-box-show-single-candidate t)
    (setq company-box-max-candidates 50)

    (defun company-box-icons--elisp (candidate)
      (when (derived-mode-p 'emacs-lisp-mode)
        (let ((sym (intern candidate)))
          (cond ((fboundp sym) 'Function)
                ((featurep sym) 'Module)
                ((facep sym) 'Color)
                ((boundp sym) 'Variable)
                ((symbolp sym) 'Text)
                (t . nil)))))

    (with-eval-after-load 'all-the-icons
      (declare-function all-the-icons-faicon 'all-the-icons)
      (declare-function all-the-icons-fileicon 'all-the-icons)
      (declare-function all-the-icons-material 'all-the-icons)
      (declare-function all-the-icons-octicon 'all-the-icons)
      (setq company-box-icons-all-the-icons
            `((Unknown . ,(all-the-icons-material "find_in_page" :height 0.7 :v-adjust -0.15))
              (Text . ,(all-the-icons-faicon "book" :height 0.68 :v-adjust -0.15))
              (Method . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Function . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Constructor . ,(all-the-icons-faicon "cube" :height 0.7 :v-adjust -0.05 :face 'font-lock-constant-face))
              (Field . ,(all-the-icons-faicon "tags" :height 0.65 :v-adjust -0.15 :face 'font-lock-warning-face))
              (Variable . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face))
              (Class . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
              (Interface . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01))
              (Module . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.15))
              (Property . ,(all-the-icons-octicon "package" :height 0.7 :v-adjust -0.05 :face 'font-lock-warning-face)) ;; Golang module
              (Unit . ,(all-the-icons-material "settings_system_daydream" :height 0.7 :v-adjust -0.15))
              (Value . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'font-lock-constant-face))
              (Enum . ,(all-the-icons-material "storage" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-orange))
              (Keyword . ,(all-the-icons-material "filter_center_focus" :height 0.7 :v-adjust -0.15))
              (Snippet . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))
              (Color . ,(all-the-icons-material "palette" :height 0.7 :v-adjust -0.15))
              (File . ,(all-the-icons-faicon "file-o" :height 0.7 :v-adjust -0.05))
              (Reference . ,(all-the-icons-material "collections_bookmark" :height 0.7 :v-adjust -0.15))
              (Folder . ,(all-the-icons-octicon "file-directory" :height 0.7 :v-adjust -0.05))
              (EnumMember . ,(all-the-icons-material "format_align_right" :height 0.7 :v-adjust -0.15 :face 'all-the-icons-blueb))
              (Constant . ,(all-the-icons-faicon "tag" :height 0.7 :v-adjust -0.05))
              (Struct . ,(all-the-icons-faicon "clone" :height 0.65 :v-adjust 0.01 :face 'font-lock-constant-face))
              (Event . ,(all-the-icons-faicon "bolt" :height 0.7 :v-adjust -0.05 :face 'all-the-icons-orange))
              (Operator . ,(all-the-icons-fileicon "typedoc" :height 0.65 :v-adjust 0.05))
              (TypeParameter . ,(all-the-icons-faicon "hashtag" :height 0.65 :v-adjust 0.07 :face 'font-lock-const-face))
              (Template . ,(all-the-icons-faicon "code" :height 0.7 :v-adjust 0.02 :face 'font-lock-variable-name-face))))))
  ;; Show quick tooltip
  (use-package company-quickhelp
      :defines company-quickhelp-delay
      :bind (:map company-active-map
                  ("M-h" . company-quickhelp-manual-begin))
      :hook (global-company-mode . company-quickhelp-mode)
      :custom (company-quickhelp-delay 0.8)))

(use-package yasnippet
        :defer t
        :straight (:build t)
        :init
        (yas-global-mode)
        :hook ((prog-mode . yas-minor-mode)
               (text-mode . yas-minor-mode)))

   (use-package yasnippet-snippets
      :defer t
      :after yasnippet
      :straight (:build t))

(use-package yatemplate
  :defer t
  :after yasnippet
  :straight (:build t))

(use-package consult-yasnippet
  :defer t
  :after yasnippet
  :straight (:build t))

(use-package which-key
  :straight (:build t)
  :defer t
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.4
      which-key-idle-secondary-delay 0.4))

(use-package flycheck
  :straight (:build t)
  :defer t
  :init
  (global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Rerunning checks on every newline is a mote excessive.
  (delq 'new-line flycheck-check-syntax-automatically)
  ;; And don’t recheck on idle as often
  (setq flycheck-idle-change-delay 2.0)

  ;; For the above functionality, check syntax in a buffer that you
  ;; switched to on briefly. This allows “refreshing” the syntax check
  ;; state for several buffers quickly after e.g. changing a config
  ;; file.
  (setq flycheck-buffer-switch-check-intermediate-buffers t)

  ;; Display errors a little quicker (default is 0.9s)
  (setq flycheck-display-errors-delay 0.2))

(use-package flycheck-posframe
  :straight (:build t)
  :hook (flycheck-mode . flycheck-posframe-mode)
  :config
  (setq flycheck-posframe-warning-prefix "! "
        flycheck-posframe-info-prefix    "··· "
        flycheck-posframe-error-prefix   "X "))

(use-package general
:defer t
:straight (:build t)
:init
(general-auto-unbind-keys))

(use-package evil
  :straight (:build t)
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :straight (:build t)
  :config
  (evil-collection-init))
