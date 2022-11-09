;; Backups ========================================== ;;

;; Auto-revert mode
(global-auto-revert-mode 1)
(setq auto-revert-interval 0.5)

;; Backup stored in /tmp
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" , temporary-file-directory t)))

(setq backup-by-copying t)
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)
(setq create-lockfiles nil)

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)


;; User ============================================= ;;

(setq user-full-name "Yakup Teke"
      user-mail-address "ykpteke@gmail.com")


;; Defaults ========================================= ;;

;; (setq frame-title-format '("%b"))
(setq frame-title-format '("%f [%m]"))

(menu-bar-mode -1)
(tool-bar-mode -1)
(global-display-line-numbers-mode 1)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; set default font
(set-frame-font "Inconsolata 16" nil t)

(setq use-short-answers t)
(setq ring-bell-function 'ignore)
(setq visible-bell nil)

(setq fill-column 100)

(show-paren-mode 1)
;; Treat Camelcase as words
(global-subword-mode 1)
;; Remember cursor place
(setq save-place-file (locate-user-emacs-file "saveplace"))
(setq save-place-forget-unreadable-files t)
(save-place-mode 1)
;; Remember my bookmarks
(setq bookmark-save-flag 1)
;; Enable indentation+completion using the TAB key.
(setq tab-always-indent 'complete)

(setq-default indent-tabs-mode nil
              tab-stop-list    ()
              tab-width        2)
(setq sentence-end-double-space nil)
(setq scroll-step 1) ; keyboard scroll one line at a time
(setq scroll-preserve-screen-position t)
(setq scroll-conservatively 101)


;; Global mark ring
(setq global-mark-ring-max 50000)

(delete-selection-mode 1)

(setq icomplete-compute-delay 0)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; maximize frame
(set-frame-parameter (selected-frame) 'fullscreen 'maximized)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; macos specific settings
(when (eq system-type 'darwin) 
  (setq mac-option-modifier nil
        mac-command-modifier 'meta
        x-select-enable-clipboard t))

;; disable selection copy
(setq select-enable-primary nil)


;; Package ========================================== ;;

;; Initialize package sources
(require 'package)

(setq package-archives
      '(("elpa" . "https://elpa.gnu.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")
        ("melpa" . "https://melpa.org/packages/")))

;; Highest number gets priority (what is not mentioned has priority 0)
(setq package-archive-priorities
      '(("elpa" . 2)
        ("nongnu" . 1)))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-and-compile
  (setq use-package-always-ensure t
        use-package-expand-minimally t))


;; Keybindings ======================================= ;;

(defun backward-delete-word (arg)
  "Delete characters backward until encountering the beginning of a word.
With argument ARG, do this that many times."
  (interactive "p")
  (delete-region (point) (progn (backward-word arg) (point))))

(global-set-key (kbd "C-M-<backspace>") 'backward-kill-word)
(global-set-key (kbd "M-<backspace>") 'backward-delete-word)

(let ((map global-map))
  (define-key map (kbd "C-;") #'comment-line)
  (define-key map (kbd "C-c C-p") #'previous-buffer)
  (define-key map (kbd "C-c C-n") #'next-buffer)
  ;; Misc
  (define-key map (kbd "C-x C-b") #'ibuffer)
  (define-key map (kbd "M-z") #'zap-up-to-char)
  ;; Isearch
  (define-key map (kbd "C-s") #'isearch-forward-regexp)
  (define-key map (kbd "C-r") #'isearch-backward-regexp)
  (define-key map (kbd "C-M-s") #'isearch-forward)
  (define-key map (kbd "C-M-r") #'isearch-backward)
  ;; magit status
  (define-key map (kbd "C-x g") #'magit-status)
  ;; Navigation paragraph
  (define-key map (kbd "M-p") #'backward-paragraph)
  (define-key map (kbd "M-n") #'forward-paragraph)
  ;; show/hide blocks
  (define-key map (kbd "C-c [") #'hs-hide-block)
  (define-key map (kbd "C-c ]") #'hs-show-block)
  (define-key map (kbd "C-x o") #'switch-window)
  )


;; Theme ========================================== ;;

(use-package dracula-theme
  ;; Dracula Theme.
  :ensure t
  :pin melpa
  :config
  (load-theme 'dracula t))


;; Hooks ============================================ ;;

(add-hook 'prog-mode-hook #'hs-minor-mode)


;; Recent Files ======================================= ;;

(require 'recentf)
;; ;; enable recent files mode.
(recentf-mode t)
;; ; 50 files ought to be enough.
(setq recentf-max-saved-items 50)


;; Diminish ======================================= ;;

(use-package diminish
  :ensure t)


;; Which Key ========================================= ;;

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))


;; Swith Window  ========================================= ;;

(use-package switch-window
  :config
  (setq-default switch-window-shortcut-style 'number)
  (setq-default switch-window-timeout nil))

(define-key global-map (kbd "C-x o") #'switch-window)


;; Projectile  ========================================= ;;

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/Projects/JotForm"))
  (setq projectile-switch-project-action #'helm-projectile))


;; Helm ================================================ ;;

(use-package helm
  :ensure t
  :demand
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("C-x c o" . helm-occur)) ;SC
  ("M-y" . helm-show-kill-ring) ;SC
  ("C-x r b" . helm-filtered-bookmarks) ;SC
  :preface (require 'helm-config)
  :config
  (helm-mode 1)
  (define-key helm-find-files-map (kbd "C-<backspace>") nil))

(use-package helm-projectile
  :config
  (setq projectile-completion-system 'helm)
  (helm-projectile-on))


;; yasnippet ================================== ;;

(use-package yasnippet
  :config
  (yas-global-mode 1))


;; Multiple Cursors ================================== ;;

(use-package multiple-cursors
  :ensure t
  :diminish multiple-cursors
  :bind (
         ("C->" . mc/mark-next-like-this)
         ("C-<" . mc/mark-previous-like-this)
         ("C-z" . mc/mark-next-like-this)
         ("M-C-z" . mc/mark-previous-like-this)
         ("C-c C-." . mc/mark-all-like-this)
         ("C-c e" . mc/mark-edit-lines)
         ))


;; Flycheck ================================== ;;

(use-package flycheck
  :after org
  :hook
  (org-src-mode . disable-flycheck-for-elisp)
  :custom
  (flycheck-emacs-lisp-initialize-packages t)
  (flycheck-display-errors-delay 0.1)
  :config
  (global-flycheck-mode)
  (flycheck-set-indication-mode 'left-margin))

(use-package flycheck-inline
  :config (global-flycheck-inline-mode))


;; Company ================================== ;;

(use-package company
  :bind (:map company-active-map
         ("C-n" . company-select-next)
         ("C-p" . company-select-previous))
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode t))


; Javascript Mode ================================= ;;

(use-package js2-mode
  :mode ("\\.js\\'")
  :config
  ;; (add-hook 'js2-mode-hook #'js2-imenu-extras-mode)
  (add-hook 'js2-mode-hook #'js2-refactor-mode)
  (define-key js-mode-map (kbd "M-.") nil)
  (add-hook 'js2-mode-hook (lambda ()
                             (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
  (add-hook 'js2-mode-hook (lambda ()
                             (setup-tide-mode)))
  )


;; Typescript Mode ================================== ;;

(use-package tide :ensure t)

(defun setup-tide-mode ()
  "Setup 'tide-mode' for js/ts."
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (tide-hl-identifier-mode +1))

(use-package typescript-mode
  :mode "\\.ts\\'"
  :config
  (setq typescript-indent-level 2)
  (add-hook 'typescript-mode-hook #'setup-tide-mode))


;; Web Mode ================================== ;;

(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  ;; enable typescript-tslint checker
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (add-hook 'web-mode-hook
            (lambda ()
              (when (string-equal "jsx" (file-name-extension buffer-file-name))
                (setup-tide-mode))))
  (flycheck-add-mode 'javascript-eslint 'web-mode))


;; Bash ========================================== ;;

(add-hook 'shell-mode-hook 'flycheck-mode)


;; YAML ========================================== ;;

(use-package yaml-mode)


;; JSON ========================================== ;;

(use-package json-mode)


;; GIT ========================================== ;;

(use-package magit
  :bind (("C-x g" . magit-status)
	 ("C-x M-g" . magit-dispatch))
  :init
  (setq project-switch-commands nil)) ; avoid magit error on C-n/C-p

(use-package git-timemachine
  :bind ("C-c t" . git-timemachine))

(use-package blamer
  :bind (("s-i" . blamer-show-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 140
                    :italic t)))
  :config
  (global-blamer-mode 1))


;; Treemacs ========================================== ;;

(use-package treemacs
  :bind ("C-c S" . treemacs))


;; Icons ========================================== ;;

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))


;; PHP Mode ========================================== ;;

(use-package php-mode)


;; LSP Mode ========================================== ;;

(use-package lsp-mode
    :hook (php-mode . lsp-deferred)
    :commands (lsp lsp-deferred))

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(with-eval-after-load 'lsp-mode
  (lsp-register-custom-settings '(("intelephense.diagnostics.undefinedConstants" nil t)))
  (lsp-register-custom-settings '(("intelephense.diagnostics.undefinedTypes" nil t))))

;; Smartparens ========================================== ;;

(use-package smartparens
  :diminish smartparens-mode ;; Do not show in modeline
  :init
  (require 'smartparens-config)
  :config
  (smartparens-global-mode t) ;; These options can be t or nil.
  (show-smartparens-global-mode t)
  (setq sp-show-pair-from-inside t)
  :custom-face
  (sp-show-pair-match-face ((t (:foreground "Orange")))) ;; Could also have :background "Grey" for example.
  )


;; Indent Guides

(use-package indent-guide
  :config
  (indent-guide-global-mode)
  (set-face-background 'indent-guide-face "dimgray"))


;; ========================================== ;;
