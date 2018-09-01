(package-initialize)

(require 'package)
;; add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (git-timemachine yaml-mode htmlize web-mode org neotree magit json-mode js2-mode ido-vertical-mode highlight-indent-guides golden-ratio flycheck exec-path-from-shell auto-complete all-the-icons))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; js indendation
(setq js-indent-level 2)
(setq js-switch-indent-offset 2)

(setq-default indent-tabs-mode nil)

;; auto-complate mode
(require 'auto-complete)
(ac-config-default)

;; themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)

;; disable emacs menu
(menu-bar-mode -1)
(tool-bar-mode -1)

;; magit mode
(require 'magit)
(global-set-key (kbd "M-ş s") 'magit-status)
(global-set-key (kbd "M-ş b") 'magit-blame)

;; web-mode
(require 'web-mode)
(setq web-mode-markup-indent-offset 2)
;; CSS offset indentation
(setq web-mode-css-indent-offset 2)
;; Script/code offset indentation (for JavaScript, Java, PHP, Ruby, Go, VBScript, Python, etc.)
(setq web-mode-code-indent-offset 2)

;;jsx-mode
(add-to-list 'auto-mode-alist '("\\.js[x]?\\'" . web-mode))
(setq web-mode-content-types-alist
'(("jsx" . "\\.js[x]?\\'")))

;; highlight-indent-guides
(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'column)

;; json-mode
(require 'json-mode)

;; flycheck
(package-install 'flycheck)
(global-flycheck-mode)

;; disable jshint
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; use eslint with js-mode
(flycheck-add-mode 'javascript-eslint 'js-mode)

;; ido-vertical-mode
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; search anything in files.
(global-set-key (kbd "M-ğ s") 'rgrep)
;; deleting all white space in file.
(global-set-key (kbd "M-ğ w") 'delete-trailing-whitespace)

;; automatically resize the focused window
(require 'golden-ratio)
(golden-ratio-mode 1)

;; fullscreen on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(require 'all-the-icons)
(require 'neotree)
(global-set-key [f8] 'neotree-toggle)
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))

(setq-default indent-tabs-mode nil)

;; empty scratch buffer
(setq inhibit-splash-screen t)
(switch-to-buffer "*scratch*")

;; remove scratch message
(setq initial-scratch-message nil)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
