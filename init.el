;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.

(package-initialize)
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(package-install-selected-packages)

(require 'package)
;; add MELPA to repository list
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/"))

;; js2-mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-basic-offset 2)
(setq-default js2-basic-offset 2)

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

;; flycheck
(require 'flycheck)
(global-flycheck-mode)

;; ido-vertical-mode
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

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

;; smartparens
(require 'smartparens)
(smartparens-global-mode)

;; json-mode
(require 'json-mode)

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
