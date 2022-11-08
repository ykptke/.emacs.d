(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))


(defvar my-packages
  '(
    ;; TypeScript Interactive Development Environment
    tide
    ;; Smartparens is a minor mode for dealing with pairs
    smartparens
    ;; git integration
    magit
    git-timemachine

    ;; editing web templates
    web-mode

    ;; syntax checking extension
    flycheck

    exec-path-from-shell
    add-node-modules-path

    ;; js
    json-mode
    js2-mode
    rjsx-mode

    ;; company is a text completion framework
    company

    ;; typescript mode
    typescript-mode

    ;; yasnippets
    yasnippet

    ;; prompt for a target window
    switch-window

    ;; turkish mode
    turkish

    ;; text scale
    default-text-scale
    
    ;; ripgrep
    ripgrep

    terminal-here
    ;; php
    php-mode
    ac-php

    projectile
    counsel-projectile
    
    multiple-cursors
    haskell-mode
    ox-reveal
    org-bullets

    ;; new
    treemacs
    lsp-mode
    lsp-ui
    lsp-treemacs
    helm
    helm-rg
    helm-projectile
    dap-mode

    ;; ui
    dracula-theme
    powerline
    indent-guide))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq default-directory "~/")
(setq command-line-default-directory "~/")

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

(load "ui.el")
(load "navigation.el")
(load "editing.el")
(load "misc.el")
(load "setup-lspmode.el")
(load "setup-orgmode.el")
(load "setup-flycheck.el")

;; Langauage-specific
(load "setup-php.el")
(load "setup-javascript.el")
(load "setup-typescript.el")
(load "setup-haskell.el")

