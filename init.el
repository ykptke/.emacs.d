;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.
(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
(when (not package-archive-contents)
  (package-refresh-contents))

;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar my-packages
  '(;; ido menu
    ido-vertical-mode

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
    
    ;; js mode for reactjs
    rjsx-mode

    ;; company is a text completion framework
    company

    ;; typescript mode
    typescript-mode

    ;; TypeScript Interactive Development Environment
    tide

    ;; yasnippets
    yasnippet
    react-snippets

    ;; elpy for python
    elpy

    ;; prompt for a target window
    switch-window

    ;; turkish mode
    turkish

    ;; text scale
    default-text-scale
    
    ;; ripgrep
    ripgrep

    ac-php

    ;; ui
    powerline))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; custom themes
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; Langauage-specific
(load "setup-flycheck.el")
(load "setup-javascript.el")
(load "setup-php.el")
(load "setup-typescript.el")
(load "setup-yasnippet.el")
(load "setup-elpy.el")
