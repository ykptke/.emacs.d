;; install it major mode for JavaScript editing

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; Better imenu
(add-hook 'js2-mode-hook #'js2-imenu-extras-mode)

;; reactjs
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))