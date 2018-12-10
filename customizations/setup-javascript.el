;; install it major mode for JavaScript editing

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))

;; indent
(setq js-indent-level 2)
