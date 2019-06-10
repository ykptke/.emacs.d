;; install it major mode for JavaScript editing

(add-to-list 'auto-mode-alist '("\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . rjsx-mode))

;; indent
(setq js-indent-level 2)

(eval-after-load 'js-mode
  '(add-hook 'js-mode-hook #'add-node-modules-path))

(eval-after-load 'rjsx-mode
  '(add-hook 'rjsx-mode-hook #'add-node-modules-path))
