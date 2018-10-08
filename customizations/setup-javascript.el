;; install it major mode for JavaScript editing
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))

;; indent
(setq js-indent-level 2)
