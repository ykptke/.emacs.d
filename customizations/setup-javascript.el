(require 'web-mode)

;; auto-enable for .js/.jsx files
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) 

;; JSX syntax highlighting
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))

;; indentation and other settings
(defun web-mode-init-hook ()
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-markup-indent-offset 2))
  
(add-hook 'web-mode-hook  'web-mode-init-hook)