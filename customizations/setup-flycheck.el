 ;; flycheck syntax check
(add-hook 'js-mode-hook
          (lambda () (flycheck-mode t)))
