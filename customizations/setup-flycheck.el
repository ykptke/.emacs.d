 ;; flycheck syntax check
(global-flycheck-mode)

;; disable jshint
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; use eslint with js-mode
(flycheck-add-mode 'javascript-eslint 'js-mode)

