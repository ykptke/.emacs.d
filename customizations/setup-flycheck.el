;; flycheck syntax check
(global-flycheck-mode)

;; disable jshint
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslit with js2-mode
(flycheck-add-mode 'javascript-eslint 'js2-mode)

;; use eslint with rjsx-mode
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)
