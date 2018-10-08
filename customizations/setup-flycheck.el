;; flycheck syntax check
(global-flycheck-mode)

;; disable jshint
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslit with js2-mode
(flycheck-add-mode 'javascript-eslint 'js2-mode)

;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)
