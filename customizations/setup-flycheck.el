;; flycheck syntax check
(global-flycheck-mode)

;; disable jshint
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslit with js2-mode
(flycheck-add-mode 'javascript-eslint 'js2-mode)

;; use eslit with rjsx-mode
(flycheck-add-mode 'javascript-eslint 'rjsx-mode)

;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

;; Change programmatically flycheck-javascript-eslint-executable for local node_modules installation
(defun my/use-eslint-from-node-modules ()
 (let* ((root (locate-dominating-file
               (or (buffer-file-name) default-directory)
               "node_modules"))
        (eslint
         (and root
              (expand-file-name "node_modules/.bin/eslint"
                                root))))
   (when (and eslint (file-executable-p eslint))
     (setq-local flycheck-javascript-eslint-executable eslint))))
(add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)
