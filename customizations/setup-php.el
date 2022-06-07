;; install it major mode for PHP editing

(defun bs-php-mode-hook ()
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 4)
  (setq php-template-compatibility nil)
  (subword-mode 1))

(add-hook 'php-mode-hook 'bs-php-mode-hook)

;; ac-php autocomplete
(require 'php-mode)

(add-hook 'php-mode-hook
          '(lambda ()
             ;; Enable auto-complete-mode
             (auto-complete-mode t)

             (require 'ac-php)
             (setq ac-sources '(ac-source-php))

             ;; As an example (optional)
             (yas-global-mode 1)

             ;; Enable ElDoc support (optional)
             (ac-php-core-eldoc-setup)

             ;; Jump to definition (optional)
             (define-key php-mode-map (kbd "M-]")
               'ac-php-find-symbol-at-point)

             (setq flycheck-phpcs-standard "PSR12")

             ;; Return back (optional)
             (define-key php-mode-map (kbd "M-[")
               'ac-php-location-stack-back)))
