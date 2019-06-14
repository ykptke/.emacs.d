;; ido-vertical-mode
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; Make "C-x o" prompt for a target window when there are more than 2
(require 'switch-window)
(setq-default switch-window-shortcut-style 'number)
(setq-default switch-window-timeout nil)
(global-set-key (kbd "C-x o") 'switch-window)
