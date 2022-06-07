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


;; search anything with projectile
(projectile-mode +1)
;; Recommended keymap prefix on macOS
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(setq projectile-project-search-path '("~/Projects/JotForm"))
(setq projectile-switch-project-action #'projectile-dired)
(counsel-projectile-mode)

;; org-tree-slide-mode
(global-set-key (kbd "<f8>") 'org-tree-slide-mode)
(global-set-key (kbd "S-<f8>") 'org-tree-slide-skip-done-toggle)
