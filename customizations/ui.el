;; theme
(require 'dracula-theme)
(load-theme 'dracula t)

;; powerline
(require 'powerline)
(powerline-default-theme)

;; highlight indent
(require 'indent-guide)
(indent-guide-global-mode)
(set-face-background 'indent-guide-face "dimgray")
