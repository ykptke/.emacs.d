;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)

;; Turn off toolbar
(tool-bar-mode -1)

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

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

(set-frame-font "Inconsolata 16" nil t)