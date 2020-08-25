 ;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)

;; Turn off toolbar
(tool-bar-mode -1)

;; Show line numbers
(global-linum-mode)

;; fullscreen on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Don't show native OS scroll bars for buffers because they're redundant
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; no bell
(setq ring-bell-function 'ignore)
