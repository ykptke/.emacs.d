;; These customizations change the way emacs looks and disable/enable
;; some user interface elements. Some useful customizations are
;; commented out, and begin with the line "CUSTOMIZE". These are more
;; a matter of preference and may require some fiddling to match your
;; preferences

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

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      x-select-enable-clipboard t

      ;; I'm actually not sure what this does but it's recommended?
      x-select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; no bell
(setq ring-bell-function 'ignore)

;; default-text-scale
(default-text-scale-mode)

;; theme
(load-theme 'solarized t)
(add-hook 'after-make-frame-functions
          (lambda (frame)
            (let ((mode (if (display-graphic-p frame) 'light 'dark)))
              (set-frame-parameter frame 'background-mode mode)
              (set-terminal-parameter frame 'background-mode mode))
            (enable-theme 'solarized)))

;; Toggle between light and dark
(defun light ()
  "Activate a light color theme."
  (interactive)
  (set-frame-parameter nil 'background-mode 'light)
  (enable-theme 'solarized)
  (powerline-reset))

(defun dark ()
  "Activate a dark color theme."
  (interactive)
  (set-frame-parameter nil 'background-mode 'dark)
  (enable-theme 'solarized)
  (powerline-reset))

;; powerline
(require 'powerline)
(powerline-default-theme)



