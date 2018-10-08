;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)

;; empty scratch buffer
(setq inhibit-splash-screen t)
(switch-to-buffer "*scratch*")

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; disable selection copy
(setq select-enable-primary nil)

;; delete whitespaces
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; Company is a text completion framework
(add-hook 'after-init-hook 'global-company-mode)

;; Smartparens is a minor mode for dealing with pairs
(smartparens-global-mode)

;; search anything in files.
(global-set-key (kbd "M-ğ s") 'rgrep)
;; deleting all white space in file.
(global-set-key (kbd "M-ğ w") 'delete-trailing-whitespace)

;; magit status
(global-set-key (kbd "M-ş s") 'magit-status)
;; magit blame
(global-set-key (kbd "M-ş b") 'magit-blame)
