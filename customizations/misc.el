;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; web-mode indent
(setq web-mode-markup-indent-offset 2)
;; CSS offset indentation
(setq web-mode-css-indent-offset 2)
(setq css-indent-offset 2)
;; Script/code offset indentation (for JavaScript, Java, PHP, Ruby, Go, VBScript, Python, etc.)
(setq web-mode-code-indent-offset 2)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

;; close all buffers
(defun kill-other-buffers ()
  "Kill all buffers except current one and toolkit (*Messages*, *scratch*). Close other windows."
  (interactive)
  (mapc 'kill-buffer (cl-remove-if
                      (lambda (x)
                        (or
                         (eq x (current-buffer))
                         (member (buffer-name x) '("*Messages*" "*scratch*"))))
                      (buffer-list)))
  (delete-other-windows))

(global-set-key (kbd "C-x a k") 'kill-other-buffers)
