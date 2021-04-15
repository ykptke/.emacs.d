;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

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

(global-set-key (kbd "C-c t") (lambda() (interactive) (find-file "~/yk/todo.org")))