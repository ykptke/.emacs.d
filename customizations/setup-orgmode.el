;; Load Org-Reveal
(require 'ox-reveal)
(setq org-reveal-root "https://revealjs.com/")
(setq org-reveal-title-slide nil)

;; appearance
(require 'org-bullets)
(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))