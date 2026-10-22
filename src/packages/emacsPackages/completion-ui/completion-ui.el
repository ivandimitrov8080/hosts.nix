;;; completion-ui.el --- Minibuffer completion and discovery -*- lexical-binding: t; -*-

;;; Commentary:
;; which-key, Vertico/Orderless/Marginalia, Consult search commands and
;; Helpful documentation commands.

;;; Code:

(require 'which-key)
(require 'vertico)
(require 'orderless)
(require 'marginalia)
(require 'consult)
(require 'helpful)

(defun completion-ui-init ()
  "Configure minibuffer completion and discovery commands."
  (which-key-mode)
  (setq which-key-idle-delay 0.5
        which-key-sort-order 'which-key-key-order-alpha)

  (vertico-mode)
  (setq vertico-cycle t)

  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion))))

  (marginalia-mode)
  (define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)

  (global-set-key (kbd "C-s") 'consult-line)
  (global-set-key (kbd "C-x b") 'consult-buffer)
  (global-set-key (kbd "C-x C-r") 'consult-recent-file)
  (global-set-key (kbd "M-g i") 'consult-imenu)
  (global-set-key (kbd "M-g g") 'consult-goto-line)
  (global-set-key (kbd "M-s g") 'consult-grep)
  (global-set-key (kbd "M-s r") 'consult-ripgrep)
  (setq consult-narrow-key "<")

  (global-set-key (kbd "C-h f") 'helpful-callable)
  (global-set-key (kbd "C-h v") 'helpful-variable)
  (global-set-key (kbd "C-h k") 'helpful-key)
  (global-set-key (kbd "C-c C-d") 'helpful-at-point)
  (global-set-key (kbd "C-h F") 'helpful-function)
  (global-set-key (kbd "C-h C") 'helpful-command))

(unless noninteractive
  (completion-ui-init))

(provide 'completion-ui)
;;; completion-ui.el ends here
