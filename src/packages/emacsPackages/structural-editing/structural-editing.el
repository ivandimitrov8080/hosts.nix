;;; structural-editing.el --- Navigation and structural editing -*- lexical-binding: t; -*-

;;; Commentary:
;; Jumping, multiple cursors, region expansion, structural editing, undo
;; history, delimiter highlighting and indentation.

;;; Code:

(require 'multiple-cursors)
(require 'expand-region)
(require 'smartparens)
(require 'smartparens-config)
(require 'undo-tree)
(require 'rainbow-delimiters)
(require 'aggressive-indent)

(defun structural-editing-init ()
  "Configure navigation and structural editing."

  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

  (global-set-key (kbd "C-=") 'er/expand-region)

  (add-hook 'prog-mode-hook 'smartparens-mode)
  (setq sp-highlight-pair-overlay nil
        sp-highlight-wrap-overlay nil
        sp-highlight-wrap-tag-overlay nil)

  (global-undo-tree-mode)
  (setq undo-tree-auto-save-history t)

  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode))

(unless noninteractive
  (structural-editing-init))

(provide 'structural-editing)
;;; structural-editing.el ends here
