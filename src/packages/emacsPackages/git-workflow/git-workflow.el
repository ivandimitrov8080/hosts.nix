;;; git-workflow.el --- Git integration -*- lexical-binding: t; -*-

;;; Commentary:
;; Magit and gutter diff indicators.

;;; Code:

(require 'magit)
(require 'diff-hl)

(defun git-workflow-init ()
  "Configure Git tooling."
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(unless noninteractive
  (git-workflow-init))

(provide 'git-workflow)
;;; git-workflow.el ends here
