;;; org-workflow.el --- Org mode workflow -*- lexical-binding: t; -*-

;;; Commentary:
;; Org, capture, Babel, and the visual Org stack.

;;; Code:

(require 'org)
(require 'org-capture)
(require 'org-tempo)
(require 'olivetti)
(require 'valign)
(require 'org-modern)
(require 'org-appear)
(require 'mixed-pitch)
(require 'markdown-mode)

(defun org-workflow-init ()
  "Configure Org mode and friends."
  (setq org-startup-indented t
        org-hide-leading-stars t
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-src-content-indentation 0
        org-todo-keywords '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED"))
        org-log-done 'note
        org-default-notes-file (concat org-directory "/notes.org")
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-pretty-entities-include-sub-superscripts t
        org-ellipsis "  ...")

  (add-to-list 'org-agenda-files (concat org-directory "/notes.org"))

  (unless (assoc "t" org-capture-templates)
    (add-to-list 'org-capture-templates
                 '("t" "Task" entry (file+headline "" "Tasks")
                   "* TODO %?\n  %u\n  %a")
                 t))

  (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
  (add-hook 'org-mode-hook #'org-indent-mode)
  (add-hook 'org-mode-hook #'mixed-pitch-mode)
  (set-face-attribute 'org-level-1 nil :height 1.3 :weight 'bold)
  (set-face-attribute 'org-level-2 nil :height 1.15 :weight 'semibold)

  (add-hook 'org-mode-hook #'valign-mode)
  (add-hook 'markdown-mode-hook #'valign-mode)

  (add-hook 'org-mode-hook #'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
  (setq org-modern-star 'replace
        org-modern-hide-stars 'leading
        org-modern-todo t
        org-modern-table t
        org-modern-tag t
        org-modern-priority t
        org-modern-keyword t
        org-modern-block-name nil
        org-modern-list '((43 . "•") (45 . "–") (42 . "•"))
        org-modern-checkbox '((?X . "☑") (?- . "◐") (?\s . "☐")))

  (add-hook 'org-mode-hook #'org-appear-mode)

  (add-hook 'org-mode-hook #'olivetti-mode)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (haskell . t)
     (nix . t)))

  (setq org-confirm-babel-evaluate t
        org-src-preserve-indentation t
        haskell-process-type 'ghci))

(unless noninteractive
  (org-workflow-init))

(provide 'org-workflow)
;;; org-workflow.el ends here
