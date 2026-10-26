;;; editor-defaults.el --- Editor defaults and session state -*- lexical-binding: t; -*-

;;; Commentary:
;; Basic editing behaviour, backup/versioning policy, session persistence,
;; and window navigation keys.

;;; Code:

(setq user-emacs-directory "~/.config/emacs/")

(defun editor-defaults-init ()
  "Apply basic editing defaults and session state."
  (setq-default
   indent-tabs-mode nil
   tab-width 2
   fill-column 80
   require-final-newline t)

  (setq custom-file (concat user-emacs-directory "custom.el"))

  (when (file-exists-p custom-file)
    (load custom-file))

  (setq
   backup-by-copying t
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2
   version-control t
   vc-follow-symlinks t
   dired-listing-switches "-alB")

  (require 'savehist)
  (savehist-mode)

  (require 'recentf)
  (recentf-mode 1)
  (setq recentf-max-saved-items 100)

  (global-set-key (kbd "C-c <left>")  'windmove-left)
  (global-set-key (kbd "C-c <right>") 'windmove-right)
  (global-set-key (kbd "C-c <up>")    'windmove-up)
  (global-set-key (kbd "C-c <down>")  'windmove-down))

(unless noninteractive
  (editor-defaults-init))

(provide 'editor-defaults)
;;; editor-defaults.el ends here
