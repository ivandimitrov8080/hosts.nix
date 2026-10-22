;;; ui-chrome.el --- Frame, theme and modeline chrome -*- lexical-binding: t; -*-

;;; Commentary:
;; Startup screen, frame chrome, fonts, theme, modeline, icons and web
;; browsing defaults.

;;; Code:

(require 'catppuccin-theme)
(require 'nerd-icons)
(require 'nerd-icons-xref)
(require 'nerd-icons-dired)
(require 'nerd-icons-completion)
(require 'nerd-icons-ibuffer)
(require 'doom-modeline)
(require 'inhibit-mouse)

(defun ui-chrome-init ()
  "Configure the visual chrome of Emacs."
  (setq inhibit-startup-screen t)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (show-paren-mode)
  (global-visual-line-mode)
  (add-to-list 'default-frame-alist '(alpha-background . 80))
  (custom-theme-set-faces
   'user
   '(variable-pitch ((t (:family "Inter" :height 140 :weight regular))))
   '(fixed-pitch ((t ( :family "FiraCode Nerd Font Mono" :height 120)))))

  (setq browse-url-browser-function #'eww-browse-url)
  (add-hook 'eww-after-render-hook 'eww-readable)

  (setq catppuccin-flavor 'mocha)
  (load-theme 'catppuccin :no-confirm)

  (nerd-icons-xref-mode)
  (add-hook 'dired-mode-hook 'nerd-icons-dired-mode)
  (add-hook 'ibuffer-mode-hook 'nerd-icons-ibuffer-mode)
  (nerd-icons-completion-mode)

  (doom-modeline-mode 1)
  (setq doom-modeline-height 25
        doom-modeline-bar-width 4
        doom-modeline-icon t
        doom-modeline-major-mode-icon t
        doom-modeline-major-mode-color-icon t
        doom-modeline-buffer-file-name-style 'truncate-upto-project
        doom-modeline-lsp t)

  (add-hook 'server-after-make-frame-hook 'inhibit-mouse-mode))

(unless noninteractive
  (ui-chrome-init))

(provide 'ui-chrome)
;;; ui-chrome.el ends here
