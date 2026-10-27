;;; code-intelligence.el --- Completion, linting and project support -*- lexical-binding: t; -*-

;;; Commentary:
;; In-buffer completion, Flycheck integration, compilation, tree-sitter
;; highlighting, project management and Dired sorting.

;;; Code:

(require 'company)
(require 'flycheck)
(require 'flycheck-aspell)
(require 'fancy-compilation)
(require 'tree-sitter)
(require 'projectile)
(require 'dired-quick-sort)
(require 'llm-integration)

(defun company-dictionary-mode ()
  "Use 'company-ispell' only, with a delay of 1s."
  (setq-local company-idle-delay 1
              company-minimum-prefix-length 4
              company-backends '(company-ispell))
  (company-mode 1))

(defun code-intelligence-init ()
  "Configure completion, linting and project support."
  (with-eval-after-load 'compile
    (fancy-compilation-mode))
  (setq compilation-scroll-output t
        compilation-ask-about-save nil
        compilation-always-kill t)

  (advice-add 'compilation-start
              :around (lambda (orig &rest args)
                        (let ((shell-file-name (or (executable-find "bash") "/bin/sh")))
                          (apply orig args))))

  (add-hook 'prog-mode-hook 'company-mode)
  (setq company-idle-delay 0.3
        company-minimum-prefix-length 2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-selection-wrap-around t
        company-show-quick-access t
        company-show-quick-access t)

  (add-hook 'message-mode-hook #'company-dictionary-mode)

  (add-hook 'after-init-hook 'global-flycheck-mode)
  (add-hook 'after-init-hook 'global-flycheck-annotate-mode)
  (global-flycheck-eglot-mode)
  (setq ispell-dictionary "en_GB"
        ispell-program-name "aspell"
        ispell-alternate-dictionary (getenv "WORDLIST")
        ispell-silently-savep t)
  (add-to-list 'flycheck-checkers 'mail-aspell-dynamic)

  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (add-to-list 'projectile-project-root-files "flake.nix")
  (add-hook 'projectile-after-switch-project-hook
            (lambda ()
              (gptel-mcp-connect '("filesystem"))
              (let ((root (projectile-project-root)))
                (unless (seq-some (lambda (r)
                                    (file-equal-p root
                                                  (if (stringp r) r (plist-get r :path))))
                                  (mcp-get-roots "filesystem"))
                  (mcp-add-root "filesystem" root)))))

  (global-tree-sitter-mode)
  (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
  (setq treesit-font-lock-level 4)

  (setq dired-quick-sort-group-directories-last ?y
        dired-quick-sort-sort-by-last "version"
        dired-quick-sort-reverse-last ?n)
  (setq dired-ls-sorting-switches "-laB")
  (dired-quick-sort-setup))

(unless noninteractive
  (code-intelligence-init))

(provide 'code-intelligence)
;;; code-intelligence.el ends here
