;;; emacs.el --- Minimal Emacs configuration for functional web development -*- lexical-binding: t; -*-

;;; Commentary:
;; A minimal, functional Emacs configuration for Haskell, Elm, and Nix development
;; Configured to work with emacs-overlay and emacsWithPackagesFromUsePackage

;;; Code:

;; Basic settings
(setq-default
 indent-tabs-mode nil              ; Use spaces, not tabs
 tab-width 2                        ; 2-space indentation
 fill-column 80                     ; 80 character line width
 require-final-newline t            ; Ensure files end with newline
 custom-file (concat user-emacs-directory "custom.el")) ; Keep custom settings separate

;; Load custom file if it exists
(when (file-exists-p custom-file)
  (load custom-file))

;; UI improvements
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(column-number-mode 1)
(show-paren-mode 1)
(set-frame-parameter nil 'alpha-background 80)

;; Better defaults
(setq
 backup-by-copying t                ; Don't clobber symlinks
 delete-old-versions t              ; Clean up old backups
 kept-new-versions 6
 kept-old-versions 2
 version-control t                  ; Use version numbers for backups
 vc-follow-symlinks t)              ; Follow symlinks without asking

;;; Package Configuration

;; Theme
(require 'catppuccin-theme)
(setq catppuccin-flavor 'mocha)
(load-theme 'catppuccin :no-confirm)

;;; Completion & Discovery Framework

;; which-key - Show available keybindings in popup
(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.5
      which-key-sort-order 'which-key-key-order-alpha)

;; vertico - Vertical completion UI
(require 'vertico)
(vertico-mode)
(setq vertico-cycle t)

;; orderless - Flexible completion style (space-separated, out-of-order matching)
(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

;; marginalia - Rich annotations in minibuffer
(require 'marginalia)
(marginalia-mode)
(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)

;; consult - Enhanced navigation and search with live preview
(require 'consult)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)
(global-set-key (kbd "M-g i") 'consult-imenu)
(global-set-key (kbd "M-g g") 'consult-goto-line)
(global-set-key (kbd "M-s g") 'consult-grep)
(global-set-key (kbd "M-s r") 'consult-ripgrep)
(setq consult-narrow-key "<")

;; helpful - Better help buffers with source code and references
(require 'helpful)
(global-set-key (kbd "C-h f") 'helpful-callable)
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-c C-d") 'helpful-at-point)
(global-set-key (kbd "C-h F") 'helpful-function)
(global-set-key (kbd "C-h C") 'helpful-command)

;;; Visual Enhancements

;; all-the-icons - Icon support (run M-x all-the-icons-install-fonts once)
(when (display-graphic-p)
  (require 'all-the-icons))

;; all-the-icons-dired - Icons in file browser
(when (display-graphic-p)
  (require 'all-the-icons-dired)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

;; doom-modeline - Beautiful, informative status bar
(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-height 25
      doom-modeline-bar-width 4
      doom-modeline-icon t
      doom-modeline-major-mode-icon t
      doom-modeline-major-mode-color-icon t
      doom-modeline-buffer-file-name-style 'truncate-upto-project
      doom-modeline-lsp t)

;; rainbow-delimiters - Colorful nested parentheses/brackets
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'elm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'haskell-mode-hook 'rainbow-delimiters-mode)

;;; Navigation & Editing

;; avy - Jump to visible text by typing characters
(require 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "C-c C-j") 'avy-resume)
(setq avy-background t
      avy-style 'at-full)

;; multiple-cursors - Edit multiple locations simultaneously
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

;; expand-region - Intelligently expand selection
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; smartparens - Intelligent parens/bracket handling
(require 'smartparens)
(require 'smartparens-config)
(add-hook 'prog-mode-hook 'smartparens-mode)
(setq sp-highlight-pair-overlay nil
      sp-highlight-wrap-overlay nil
      sp-highlight-wrap-tag-overlay nil)

;; undo-tree - Visual undo history
(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t
      undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

;;; Built-in Enhancements

;; savehist - Save minibuffer history between sessions
(require 'savehist)
(savehist-mode)

;; recentf - Track recently opened files
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100)

;; Eglot - LSP client (built-in Emacs 29+)
(require 'eglot)
(add-hook 'nix-mode-hook 'eglot-ensure)
(add-hook 'elm-mode-hook 'eglot-ensure)
(add-hook 'haskell-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'web-mode-hook 'eglot-ensure)
(setq eglot-autoshutdown t)
(add-to-list 'eglot-server-programs '(nix-mode . ("nixd")))
(add-to-list 'eglot-server-programs '(elm-mode . ("elm-language-server")))
(add-to-list 'eglot-server-programs '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))

;; Company - completion framework
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 2
      company-show-quick-access t)

;; Flycheck - syntax checking
(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)

;; Magit - Git interface
(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

;; Projectile - project management
(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;;; Language Support

;; Nix
(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

;; Elm
(require 'elm-mode)
(add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))
(setq elm-format-on-save t)

;; Haskell
(require 'haskell-mode)
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-mode))
(setq haskell-process-type 'cabal-repl
      haskell-interactive-popup-errors nil)

;; Web-mode for HTML
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-enable-auto-pairing t
      web-mode-enable-css-colorization t)

;; JavaScript
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-basic-offset 2
      js2-bounce-indent-p t)

;; JSON
(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

;; Org Mode
(require 'org)
(setq org-startup-indented t
      org-hide-leading-stars t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-content-indentation 0
      org-todo-keywords '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED"))
      org-log-done 'note)

(require 'org-tempo)

;; Org-babel: Code execution in org files
(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (haskell . t)
   (nix . t)))

;; Org-babel settings
(setq org-confirm-babel-evaluate t      ; Prompt before executing code blocks (safer)
      org-src-preserve-indentation t    ; Preserve code block indentation
      haskell-process-type 'ghci)      ; haskell run without stack or cabal

;; Tree-sitter for better syntax highlighting
(require 'tree-sitter)
(global-tree-sitter-mode)

(require 'tree-sitter-langs)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;; Markdown mode (often useful for documentation)
(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(setq markdown-command "pandoc")

;; YAML mode
(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(require 'dired-quick-sort)
(setq dired-quick-sort-group-directories-last ?y
      dired-quick-sort-sort-by-last "version"
      dired-quick-sort-reverse-last ?n)
(dired-quick-sort-setup)

;;; Additional Keybindings

;; Better window navigation
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

;;; Final setup
(provide 'emacs)
;;; emacs.el ends here
