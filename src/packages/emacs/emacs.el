;;; emacs.el --- Emacs configuration for everything -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs configuration for everything
;; Configured to work with emacs-overlay and emacsWithPackagesFromUsePackage

;;; Code:

(setq-default
 indent-tabs-mode nil               ; Use spaces, not tabs
 tab-width 2                        ; 2-space indentation
 fill-column 80                     ; 80 character line width
 require-final-newline t)           ; Ensure files end with newline

(setq custom-file (concat user-emacs-directory "custom.el"))

(when (file-exists-p custom-file)
  (load custom-file))

(setq
 backup-by-copying t                ; Don't clobber symlinks
 delete-old-versions t              ; Clean up old backups
 kept-new-versions 6
 kept-old-versions 2
 version-control t                  ; Use version numbers for backups
 vc-follow-symlinks t)              ; Follow symlinks without asking

(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.5
      which-key-sort-order 'which-key-key-order-alpha)

(require 'vertico)
(vertico-mode)
(setq vertico-cycle t)

(require 'orderless)
(setq completion-styles '(orderless basic)
      completion-category-defaults nil
      completion-category-overrides '((file (styles partial-completion))))

(require 'marginalia)
(marginalia-mode)
(define-key minibuffer-local-map (kbd "M-A") 'marginalia-cycle)

(require 'consult)
(global-set-key (kbd "C-s") 'consult-line)
(global-set-key (kbd "C-x b") 'consult-buffer)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)
(global-set-key (kbd "M-g i") 'consult-imenu)
(global-set-key (kbd "M-g g") 'consult-goto-line)
(global-set-key (kbd "M-s g") 'consult-grep)
(global-set-key (kbd "M-s r") 'consult-ripgrep)
(setq consult-narrow-key "<")

(require 'helpful)
(global-set-key (kbd "C-h f") 'helpful-callable)
(global-set-key (kbd "C-h v") 'helpful-variable)
(global-set-key (kbd "C-h k") 'helpful-key)
(global-set-key (kbd "C-c C-d") 'helpful-at-point)
(global-set-key (kbd "C-h F") 'helpful-function)
(global-set-key (kbd "C-h C") 'helpful-command)

(require 'gptel)
(require 'mcp)
(require 'gptel-agent)
(require 'llm-tool-collection)
(require 'gptel-integrations)

(setq gptel-model 'gpt-4.1
      gptel-backend (gptel-make-gh-copilot "Copilot")
      gptel-use-tools t)

(setq mcp-hub-servers
      '(("fetch" . (:command "mcp-server-fetch"))
        ("memory" . (:command "mcp-server-memory"))
        ("sequential-thinking" . (:command "mcp-server-sequential-thinking"))
        ("time" . (:command "mcp-server-time"))
        ("git" . (:command "mcp-server-git"))
        ("github" . (:command "github-mcp-server http"))
        ("playwright" . (:command "playwright-mcp"))
        ("nixos" . (:command "mcp-nixos"))
        ("websearch" . (:command "open-websearch"))))

(mapcar (apply-partially #'apply #'gptel-make-tool)
        (llm-tool-collection-get-all))

(require 'avy)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g f") 'avy-goto-line)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "C-c C-j") 'avy-resume)
(setq avy-background t
      avy-style 'at-full)

(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)

(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

(require 'smartparens)
(require 'smartparens-config)
(add-hook 'prog-mode-hook 'smartparens-mode)
(setq sp-highlight-pair-overlay nil
      sp-highlight-wrap-overlay nil
      sp-highlight-wrap-tag-overlay nil)

(require 'undo-tree)
(global-undo-tree-mode)
(setq undo-tree-auto-save-history t
      undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

(require 'savehist)
(savehist-mode)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100)

(require 'eglot)
(add-hook 'nix-mode-hook 'eglot-ensure)
(add-hook 'elm-mode-hook 'eglot-ensure)
(add-hook 'haskell-ts-mode-hook 'eglot-ensure)
(add-hook 'js-mode-hook 'eglot-ensure)
(add-hook 'web-mode-hook 'eglot-ensure)
(add-hook 'nushell-mode-hook 'eglot-ensure)
(add-hook 'typst-ts-mode-hook 'eglot-ensure)
(setq eglot-autoshutdown t)
(add-to-list 'eglot-server-programs '(nix-mode . ("nixd")))
(add-to-list 'eglot-server-programs '(elm-mode . ("elm-language-server")))
(add-to-list 'eglot-server-programs '(haskell-ts-mode . ("haskell-language-server-wrapper" "--lsp")))
(add-to-list 'eglot-server-programs '(js-mode . ("typescript-language-server" "--stdio")))
(add-to-list 'eglot-server-programs '(nushell-mode . ("nu" "--lsp")))
(add-to-list 'eglot-server-programs '(typst-ts-mode . ("tinymist")))

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-idle-delay 0.2
      company-minimum-prefix-length 2
      company-show-quick-access t)

(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'projectile)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(add-to-list 'projectile-project-root-files "flake.nix")

(require 'nix-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-mode))

(require 'elm-mode)
(add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))
(setq elm-format-on-save t)

(require 'haskell-ts-mode)
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-ts-mode))

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(setq web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2
      web-mode-enable-auto-pairing t
      web-mode-enable-css-colorization t)

(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(setq js2-basic-offset 2
      js2-bounce-indent-p t)

(require 'json-mode)
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(setq markdown-command "pandoc")

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

(require 'typst-ts-mode)
(add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode))

(require 'org)
(setq org-startup-indented t
      org-hide-leading-stars t
      org-src-fontify-natively t
      org-src-tab-acts-natively t
      org-src-content-indentation 0
      org-todo-keywords '((sequence "TODO" "FEEDBACK" "VERIFY" "|" "DONE" "DELEGATED"))
      org-log-done 'note
      org-default-notes-file (concat org-directory "/notes.org"))
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-hook 'org-mode-hook 'org-indent-mode)
(add-hook 'org-mode-hook 'variable-pitch-mode)

(require 'org-modern)
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)

(require 'org-superstar)
(add-hook 'org-mode-hook #'org-superstar-mode)

(require 'org-tempo)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
   (python . t)
   (shell . t)
   (haskell . t)
   (nix . t)))

(setq org-confirm-babel-evaluate t      ; Prompt before executing code blocks (safer)
      org-src-preserve-indentation t    ; Preserve code block indentation
      haskell-process-type 'ghci)       ; haskell run without stack or cabal

(require 'tree-sitter)
(global-tree-sitter-mode)

(require 'tree-sitter-langs)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

(require 'dired-quick-sort)
(setq dired-quick-sort-group-directories-last ?y
      dired-quick-sort-sort-by-last "version"
      dired-quick-sort-reverse-last ?n)
(dired-quick-sort-setup)

(setq notmuch-crypto-process-mime t)
(autoload 'notmuch "notmuch" "Notmuch mail" t)

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(require 'emms-setup)
(emms-all)
(setq emms-player-list '(emms-player-mpv)
      emms-info-functions '(emms-info-native))

(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(column-number-mode t)
(show-paren-mode t)
(global-display-line-numbers-mode t)
(global-visual-line-mode t)
(add-to-list 'default-frame-alist '(alpha-background . 80))
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Inter" :height 140 :weight thin))))
 '(default ((t ( :family "FiraCode Nerd Font Mono" :height 120)))))

(require 'catppuccin-theme)
(setq catppuccin-flavor 'mocha)
(load-theme 'catppuccin :no-confirm)

(when (display-graphic-p)
  (require 'all-the-icons))

(when (display-graphic-p)
  (require 'all-the-icons-dired)
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(require 'doom-modeline)
(doom-modeline-mode 1)
(setq doom-modeline-height 25
      doom-modeline-bar-width 4
      doom-modeline-icon t
      doom-modeline-major-mode-icon t
      doom-modeline-major-mode-color-icon t
      doom-modeline-buffer-file-name-style 'truncate-upto-project
      doom-modeline-lsp t)

(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
(add-hook 'elm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'haskell-ts-mode-hook 'rainbow-delimiters-mode)

(setq image-type-auto-detectable t)
(setq telega-use-images t
      telega-emoji-font-family "Noto Color Emoji")
(auto-image-file-mode 1)
(require 'telega)
(add-hook 'telega-load-hook 'telega-notifications-mode)
(add-hook 'telega-load-hook 'telega-autoplay-mode)

(require 'transmission)

(require 'aggressive-indent)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

(require 'pass)

(require 'direnv)
(setq direnv-always-show-summary nil)
(direnv-mode)

(require 'xterm-color)

;;; function redeclaration

(with-eval-after-load 'nix-flake
  (defun nix-flake--installable-command (subcommand options flake-ref attribute
                                                    &optional extra-arguments)
    (let ((installable (if attribute
                           (concat (shell-quote-argument flake-ref) "#" attribute)
                         (shell-quote-argument flake-ref))))
      (concat nix-executable
              " "
              (mapconcat #'shell-quote-argument
                         (nix-flake--to-list subcommand)
                         " ")
              " " installable
              (if options (concat " " (mapconcat #'shell-quote-argument options " ")) "")
              (if extra-arguments (concat " -- " extra-arguments) "")))))

;;; custom commands

(defun emms-mus ()
  "Starts emms with music directory loaded."
  (interactive)
  (emms-play-directory-tree "~/mus/")
  (emms-shuffle)
  (emms-start))

;;; TODO: make this integrated with the json docs generated in pkgs

(require 'request)

(defun telega-last-screenshot ()
  "Attaches the last screenshot to telega chat."
  (interactive)
  (telega-chatbuf-attach-photo (car (last (directory-files "~/pic/ss/" t)))))

(defun wttr ()
  "Get the current weather."
  (interactive)
  (request "https://wttr.in/Chiang_Mai"
    :success (cl-function
              (lambda (&key data &allow-other-keys)
                (with-current-buffer (get-buffer-create "*wttr*")
                  (view-mode -1)
                  (erase-buffer)
                  (insert data)
                  (xterm-color-colorize-buffer)
                  (view-mode 1)
                  (pop-to-buffer (current-buffer))
                  )))))

(defun translate ()
  "Translate a region."
  (interactive)
  (let* ((text (buffer-substring-no-properties (region-beginning) (region-end)))
         (url "http://localhost:5000/translate"))
    (request url
      :method "POST"
      :data `(("source" . "auto") ("target" . "en") ("q" . ,text))
      :parser 'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  (with-current-buffer (get-buffer-create "*translate*")
                    (view-mode -1)
                    (erase-buffer)
                    (insert (assoc-default 'translatedText data))
                    (view-mode 1)
                    (pop-to-buffer (current-buffer))
                    ))))))


;;; Final setup
(provide 'emacs)
;;; emacs.el ends here
