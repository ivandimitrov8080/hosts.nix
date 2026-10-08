;;; emacs.el --- Emacs configuration for everything -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs configuration for everything
;; Configured to work with emacs-overlay and emacsWithPackagesFromUsePackage

;;; Code:

(setq user-emacs-directory "~/.config/emacs/")

(defun comint-password-store-fun (prompt)
  "Return a password for PROMPT from the pass store, or nil to prompt normally.
PROMPT is the full text comint would otherwise show in the minibuffer."
  (cond
   ((string-match-p "sudo" prompt)
    (password-store-get
     (completing-read prompt (password-store-list) nil t)))
   ;; add more mappings as needed
   (t nil)))

(setq-default
 indent-tabs-mode nil
 tab-width 2
 fill-column 80
 require-final-newline t
 comint-password-function #'comint-password-store-fun)

(setq custom-file (concat user-emacs-directory "custom.el"))

(when (file-exists-p custom-file)
  (load custom-file))

(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 vc-follow-symlinks t)

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
(require 'gptel-integrations)
(setq gptel-log-level 'info)
(gptel-make-gh-copilot "Copilot")
(add-hook 'gptel-mode-hook 'company-mode)


(setq gptel-backend (gptel-make-deepseek "Deepseek"
                      :stream t
                      :key (lambda () (password-store-get "dev/deepseek.com/key")))
      gptel-model 'deepseek-v4-flash
      gptel-use-tools t)

(setq mcp-hub-servers
      '(("fetch" . (:command "mcp-server-fetch"))
        ("filesystem" . (:command "mcp-server-filesystem" :roots ("~/src/hosts.nix")))
        ("time" . (:command "mcp-server-time"))
        ("git" . (:command "mcp-server-git"))
        ("websearch" . (
                        :command "open-websearch"
                        :env (
                              :DEFAULT_SEARCH_ENGINE "duckduckgo"
                              :ALLOWED_SEARCH_ENGINES "duckduckgo,brave")))))

(add-hook 'after-init-hook
          (lambda ()
            (gptel-mcp-connect '("fetch" "websearch"))))

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
(setq undo-tree-auto-save-history t)

(require 'savehist)
(savehist-mode)

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-saved-items 100)

(require 'company)
(add-hook 'prog-mode-hook 'company-mode)
(setq company-idle-delay 0.3
      company-minimum-prefix-length 2
      company-tooltip-align-annotations t
      company-tooltip-flip-when-above t
      company-selection-wrap-around t
      company-show-quick-access t
      company-show-quick-access t)

(defun company-dictionary-mode ()
  "Use 'company-ispell' only, with a delay of 1s."
  (setq-local company-idle-delay 1
              company-minimum-prefix-length 4
              company-backends '(company-ispell))
  (company-mode 1))

(add-hook 'message-mode-hook #'company-dictionary-mode)

(require 'flycheck)
(add-hook 'after-init-hook 'global-flycheck-mode)
(add-hook 'after-init-hook 'global-flycheck-annotate-mode)
(global-flycheck-eglot-mode)
(setq ispell-dictionary "en_GB"
      ispell-program-name "aspell"
      ispell-alternate-dictionary (getenv "WORDLIST")
      ispell-silently-savep t)
(require 'flycheck-aspell)
(add-to-list 'flycheck-checkers 'mail-aspell-dynamic)

(require 'magit)
(global-set-key (kbd "C-x g") 'magit-status)

(require 'projectile)
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

(require 'nix-mode)
(require 'nix-ts-mode)
(add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))

(require 'rust-mode)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

(require 'elm-mode)
(add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))
(setq elm-format-on-save t)

(require 'haskell-ts-mode)
(add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-ts-mode))
(setq haskell-ts-prettify-symbols t
      haskell-ts-prettify-words t)
(add-hook 'haskell-ts-mode-hook 'prettify-symbols-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . html-ts-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
(add-hook 'html-ts-mode-hook 'company-mode)

(require 'nushell-mode)
(add-to-list 'auto-mode-alist '("\\.nu\\'" . nushell-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

(require 'markdown-mode)
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

(require 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

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
      org-default-notes-file (concat org-directory "/notes.org")
      org-hide-emphasis-markers t
      org-pretty-entities t
      org-pretty-entities-include-sub-superscripts t
      org-ellipsis "  ...")
(add-to-list 'org-agenda-files (concat org-directory "/notes.org"))
(require 'org-capture)
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

(require 'org-modern)
(add-hook 'org-mode-hook #'org-modern-mode)
(add-hook 'org-agenda-finalize-hook #'org-modern-agenda)
(setq org-modern-star 'replace
      org-modern-hide-stars 'leading
      org-modern-todo t
      org-modern-tag t
      org-modern-priority t
      org-modern-keyword t                     ; #+TITLE: rendered as a keyword pill
      org-modern-block-name nil
      org-modern-list '((43 . "•") (45 . "–") (42 . "•"))
      org-modern-checkbox '((?X . "☑") (?- . "◐") (?\s . "☐"))
      org-modern-horizontal-rule "─")
(require 'org-appear)
(add-hook 'org-mode-hook #'org-appear-mode)

(require 'org-tempo)
(require 'olivetti)
(add-hook 'org-mode-hook #'olivetti-mode)

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
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)
(setq treesit-font-lock-level 4)

(require 'dired-quick-sort)
(setq dired-quick-sort-group-directories-last ?y
      dired-quick-sort-sort-by-last "version"
      dired-quick-sort-reverse-last ?n)
(dired-quick-sort-setup)

(setq notmuch-crypto-process-mime t
      notmuch-hello-auto-refresh t
      notmuch-saved-searches '((:name "inbox" :query "tag:inbox" :key "i")
                               (:name "unread" :query "tag:unread" :key "u")))
(setq mail-interactive t
      send-mail-function 'sendmail-send-it
      sendmail-program "msmtp"
      user-mail-address "ivan@idimitrov.dev"
      user-full-name "Ivan Kirilov Dimitrov"
      shr-use-colors nil
      shr-blocked-images ".*"
      shr-inhibit-images t)
(autoload 'notmuch "notmuch" "Notmuch mail" t)
(add-hook 'notmuch-mua-send-hook #'mml-secure-message-sign-pgpmime)
(with-eval-after-load 'notmuch
  (setq notmuch-search-history nil))
(add-hook 'notmuch-message-mode-hook 'message-mode)

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)

(require 'emms-setup)
(emms-all)
(setq emms-player-list '(emms-player-mpv)
      emms-info-functions '(emms-info-exiftool emms-info-native)
      emms-browser-covers #'emms-browser-cache-thumbnail-async
      emms-browser-thumbnail-small-size 64
      emms-browser-thumbnail-medium-size 128
      emms-track-description-function #'emms-info-track-description)

(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(column-number-mode t)
(show-paren-mode t)
(global-visual-line-mode t)
(add-to-list 'default-frame-alist '(alpha-background . 80))
(custom-theme-set-faces
 'user
 '(variable-pitch ((t (:family "Inter" :height 140 :weight thin))))
 '(fixed-pitch ((t ( :family "FiraCode Nerd Font Mono" :height 120)))))

(require 'catppuccin-theme)
(setq catppuccin-flavor 'mocha)
(load-theme 'catppuccin :no-confirm)


(require 'nerd-icons)
(require 'nerd-icons-xref)
(require 'nerd-icons-dired)
(require 'nerd-icons-completion)
(require 'nerd-icons-ibuffer)
(nerd-icons-xref-mode)
(add-hook 'dired-mode-hook 'nerd-icons-dired-mode)
(add-hook 'ibuffer-mode-hook 'nerd-icons-ibuffer-mode)
(nerd-icons-completion-mode)

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

(require 'erc)
(add-to-list 'erc-modules 'sasl)
(erc-update-modules)

(require 'erc-sasl)
(setq erc-sasl-user "ivand"
      erc-sasl-auth-source-function (lambda (&rest _) (password-store-get "soc/nickserv/erc")))

(setq telega-use-images t
      telega-emoji-font-family (font-spec :family "Noto Color Emoji")
      telega-emoji-use-images nil
      telega-chat-input-markups '("markdown2" "org" nil)
      telega-msg-save-dir "~/dl/telega")
(auto-image-file-mode 1)
(add-hook 'telega-load-hook 'telega-notifications-mode)
(add-hook 'telega-load-hook 'telega-autoplay-mode)
(add-hook 'telega-chat-mode-hook 'telega-completions-setup-capf)
(add-hook 'telega-chat-mode-hook #'company-dictionary-mode)

(require 'telega)

(require 'transmission)

(require 'aggressive-indent)
(add-hook 'emacs-lisp-mode-hook #'aggressive-indent-mode)

(require 'pass)

(require 'direnv)
(setq direnv-always-show-summary nil)
(direnv-mode)

(require 'xterm-color)

(require 'khalel)
(setq khalel-import-org-file (concat org-directory "/calendar.org"))
(add-to-list 'org-agenda-files (concat org-directory "/calendar.org"))

(require 'elfeed)
(setq elfeed-feeds
      '(("https://rss.arxiv.org/atom/cs.AI" cs ai)
        ("https://rss.arxiv.org/atom/cs.CE" cs eng fin sci)
        ("https://rss.arxiv.org/atom/cs.CY" cs soc)
        ("https://www.technologyreview.com/topic/artificial-intelligence/feed" cs ai)
        ("https://cvefeed.io/rssfeed/severity/high.atom" cs cve)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCODHrzPMGbNv67e84WDZhQQ" yt fern)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC7YOGHUfC1Tb6E4pudI9STA" yt mentaloutlaw)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCKTwTio3tOv713CnIcxyLhA" yt dungeonsoup)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCYO_jab_esuFRV4b17AJtAw" yt 3b1b)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCsXVk37bltHxD1rDPwtNM8Q" yt kurzgesagt)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UC1D3yD4wlPMico0dss264XA" yt nileblue)
        ("https://www.youtube.com/feeds/videos.xml?channel_id=UCFhXFikryT4aFcLkLw2LBLA" yt nilered)
        ))
(add-hook 'elfeed-new-entry-hook
          (elfeed-make-tagger :before "2 weeks ago"
                              :remove 'unread))

(defun browse-url-emms (url &rest _args)
  "Automatically open URL in REST mpv."
  (emms-play-url url))

(setq browse-url-handlers
      `(("youtube\\.com/watch\\?v=.*" . browse-url-emms)
        ("youtube\\.com/shorts/.*" . browse-url-emms)))

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
  "Loads emms with music directory."
  (interactive)
  (emms-playlist-current-clear)
  (emms-add-directory-tree "~/mus/")
  (emms-shuffle)
  (emms-start))

(defun emms-shows ()
  "Loads emms with shows directory."
  (interactive)
  (emms-playlist-current-clear)
  (emms-add-directory-tree "~/shows/"))

(setq nixos-options-json-file (getenv "OPTIONS_JSON_NIXOS"))
(require 'nixos-options)
(require 'vertico-nixos-options)
(setq vertico-nixos-options-home-manager-file (getenv "OPTIONS_JSON_HOME_MANAGER"))
(global-set-key (kbd "C-c n") 'vertico-nixos-options)
(global-set-key (kbd "C-c h") 'vertico-home-manager-options)

(require 'request)

(defun wttr ()
  "Get the current weather."
  (interactive)
  (request "https://wttr.in/Da_Nang"
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

(defun rebuild-nova ()
  "Rebuild nova."
  (interactive)
  (async-shell-command "nixos-rebuild switch --flake ~/src/hosts.nix#nova --profile-name nova --sudo --ask-sudo-password"))

(defun rebuild-vps ()
  "Rebuild vpsfree-ivand."
  (interactive)
  (async-shell-command "ssh vpsfree-ivand 'cd ~/src/hosts.nix; git pull; nixos-rebuild switch --flake ./#vps --sudo --ask-sudo-password'"))

(require 'eglot)
(add-hook 'nix-ts-mode-hook 'eglot-ensure)
(add-hook 'elm-mode-hook 'eglot-ensure)
(add-hook 'haskell-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'css-ts-mode-hook 'eglot-ensure)
(add-hook 'html-ts-mode-hook 'eglot-ensure)
(add-hook 'nushell-mode-hook 'eglot-ensure)
(add-hook 'typst-ts-mode-hook 'eglot-ensure)
(add-hook 'rust-ts-mode-hook 'eglot-ensure)
(setq eglot-autoshutdown t)
(add-to-list 'eglot-server-programs '(nix-ts-mode . ("nixd")))
(add-to-list 'eglot-server-programs '(elm-mode . ("elm-language-server")))
(add-to-list 'eglot-server-programs '(haskell-ts-mode . ("haskell-language-server-wrapper" "--lsp")))
(add-to-list 'eglot-server-programs '(js-ts-mode . ("typescript-language-server" "--stdio")))
(add-to-list 'eglot-server-programs '(html-ts-mode . ("vscode-html-languageserver" "--stdio")))
(add-to-list 'eglot-server-programs '(css-ts-mode . ("vscode-css-languageserver" "--stdio")))
(add-to-list 'eglot-server-programs '(nushell-mode . ("nu" "--lsp")))
(add-to-list 'eglot-server-programs '(typst-ts-mode . ("tinymist")))
(add-to-list 'eglot-server-programs '(rust-ts-mode . ("rust-analyzer")))

;;; Final setup
(provide 'emacs)
;;; emacs.el ends here
