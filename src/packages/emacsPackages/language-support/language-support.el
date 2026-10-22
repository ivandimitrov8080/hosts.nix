;;; language-support.el --- Language modes and LSP wiring -*- lexical-binding: t; -*-

;;; Commentary:
;; Tree-sitter and language-specific modes plus their Eglot servers.

;;; Code:

(require 'nix-mode)
(require 'nix-ts-mode)
(require 'rust-mode)
(require 'elm-mode)
(require 'haskell-ts-mode)
(require 'nushell-mode)
(require 'typst-ts-mode)
(require 'markdown-mode)
(require 'yaml-mode)
(require 'eglot)
(require 'company)

(defun language-support-init ()
  "Configure language modes and their Eglot servers."
  (add-to-list 'auto-mode-alist '("\\.drv\\'" . nix-drv-mode))
  (add-to-list 'auto-mode-alist '("\\.nix\\'" . nix-ts-mode))

  (add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-ts-mode))

  (add-to-list 'auto-mode-alist '("\\.elm\\'" . elm-mode))
  (setq elm-format-on-save t)

  (add-to-list 'auto-mode-alist '("\\.hs\\'" . haskell-ts-mode))
  (setq haskell-ts-prettify-symbols t
        haskell-ts-prettify-words t)
  (add-hook 'haskell-ts-mode-hook 'prettify-symbols-mode)

  (add-to-list 'auto-mode-alist '("\\.html?\\'" . html-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.py\\'" . python-ts-mode))
  (add-hook 'html-ts-mode-hook 'company-mode)

  (add-to-list 'auto-mode-alist '("\\.nu\\'" . nushell-mode))

  (add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.json\\'" . json-ts-mode))

  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

  (add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-ts-mode))

  (add-to-list 'auto-mode-alist '("\\.typ\\'" . typst-ts-mode))

  (add-hook 'nix-ts-mode-hook 'eglot-ensure)
  (add-hook 'elm-mode-hook 'eglot-ensure)
  (add-hook 'haskell-ts-mode-hook 'eglot-ensure)
  (add-hook 'js-ts-mode-hook 'eglot-ensure)
  (add-hook 'css-ts-mode-hook 'eglot-ensure)
  (add-hook 'html-ts-mode-hook 'eglot-ensure)
  (add-hook 'nushell-mode-hook 'eglot-ensure)
  (add-hook 'typst-ts-mode-hook 'eglot-ensure)
  (add-hook 'rust-ts-mode-hook 'eglot-ensure)
  (add-hook 'python-ts-mode-hook 'eglot-ensure)
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
  (add-to-list 'eglot-server-programs '(python-ts-mode . ("pylsp"))))

(unless noninteractive
  (language-support-init))

(provide 'language-support)
;;; language-support.el ends here
