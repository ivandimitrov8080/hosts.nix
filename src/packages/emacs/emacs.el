;;; emacs.el --- Emacs configuration for everything -*- lexical-binding: t; -*-

;;; Commentary:
;; Emacs configuration for everything.
;;
;; The configuration lives in domain-specific Emacs packages (see
;; src/packages/emacsPackages).  Each package ships the settings for its
;; domain and declares its own dependencies, so this file only loads them.

;;; Code:

(require 'pass-integration)
(require 'editor-defaults)
(require 'ui-chrome)
(require 'completion-ui)
(require 'structural-editing)
(require 'llm-integration)
(require 'code-intelligence)
(require 'language-support)
(require 'git-workflow)
(require 'org-workflow)
(require 'media-playback)
(require 'messaging)
(require 'rss-feeds)
(require 'nix-tooling)
(require 'utility-commands)

;;; Final setup
(provide 'emacs)
;;; emacs.el ends here
