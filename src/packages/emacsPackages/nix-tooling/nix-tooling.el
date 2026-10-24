;;; nix-tooling.el --- Nix, NixOS and Home Manager tooling -*- lexical-binding: t; -*-

;;; Commentary:
;; NixOS/Home Manager option browsing and direnv integration.

;;; Code:

(require 'direnv)
(require 'nixos-options)
(require 'vertico-nixos-options)

(defun nix-tooling-init ()
  "Configure Nix-specific helpers."
  (setq vertico-nixos-options-file (getenv "OPTIONS_JSON_NIXOS"))
  (setq vertico-nixos-options-home-manager-file (getenv "OPTIONS_JSON_HOME_MANAGER"))
  (global-set-key (kbd "C-c n") 'vertico-nixos-options)
  (global-set-key (kbd "C-c h") 'vertico-home-manager-options)

  (setq direnv-always-show-summary nil)
  (direnv-mode))

(unless noninteractive
  (nix-tooling-init))

(provide 'nix-tooling)
;;; nix-tooling.el ends here
