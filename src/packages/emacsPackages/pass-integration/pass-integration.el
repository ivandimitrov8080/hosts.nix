;;; pass-integration.el --- Password store helpers -*- lexical-binding: t; -*-

;;; Commentary:
;; Runtime caching on top of `pass' plus comint/ERC friendly lookups.

;;; Code:

(require 'pass)

(defvar password-store-cache (make-hash-table :test #'equal)
  "Runtime cache of password-store entries.")

(defun password-store-get-cached (entry)
  "Return password for ENTRY, using a runtime cache first."
  (or (gethash entry password-store-cache)
      (puthash entry (password-store-get entry) password-store-cache)))

(defun comint-password-store-fun (prompt)
  "Return a password for PROMPT from the pass store, or nil to prompt normally.
PROMPT is the full text comint would otherwise show in the minibuffer."
  (password-store-get-cached
   (completing-read prompt (password-store-list) nil t)))

(defun pass-integration-init ()
  "Route comint password prompts through the pass store."
  (setq-default comint-password-function #'comint-password-store-fun))

(unless noninteractive
  (pass-integration-init))

(provide 'pass-integration)
;;; pass-integration.el ends here
