;;; vertico-nixos-options.el --- Vertico/Consult interface for nixos-options  -*- lexical-binding: t; -*-

;; A Vertico/Consult port of `ivy-nixos-options.el' from
;; https://github.com/nix-community/nix-emacs.

;; Keywords: unix
;; Version: 0.3.0
;; Package-Requires: ((emacs "27.1") (nixos-options "0.0.1") (consult "0.17"))

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;;; Commentary:

;; Browse the NixOS and Home Manager options with Vertico.  Parsing and
;; rendering are delegated to the `nixos-options' package; this file only adds
;; the `consult--read' front-end that previews documentation in a side window
;; and annotates every candidate with its type and a short description.

;;; Code:

(require 'subr-x)
(require 'json)
(require 'nixos-options)
(require 'consult)

(defgroup vertico-nixos-options nil
  "Vertico/Consult interface for browsing NixOS/Home Manager options."
  :group 'nixos-options)

(defcustom vertico-nixos-options-file nixos-options-json-file
  "`options.json' file holding the NixOS options."
  :type 'file :group 'vertico-nixos-options)

(defcustom vertico-nixos-options-home-manager-file nil
  "`options.json' file holding the Home Manager options, or nil to disable."
  :type '(choice (const :tag "Disabled" nil) file)
  :group 'vertico-nixos-options)

(defcustom vertico-nixos-options-default 'documentation
  "Default action after an option is selected: `documentation', `insert' or `description'."
  :type '(choice (const :tag "Show documentation" documentation)
                 (const :tag "Insert name" insert)
                 (const :tag "Show description" description))
  :group 'vertico-nixos-options)

(defvar vertico-nixos-options-history nil "Minibuffer history.")
(defvar vertico-nixos-options--cache (make-hash-table :test #'equal)
  "Cache of parsed databases, keyed by file name.")

(defun vertico-nixos-options--options (file)
  "Return the options in FILE, reusing the `nixos-options' database."
  (unless file (user-error "No options file configured"))
  (if (equal file nixos-options-json-file)
      nixos-options
    (or (gethash file vertico-nixos-options--cache)
        (puthash file (let ((json-key-type 'string))
                        (mapcar #'nixos-options--make-alist (json-read-file file)))
                 vertico-nixos-options--cache))))

(defun vertico-nixos-options--annotate (options name)
  "Annotate NAME looked up in the OPTIONS list."
  (when-let* ((opt (assoc name options))
              (desc (nixos-options-get-description opt)))
    (concat " "
            (propertize (or (nixos-options-get-type opt) "")
                        'face 'font-lock-type-face)
            " "
            (propertize (truncate-string-to-width
                         (replace-regexp-in-string "[ \t\n]+" " " desc)
                         60 nil nil "…")
                        'face 'completions-annotations))))

(defun vertico-nixos-options--state (options)
  "Return a Consult state function previewing documentation from OPTIONS."
  (lambda (action cand)
    (when (eq action 'preview)
      (if-let ((opt (and cand (assoc cand options))))
          (display-buffer
           (nixos-options-doc-buffer
            (nixos-options-get-documentation-for-option opt))
           '((display-buffer-in-side-window) (side . bottom) (window-height . 0.33)))
        (when-let ((win (get-buffer-window "*nixos-options-doc*")))
          (delete-window win))))))

(defun vertico-nixos-options--dispatch (options name insert)
  "Perform the default action on the option NAME found in OPTIONS.
With INSERT non-nil, insert the option name instead."
  (let ((opt (assoc name options)))
    (pcase (if insert 'insert vertico-nixos-options-default)
      ('insert (insert (nixos-options-get-name opt)))
      ('description (message "%s: %s"
                             (nixos-options-get-name opt)
                             (nixos-options-get-description opt)))
      (_ (pop-to-buffer
          (nixos-options-doc-buffer
           (nixos-options-get-documentation-for-option opt)))))))

(defun vertico-nixos-options--read (label options &optional insert)
  "Read an option from OPTIONS, prompting with LABEL; INSERT the name."
  (when-let ((selected
              (consult--read
               (mapcar #'car options)
               :prompt (format "%s options: " label)
               :history 'vertico-nixos-options-history
               :require-match t
               :sort nil
               :add-history (thing-at-point 'symbol t)
               :annotate (lambda (name)
                           (vertico-nixos-options--annotate options name))
               :state (vertico-nixos-options--state options))))
    (vertico-nixos-options--dispatch options selected insert)))

;;;###autoload
(defun vertico-nixos-options (&optional insert)
  "Browse the NixOS options with Vertico.
With a prefix argument, insert the selected option name instead."
  (interactive "P")
  (vertico-nixos-options--read
   "NixOS" (vertico-nixos-options--options vertico-nixos-options-file) insert))

;;;###autoload
(defun vertico-home-manager-options (&optional insert)
  "Browse the Home Manager options with Vertico.
With a prefix argument, insert the selected option name instead."
  (interactive "P")
  (vertico-nixos-options--read
   "Home Manager"
   (vertico-nixos-options--options vertico-nixos-options-home-manager-file)
   insert))

(provide 'vertico-nixos-options)
;;; vertico-nixos-options.el ends here
