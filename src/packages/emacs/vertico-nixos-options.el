;;; vertico-nixos-options.el --- A Vertico/Consult interface for nixos-options. -*- lexical-binding: t; -*-

;; A Vertico/Consult port of `ivy-nixos-options.el' from
;; https://github.com/nix-community/nix-emacs.

;; Keywords: unix
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (nixos-options "0.0.1") (consult "0.17"))

;; This file is not part of GNU Emacs.

;;; License: GPLv3

;;; Commentary:

;; Browse NixOS and Home Manager options with Vertico.  Vertico has no
;; multi-action engine of its own, so this builds on Consult's `consult--read':
;; the selected option's documentation is previewed in a side window and every
;; candidate is annotated with its type and a short description.
;;
;; Two ways of working with several option databases are supported:
;;
;; 1. Merged: `vertico-nix-options-all' browses every configured source at
;;    once.  Home Manager options are namespaced under `home-manager.' via the
;;    source's :prefix, mirroring how `home-manager.users.<name>' sits below
;;    the NixOS options.
;;
;; 2. Separate: each source points at its own `options.json' file and can be
;;    browsed at runtime with `vertico-nixos-options' /
;;    `vertico-home-manager-options', or with `vertico-nix-options' which
;;    prompts for the source (or an arbitrary JSON file) to use.
;;
;; Unlike the upstream `ivy-nixos-options', this adapter does not rely on the
;; global `nixos-options' database that the `nixos-options' package populates at
;; load time.  It parses the JSON files itself (with caching) so several
;; databases can be switched between at runtime.

;;; Code:

(require 'subr-x)
(require 'json)
(require 'nixos-options)
(require 'consult)

(defgroup vertico-nixos-options nil
  "Vertico/Consult interface for browsing NixOS/Home Manager options."
  :group 'nixos-options)

(defcustom vertico-nixos-options-sources nil
  "Alist of option databases to browse.

Each element is (KEY . PLIST) where KEY is a string naming the source and
PLIST recognises the following keywords:

  :label   Human readable name shown in the prompt.
  :file    Path to an `options.json' file.
  :prefix  String prepended to every option name.  Use \"\" to keep the
           names untouched or, for example, \"home-manager.\" to namespace
           Home Manager options below the NixOS `home-manager' options.

When several sources are browsed together (see `vertico-nix-options-all')
the union of their options is shown, so give clashing sources a distinct
:PREFIX.  If this is left nil the adapter falls back to a single source
derived from `nixos-options-json-file'."
  :type '(alist :key-type string :value-type plist)
  :group 'vertico-nixos-options)

(defcustom vertico-nixos-options-nixos-source "nixos"
  "Key of the source used by `vertico-nixos-options'."
  :type 'string
  :group 'vertico-nixos-options)

(defcustom vertico-nixos-options-home-manager-source "home-manager"
  "Key of the source used by `vertico-home-manager-options'."
  :type 'string
  :group 'vertico-nixos-options)

(defcustom vertico-nixos-options-default 'documentation
  "Default action performed by the browser after an option is selected.
One of `documentation', `insert' or `description'."
  :type '(choice (const :tag "Show documentation" documentation)
                 (const :tag "Insert name" insert)
                 (const :tag "Show description" description))
  :group 'vertico-nixos-options)

(defvar vertico-nixos-options-history nil
  "Minibuffer history for the option browsers.")

(defvar vertico-nixos-options--cache (make-hash-table :test #'equal)
  "Cache of parsed option databases, keyed by source key or file name.")

(defconst vertico-nixos-options--doc-buffer "*nixos-options-doc*"
  "Name of the buffer `nixos-options-doc-buffer' creates for previews.")

;;; Sources

(defun vertico-nixos-options--effective-sources ()
  "Return the configured sources, or a sensible default when none are set."
  (or vertico-nixos-options-sources
      (when-let ((file (and (boundp 'nixos-options-json-file)
                            nixos-options-json-file)))
        (list (list "nixos" :label "NixOS" :file file :prefix "")))))

(defun vertico-nixos-options--source-plist (key)
  "Return the plist describing the source KEY, signalling an error otherwise."
  (or (cdr (assoc key (vertico-nixos-options--effective-sources)))
      (error "Unknown option source `%s'" key)))

(defun vertico-nixos-options--sources ()
  "Return the list of configured source keys."
  (mapcar #'car (vertico-nixos-options--effective-sources)))

;;; Parsing

(defun vertico-nixos-options--json-value->string (value)
  "Return a printable representation of the JSON VALUE.
Bare booleans become \"true\"/\"false\"; everything else is returned
unchanged."
  (cond ((eq value t) "true")
        ((eq value :json-false) "false")
        (t value)))

(defun vertico-nixos-options--make-option (entry prefix)
  "Turn the JSON ENTRY into an option alist, prefixing its name with PREFIX.
ENTRY is a (NAME . DATA) cons as returned by `json-read-file'.  The result
has the shape the `nixos-options' accessors expect."
  (let* ((name (concat prefix (car entry)))
         (data (cdr entry)))
    (dolist (field '("default" "example"))
      (when-let ((cell (assoc field data)))
        (setcdr cell (vertico-nixos-options--json-value->string (cdr cell)))))
    (push (cons "name" name) data)
    (cons name data)))

(defun vertico-nixos-options--parse-file (file prefix)
  "Parse the `options.json' FILE, prepending PREFIX to every option name."
  (let* ((json-key-type 'string)
         (raw (json-read-file file)))
    (mapcar (lambda (entry) (vertico-nixos-options--make-option entry prefix))
            raw)))

;;; Dataset handling

(defun vertico-nixos-options--source (key)
  "Return the parsed option list for the source KEY, loading it if needed."
  (or (gethash key vertico-nixos-options--cache)
      (let* ((plist (vertico-nixos-options--source-plist key))
             (file (plist-get plist :file))
             (prefix (or (plist-get plist :prefix) "")))
        (unless (file-readable-p file)
          (error "Cannot read options file `%s' for source `%s'" file key))
        (puthash key (vertico-nixos-options--parse-file file prefix)
                 vertico-nixos-options--cache))))

(defun vertico-nixos-options--all ()
  "Return the merged option list of every configured source."
  (apply #'append
         (mapcar #'vertico-nixos-options--source
                 (vertico-nixos-options--sources))))

(defun vertico-nixos-options--parse-cached (key file &optional prefix)
  "Parse FILE with PREFIX, caching the result under KEY."
  (or (gethash key vertico-nixos-options--cache)
      (puthash key (vertico-nixos-options--parse-file file (or prefix ""))
               vertico-nixos-options--cache)))

(defun vertico-nixos-options--dataset (source)
  "Resolve SOURCE into a plist with :options and :label.
SOURCE may be:

  * a key in `vertico-nixos-options-sources',
  * the string \"all\", merging every configured source, or
  * a path to an `options.json' file, browsed as-is."
  (cond
   ((equal source "all")
    (list :options (vertico-nixos-options--all) :label "All"))
   ((assoc source (vertico-nixos-options--effective-sources))
    (list :options (vertico-nixos-options--source source)
          :label (or (plist-get (vertico-nixos-options--source-plist source) :label)
                     source)))
   ((and (stringp source) (file-readable-p source))
    (list :options (vertico-nixos-options--parse-cached source source)
          :label (file-name-base source)))
   (t (user-error "Unknown option source `%s'" source))))

(defun vertico-nixos-options-refresh ()
  "Forget every cached option database."
  (interactive)
  (setq vertico-nixos-options--cache (make-hash-table :test #'equal)))

;;; Display helpers

(defun vertico-nixos-options--annotate (options name)
  "Annotate the option NAME looked up in the OPTIONS dataset."
  (when-let* ((opt (assoc name options))
              (desc (nixos-options-get-description opt)))
    (concat " "
            (propertize (or (nixos-options-get-type opt) "")
                        'face 'font-lock-type-face)
            " "
            (propertize
             (truncate-string-to-width
              (replace-regexp-in-string "[ \t\n]+" " " desc) 60 nil nil "…")
             'face 'completions-annotations))))

(defun vertico-nixos-options--kill-preview-window ()
  "Delete the documentation preview window if it is visible."
  (when-let ((win (get-buffer-window
                   (get-buffer vertico-nixos-options--doc-buffer))))
    (delete-window win)))

(defun vertico-nixos-options--make-state (options)
  "Return a Consult state function previewing documentation taken from OPTIONS."
  (lambda (action cand)
    (when (eq action 'preview)
      (if-let ((opt (and cand (assoc cand options))))
          (display-buffer
           (nixos-options-doc-buffer
            (nixos-options-get-documentation-for-option opt))
           '((display-buffer-in-side-window) (side . bottom) (window-height . 0.33)))
        (vertico-nixos-options--kill-preview-window)))))

(defun vertico-nixos-options--dispatch (options name action)
  "Perform ACTION on the option NAME found in the OPTIONS dataset."
  (let ((opt (assoc name options)))
    (pcase (or action vertico-nixos-options-default)
      ('insert (insert (nixos-options-get-name opt)))
      ('description (message "%s: %s"
                             (nixos-options-get-name opt)
                             (nixos-options-get-description opt)))
      (_ (pop-to-buffer
          (nixos-options-doc-buffer
           (nixos-options-get-documentation-for-option opt)))))))

(defun vertico-nixos-options--read (options label &optional insert)
  "Read an option from OPTIONS, prompting with LABEL.
With INSERT non-nil insert the option name instead of performing the
default action."
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
               :state (vertico-nixos-options--make-state options))))
    (vertico-nixos-options--dispatch
     options selected (if insert 'insert vertico-nixos-options-default))))

;;; Commands

(defun vertico-nixos-options--choose-source ()
  "Prompt for the source to browse.
Offers every configured source plus the merged \"all\" pseudo-source, and
accepts an arbitrary `options.json' file path."
  (let ((choices (append (vertico-nixos-options--sources) (list "all"))))
    (completing-read "Option source: " choices nil nil nil nil (car choices))))

;;;###autoload
(defun vertico-nix-options (source &optional insert)
  "Browse the options of SOURCE with Vertico.

SOURCE is a key in `vertico-nixos-options-sources', the string \"all\" to
browse every configured source merged together, or a path to an
`options.json' file.  When called interactively, prompt for the source.

With a prefix argument INSERT, insert the selected option name at point
instead of performing the default action."
  (interactive (list (vertico-nixos-options--choose-source) current-prefix-arg))
  (let* ((dataset (vertico-nixos-options--dataset source))
         (options (plist-get dataset :options)))
    (vertico-nixos-options--read options (plist-get dataset :label) insert)))

;;;###autoload
(defun vertico-nixos-options (&optional insert)
  "Browse the NixOS options with Vertico.
With a prefix argument INSERT, insert the option name at point instead of
performing the default action."
  (interactive "P")
  (vertico-nix-options vertico-nixos-options-nixos-source insert))

;;;###autoload
(defun vertico-home-manager-options (&optional insert)
  "Browse the Home Manager options with Vertico.
With a prefix argument INSERT, insert the option name at point instead of
performing the default action."
  (interactive "P")
  (vertico-nix-options vertico-nixos-options-home-manager-source insert))

;;;###autoload
(defun vertico-nix-options-all (&optional insert)
  "Browse every configured option source merged with Vertico.
Home Manager options are namespaced under `home-manager.' as configured by
the source's :prefix."
  (interactive "P")
  (vertico-nix-options "all" insert))

(provide 'vertico-nixos-options)
;;; vertico-nixos-options.el ends here
