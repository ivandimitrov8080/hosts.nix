;;; hm-options.el --- Magit-style browser for home-manager options -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Ivan Kirilov Dimitrov

;; Author: Ivan Kirilov Dimitrov
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (magit-section "4.0.0") (transient "0.4.0"))
;; Keywords: nix, tools, docs
;; URL: https://github.com/ivandimitrov8080/hosts.nix

;;; Commentary:

;; A magit-style, collapsible browser for the option database that
;; home-manager renders as `share/doc/home-manager/options.json'.
;; NixOS ships the same format (`share/doc/nixos/options.json'), so it
;; works there too -- see `hm-options-file'.
;;
;;     M-x hm-options
;;
;; The options are shown as a tree of attribute paths.  Every node that
;; corresponds to a real option can be expanded (TAB) to reveal its
;; description, and RET opens the full documentation, with the modules
;; that declare it, in a separate buffer.
;;
;; The only dependencies are `magit-section' and `transient', both of
;; which come with Magit.

;;; Code:

(require 'magit-section)
(require 'transient)
(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'browse-url)
(require 'button)

(defgroup hm-options nil
  "Browse the home-manager option database."
  :group 'nix
  :prefix "hm-options-")

(defcustom hm-options-file nil
  "The options.json file to read.
When nil, the file is searched for relative to the project root and in
the usual Nix profile locations; see `hm-options--locate-file'."
  :type '(choice (const :tag "Auto-detect" nil) file))

(defcustom hm-options-search-roots
  '(project nix-profile user-profile system-profile)
  "Places searched by `hm-options--locate-file'.
The elements `project', `nix-profile', `user-profile' and
`system-profile' stand for the directory of the current project, the
user's `~/.nix-profile', `/etc/profiles/per-user/$USER' and
`/run/current-system/sw' respectively."
  :type '(repeat symbol))

(defcustom hm-options-relative-files
  '("result/share/doc/home-manager/options.json"
    "share/doc/home-manager/options.json"
    "result/share/doc/nixos/options.json"
    "share/doc/nixos/options.json")
  "Candidate paths, relative to each of `hm-options-search-roots'."
  :type '(repeat string))

(defcustom hm-options-show-summary t
  "Whether to show a one-line summary of the option in its heading."
  :type 'boolean)

(defcustom hm-options-summary-width 60
  "Truncate summaries shown in headings to this many characters."
  :type 'natural)

;;; Data ---------------------------------------------------------------------

(cl-defstruct (hm-options--node (:constructor hm-options--make-node))
  (children (make-hash-table :test #'equal) :read-only t)
  entry)

(defvar hm-options--entries nil
  "Hash table of option name -> parsed JSON object.")
(defvar hm-options--names nil
  "Sorted list of all option names.")
(defvar hm-options--tree nil
  "Root `hm-options--node' of the option tree.")
(defvar hm-options--source nil
  "File the current data was read from.")
(defvar hm-options--origin-buffer nil
  "Buffer that `hm-options' was invoked from.")

(defvar-local hm-options--sections nil
  "Hash table of option name -> the section displaying it.")
(defvar-local hm-options--view nil
  "How the current buffer was rendered: `tree' or (list . REGEXP).")
(defvar-local hm-options--option nil
  "The option described by the current `hm-options-info-mode' buffer.")

(defun hm-options--field (entry key)
  "Return KEY from the JSON object ENTRY."
  (and entry (gethash key entry)))

(defun hm-options--literal (entry key)
  "Return the expression stored at KEY of ENTRY as a string."
  (when-let* ((value (hm-options--field entry key)))
    (if (and (hash-table-p value)
             (equal (gethash "_type" value) "literalExpression"))
        (gethash "text" value)
      (format "%s" (if (hash-table-p value) (gethash "text" value) value)))))

(defun hm-options--clean-description (text)
  "Strip the MyST roles that home-manager embeds in descriptions."
  (when (and text (not (equal text "")))
    (string-trim
     (replace-regexp-in-string
      "{[a-z-]+}`\\([^`]*\\)`" "\\1"
      (replace-regexp-in-string
       "\\[\\([^]]*\\)\\](\\(https?://[^)]*\\))" "\\1 (\\2)" text)))))

;;; Loading ------------------------------------------------------------------

(defun hm-options--search-directories ()
  "Return the list of directories `hm-options--locate-file' looks in."
  (delq nil
        (mapcar
         (lambda (root)
           (pcase root
             ('project
              (or (and (fboundp 'projectile-project-root)
                       (ignore-errors (projectile-project-root)))
                  (locate-dominating-file default-directory "flake.nix")
                  (locate-dominating-file default-directory ".git")
                  default-directory))
             ('nix-profile (expand-file-name "~/.nix-profile"))
             ('user-profile (format "/etc/profiles/per-user/%s" (user-login-name)))
             ('system-profile "/run/current-system/sw")
             ((pred stringp) root)
             (_ (expand-file-name (format "%s" root)))))
         hm-options-search-roots)))

(defun hm-options--locate-file ()
  "Return the path of an options.json, or nil if none was found."
  (cond
   ((and hm-options-file (file-readable-p hm-options-file)) hm-options-file)
   (t (cl-loop for dir in (hm-options--search-directories)
               thereis (cl-loop for rel in hm-options-relative-files
                                for file = (expand-file-name rel dir)
                                when (file-readable-p file) return file)))))

(defun hm-options--read-file (&optional file)
  "Parse the options file and cache the result.
FILE overrides `hm-options-file' and the automatic search."
  (setq file (or file
                 (hm-options--locate-file)
                 (read-file-name "options.json: " nil nil t
                                 (expand-file-name
                                  "result/share/doc/home-manager/options.json"))))
  (let ((t0 (float-time)))
    (unless (and hm-options--entries (equal file hm-options--source))
      (message "hm-options: reading %s..." file)
      (with-temp-buffer
        (insert-file-contents file)
        (setq hm-options--entries
              (json-parse-buffer :object-type 'hash-table
                                 :array-type 'list
                                 ;; JSON `false' and `null' both become nil;
                                 ;; the default `:false' object is truthy.
                                 :false-object nil
                                 :null-object nil)))
      (setq hm-options--names (sort (hash-table-keys hm-options--entries) #'string<)
            hm-options--tree (hm-options--build-tree hm-options--entries)
            hm-options--source file)
      (message "hm-options: %d options in %.2fs"
               (hash-table-count hm-options--entries)
               (- (float-time) t0)))))

(defun hm-options--build-tree (entries)
  "Build the prefix tree of option names from ENTRIES."
  (let ((root (hm-options--make-node)))
    (maphash
     (lambda (name entry)
       (let ((node root))
         (dolist (segment (split-string name "\\." t))
           (setq node (or (gethash segment (hm-options--node-children node))
                          (puthash segment (hm-options--make-node)
                                   (hm-options--node-children node)))))
         (setf (hm-options--node-entry node) entry)))
     entries)
    root))

;;; Rendering ----------------------------------------------------------------

(defun hm-options--insert-node (path segment node)
  "Insert the option tree rooted at NODE, reachable as SEGMENT below PATH.

The body of a node is only inserted once it is expanded, which is what
`magit-insert-section-body' is for; that keeps the initial buffer small."
  (let* ((entry (hm-options--node-entry node))
         (children (sort (hash-table-keys (hm-options--node-children node)) #'string<))
         (section
          (magit-insert-section (hm-option path t)
            (magit-insert-heading
             (if children (length children))
             (propertize segment 'face 'magit-section-heading)
             (when entry (hm-options--heading-annotation entry)))
            (magit-insert-section-body
              (if children
                  (dolist (child children)
                    (hm-options--insert-node (concat path "." child) child
                                             (gethash child (hm-options--node-children node))))
                (when entry
                  (insert (or (hm-options--description-summary entry)
                              "No description.\n"))))))))
    (puthash path section hm-options--sections)
    ;; HIDE only sets the `hidden' slot; the overlay that actually hides the
    ;; body is created by `magit-section-hide', which also paints the
    ;; expand/collapse indicator in the fringe.
    (magit-section-hide section)))

(defun hm-options--heading-annotation (entry)
  "Return the type of ENTRY, for display next to its name."
  (concat "  "
          (propertize (or (hm-options--field entry "type") "")
                      'face 'font-lock-type-face)))

(defun hm-options--description-summary (entry)
  "Return a one-line summary of ENTRY, or nil."
  (when hm-options-show-summary
    (when-let* ((text (hm-options--clean-description
                      (hm-options--field entry "description"))))
      (setq text (car (split-string text "\n")))
      (concat (if (> (length text) hm-options-summary-width)
                  (concat (substring text 0 hm-options-summary-width) "...")
                text)
              "\n"))))

(defun hm-options--insert-flat (regexp)
  "Insert every option whose name matches REGEXP, as a flat list."
  (dolist (name (if regexp
                    (cl-remove-if-not (lambda (n) (string-match-p regexp n))
                                      hm-options--names)
                  hm-options--names))
    (let* ((entry (gethash name hm-options--entries))
           (section
            (magit-insert-section (hm-option name t)
              (magit-insert-heading
               (propertize name 'face 'magit-section-heading)
               (hm-options--heading-annotation entry))
              (magit-insert-section-body
                (insert (or (hm-options--description-summary entry)
                            (hm-options--clean-description
                             (hm-options--field entry "description"))
                            "No description.\n"))))))
      (puthash name section hm-options--sections)
      (magit-section-hide section))))

(defun hm-options--render (buffer title view)
  "Render the option browser into BUFFER, using VIEW."
  (with-current-buffer buffer
    (let ((inhibit-read-only t))
      (hm-options-mode)
      (erase-buffer)
      (setq-local hm-options--sections (make-hash-table :test #'equal)
                  hm-options--view view
                  hm-options--option nil
                  truncate-lines nil)
      (setq magit-root-section nil
            magit-insert-section--parent nil
            magit-insert-section--oldroot nil)
      (magit-insert-section (hm-root)
        (magit-insert-heading (propertize title 'face 'magit-section-heading))
        (insert "\n")
        (pcase view
          ('tree (dolist (segment (sort (hash-table-keys
                                         (hm-options--node-children hm-options--tree))
                                        #'string<))
                   (hm-options--insert-node segment segment
                                            (gethash segment
                                                     (hm-options--node-children
                                                      hm-options--tree)))))
          (`(list . ,regexp) (hm-options--insert-flat regexp))))
      (goto-char (point-min)))))

(defun hm-options--refresh-buffer ()
  "Re-render the current buffer, reusing its view."
  (hm-options--read-file)
  (hm-options--render (current-buffer) (hm-options--title hm-options--view)
                      hm-options--view))

(defun hm-options--title (view)
  "Return a title line for a buffer rendered with VIEW."
  (format "Home Manager options (%d)%s - %s"
          (length hm-options--names)
          (pcase view
            (`(list . ,regexp) (format " matching %S" regexp))
            (_ ""))
          hm-options--source))

;;; Commands -----------------------------------------------------------------

;;;###autoload
(defun hm-options (&optional refresh file)
  "Display the home-manager options browser.
With prefix argument REFRESH re-read the options file.  FILE overrides
`hm-options-file'."
  (interactive (list current-prefix-arg))
  (unless (derived-mode-p 'hm-options-mode)
    (setq hm-options--origin-buffer (current-buffer)))
  (when refresh (setq hm-options--entries nil))
  (hm-options--read-file file)
  (let ((buffer (get-buffer-create "*hm-options*")))
    (hm-options--render buffer (hm-options--title 'tree) 'tree)
    (pop-to-buffer buffer)))

(defun hm-options-refresh ()
  "Re-read the options file and rebuild the browser."
  (interactive)
  (hm-options--refresh-buffer))

(defun hm-options-list (regexp)
  "List all options whose name matches REGEXP in a flat buffer."
  (interactive (list (read-regexp "Option name matches" nil 'hm-options--history)))
  (hm-options--read-file)
  (let ((buffer (get-buffer-create "*hm-options*")))
    (hm-options--render buffer
                        (hm-options--title (cons 'list regexp))
                        (cons 'list regexp))
    (pop-to-buffer buffer)))

(defvar hm-options--history nil
  "Minibuffer history of option-related regexps.")

(defun hm-options--name-at-point ()
  "Return the name of the option at point, or nil."
  (let ((section (magit-current-section)))
    (while (and section (not (eq (oref section type) 'hm-option)))
      (setq section (oref section parent)))
    (and section (oref section value))))

(defun hm-options--current-name ()
  "Return the option the current buffer is looking at."
  (or hm-options--option (hm-options--name-at-point)))

(defun hm-options--reveal (section)
  "Make SECTION visible and move point onto it."
  (when (oref section hidden)
    (magit-section-show section))
  (goto-char (oref section start))
  (when (get-buffer-window (current-buffer) t)
    (recenter)))

(defun hm-options--jump-tree (name)
  "Move point to the tree section for NAME, expanding its ancestors."
  (let ((path nil)
        (previous nil))
    (dolist (segment (split-string name "\\." t))
      (setq path (if path (concat path "." segment) segment))
      ;; The children of a section only exist once it has been expanded.
      (when (and previous (oref previous hidden))
        (magit-section-show previous))
      (setq previous (or (gethash path hm-options--sections)
                         (user-error "No option named %s" name))))
    (hm-options--reveal previous)))

(defun hm-options-jump (name)
  "Move point to the section describing NAME."
  (interactive (list (hm-options--read-name)))
  (unless (derived-mode-p 'hm-options-mode)
    (hm-options))
  (cond
   ((eq hm-options--view 'tree)
    (hm-options--jump-tree name))
   ((gethash name hm-options--sections)
    (hm-options--reveal (gethash name hm-options--sections)))
   (t
    (hm-options--render (current-buffer)
                        (hm-options--title 'tree) 'tree)
    (hm-options--jump-tree name))))

(defun hm-options--read-name ()
  "Read an option name from the minibuffer."
  (hm-options--read-file)
  (completing-read "Option: " (hm-options--completion-table)
                   nil t nil nil (hm-options--current-name)))

(defun hm-options--completion-table ()
  "Return a completion table over every known option."
  (lambda (string predicate action)
    (if (eq action 'metadata)
        '(metadata (category . hm-option)
                   (annotation-function . hm-options--annotation))
      (complete-with-action action hm-options--names string predicate))))

(defun hm-options--annotation (name)
  "Annotate NAME with its type."
  (when-let* ((entry (and hm-options--entries (gethash name hm-options--entries))))
    (concat "  " (propertize (or (hm-options--field entry "type") "")
                             'face 'completions-annotations))))

(defun hm-options-copy-path ()
  "Add the option path at point to the kill ring."
  (interactive)
  (if-let* ((name (hm-options--current-name)))
      (progn (kill-new name) (message "Copied %s" name))
    (user-error "No option at point")))

(defun hm-options-browse-declaration ()
  "Open the first module declaring the option at point in a browser."
  (interactive)
  (if-let* ((name (hm-options--current-name))
            (entry (gethash name hm-options--entries))
            (decl (car (hm-options--field entry "declarations")))
            (url (gethash "url" decl)))
      (browse-url url)
    (user-error "No declaration recorded for this option")))

;;; Inserting a snippet ------------------------------------------------------

(defun hm-options--simple-default-p (text)
  "Return non-nil if TEXT is a literal simple enough to use as a snippet."
  (and (stringp text)
       (string-match-p
        (concat "\\`\\(true\\|false\\|null"
                "\\|-?[0-9]+\\(\\.[0-9]+\\)?"
                "\\|\"[^\"]*\""
                "\\|\\[\\s-*\\]\\|{\\s-*\\}"
                "\\|\\./[^ \t\n]*\\)\\'")
        text)))

(defun hm-options--snippet-value (entry)
  "Return a sensible placeholder value for ENTRY, based on its type."
  (let ((default (hm-options--literal entry "default"))
        (type (or (hm-options--field entry "type") "")))
    (cond
     ((hm-options--simple-default-p default) default)
     ((string-match-p "\\`list of" type) "[ ]")
     ((string-match-p "attribute set\\|submodule\\|attrs" type) "{ }")
     ((string-match-p "boolean" type) "false")
     ((string-match-p "integer\\|float" type) "0")
     ((string-match-p "string" type) "\"\"")
     ((string-match-p "path" type) "./.")
     (t "null"))))

(defun hm-options--snippet (name)
  "Return a Nix snippet for the option NAME."
  (let ((value (hm-options--snippet-value (gethash name hm-options--entries))))
    (concat name " = " value ";\n")))

(defun hm-options-insert (name)
  "Insert a Nix snippet for the option NAME into the originating buffer."
  (interactive (list (hm-options--current-name)))
  (unless name (user-error "No option at point"))
  (let ((buffer (or (and (buffer-live-p hm-options--origin-buffer)
                         hm-options--origin-buffer)
                    (user-error "No originating buffer; open the browser from a Nix file"))))
    (with-current-buffer buffer
      (insert (hm-options--snippet name)))
    (message "Inserted %s = %s; into %s"
             name (hm-options--snippet-value (gethash name hm-options--entries))
             (buffer-name buffer))
    (when (string-match-p "<name>" name)
      (message "Note: the path still contains <name> placeholders"))))

;;; The documentation buffer -------------------------------------------------

(defun hm-options--insert-prop (label value &optional block)
  "Insert VALUE as a subsection labelled LABEL.
When BLOCK is non-nil VALUE is inserted as a body instead of a heading."
  (when (and value (not (equal value "")))
    (magit-insert-section (hm-prop label)
      (if block
          (progn
            (magit-insert-heading (propertize label 'face 'magit-section-heading))
            (insert value))
        (let ((lines (split-string value "\n")))
          (magit-insert-heading
           (propertize (concat label ": ") 'face 'magit-section-heading)
           (propertize (car lines) 'face 'font-lock-string-face))
          (when (cdr lines)
            (insert (string-join (cdr lines) "\n")))))
      (insert "\n"))))

(defun hm-options--insert-declarations (entry)
  "Insert the modules declaring ENTRY as clickable links."
  (when-let* ((declarations (hm-options--field entry "declarations")))
    (magit-insert-section (hm-decls)
      (magit-insert-heading (propertize "Declared in" 'face 'magit-section-heading))
      (dolist (declaration declarations)
        (let ((url (gethash "url" declaration)))
          (insert "  ")
          (insert-text-button (or (gethash "name" declaration) url)
                              'action (lambda (_button) (browse-url url))
                              'follow-link t
                              'help-echo url)
          (insert "\n")))
      (insert "\n"))))

(defun hm-options-describe (name)
  "Show the full documentation of the option NAME."
  (interactive (list (or (hm-options--current-name) (hm-options--read-name))))
  (unless name (user-error "No option at point"))
  (let* ((entry (or (gethash name hm-options--entries)
                    (user-error "Unknown option: %s" name)))
         (buffer (get-buffer-create (format "*hm-options: %s*" name))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (hm-options-info-mode)
        (erase-buffer)
        (setq-local hm-options--option name
                    truncate-lines nil)
        (setq magit-root-section nil
              magit-insert-section--parent nil
              magit-insert-section--oldroot nil)
        (magit-insert-section (hm-info)
          (magit-insert-heading (propertize name 'face 'magit-section-heading))
          (insert "\n")
          (hm-options--insert-prop "Type" (hm-options--field entry "type"))
          (hm-options--insert-prop "Default" (hm-options--literal entry "default") t)
          (hm-options--insert-prop "Example" (hm-options--literal entry "example") t)
          (hm-options--insert-prop "Read only"
                                   (if (hm-options--field entry "readOnly") "yes" "no"))
          (hm-options--insert-declarations entry)
          (hm-options--insert-prop "Description"
                                   (hm-options--clean-description
                                    (hm-options--field entry "description"))
                                   t))
        (goto-char (point-min))))
    (pop-to-buffer buffer)))

;;; Modes and bindings -------------------------------------------------------

(defvar hm-options--transient-layout
  [["Show"
    ("RET" "documentation" hm-options-describe)
    ("b" "browse declaration" hm-options-browse-declaration)
    ("w" "copy option path" hm-options-copy-path)]
   ["Insert"
    ("i" "insert snippet" hm-options-insert)]
   ["Search"
    ("s" "jump to option" hm-options-jump)
    ("/" "filter by regexp" hm-options-list)]
   ["Refresh"
    ("g" "re-read options.json" hm-options-refresh)]])

;;;###autoload (autoload 'hm-options-dispatch "hm-options" nil t)
(transient-define-prefix hm-options-dispatch ()
  "Home Manager options."
  hm-options--transient-layout)

(define-derived-mode hm-options-mode magit-section-mode "HMOptions"
  "Major mode for browsing the home-manager option database.

\\{hm-options-mode-map}"
  :group 'hm-options
  (setq-local revert-buffer-function (lambda (&rest _) (hm-options--refresh-buffer)))
  (setq-local buffer-list-update-hook nil)
  (read-only-mode 1))

(keymap-set hm-options-mode-map "TAB"   #'magit-section-toggle)
(keymap-set hm-options-mode-map "RET"   #'hm-options-describe)
(keymap-set hm-options-mode-map "?"     #'hm-options-dispatch)
(keymap-set hm-options-mode-map "s"     #'hm-options-jump)
(keymap-set hm-options-mode-map "/"     #'hm-options-list)
(keymap-set hm-options-mode-map "a"     #'hm-options)
(keymap-set hm-options-mode-map "i"     #'hm-options-insert)
(keymap-set hm-options-mode-map "w"     #'hm-options-copy-path)
(keymap-set hm-options-mode-map "b"     #'hm-options-browse-declaration)
(keymap-set hm-options-mode-map "d"     #'hm-options-describe)
(keymap-set hm-options-mode-map "g"     #'hm-options-refresh)
(keymap-set hm-options-mode-map "q"     #'quit-window)

(define-derived-mode hm-options-info-mode magit-section-mode "HMOptionsInfo"
  "Major mode for the documentation buffer of a single home-manager option.

\\{hm-options-info-mode-map}"
  :group 'hm-options
  (read-only-mode 1))

(keymap-set hm-options-info-mode-map "TAB" #'magit-section-toggle)
(keymap-set hm-options-info-mode-map "?"   #'hm-options-dispatch)
(keymap-set hm-options-info-mode-map "i"   #'hm-options-insert)
(keymap-set hm-options-info-mode-map "w"   #'hm-options-copy-path)
(keymap-set hm-options-info-mode-map "b"   #'hm-options-browse-declaration)
(keymap-set hm-options-info-mode-map "g"   #'hm-options-describe)
(keymap-set hm-options-info-mode-map "q"   #'quit-window)

;;; Marginalia ---------------------------------------------------------------

(defun hm-options--marginalia (name)
  "Marginalia annotator for home-manager options."
  (when (and (fboundp 'marginalia--fields)
             hm-options--entries
             (gethash name hm-options--entries))
    (let ((entry (gethash name hm-options--entries)))
      (marginalia--fields
       ((or (hm-options--field entry "type") "") :face 'marginalia-type :width 30)
       ((hm-options--clean-description (hm-options--field entry "description"))
        :truncate 8.0 :face 'marginalia-documentation)))))

(with-eval-after-load 'marginalia
  (let ((entry '(hm-option hm-options--marginalia none none)))
    ;; Marginalia renamed its registry to `marginalia-annotators' at some
    ;; point; support both spellings.
    (cond
     ((boundp 'marginalia-annotators)
      (add-to-list 'marginalia-annotators entry)
      (when (fboundp 'marginalia--cache-reset) (marginalia--cache-reset)))
     ((boundp 'marginalia-annotator-registry)
      (add-to-list 'marginalia-annotator-registry entry)))))

(provide 'hm-options)
;;; hm-options.el ends here
