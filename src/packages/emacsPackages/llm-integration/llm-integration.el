;;; llm-integration.el --- LLM and MCP integration -*- lexical-binding: t; -*-

;;; Commentary:
;; gptel backends, tools, directives and MCP server wiring.

;;; Code:

(require 'gptel)
(require 'mcp)
(require 'gptel-integrations)
(require 'pass-integration)

(defun gptel-compile ()
  "Prompt for a compilation command, run it, and return the output.

Uses =compile' interactively, so the command can be edited in the
minibuffer each time.  Blocks until the compilation process exits,
then returns the contents of its buffer as a string (success or
failure alike)."
  (let ((buf (get-buffer (call-interactively #'compile))))
    (while (get-buffer-process buf)
      (accept-process-output nil 0.1))
    (with-current-buffer buf
      (string-trim (buffer-string)))))

(defun llm-integration-init ()
  "Configure gptel and MCP."
  (setq gptel-log-level 'info
        jsonrpc-default-request-timeout 60)
  (advice-add 'mcp-async-call-tool :override
              (lambda (connection name arguments callback error-callback)
                (jsonrpc-async-request
                 connection :tools/call
                 (list :name name :arguments (or arguments #s(hash-table)))
                 :timeout (mcp--timeout connection)
                 :success-fn (lambda (res) (funcall callback res))
                 :error-fn (jsonrpc-lambda (&key code message &allow-other-keys)
                             (funcall error-callback code message))
                 :timeout-fn (lambda ()
                               (funcall error-callback 124
                                        (format "tool `%s' timed out" name))))))
  (gptel-make-gh-copilot "Copilot")
  (add-hook 'gptel-mode-hook 'company-mode)

  (setq gptel-backend (gptel-make-deepseek "Deepseek"
                        :stream t
                        :key (lambda () (password-store-get-cached "dev/deepseek.com/key")))
        gptel-model 'deepseek-v4-flash
        gptel-use-tools t
        gptel-default-mode 'org-mode)

  (gptel-make-tool
   :name "compile"
   :function #'gptel-compile
   :description "Compile the project and return the compiler output, including any errors. The command is confirmed interactively before it runs."
   :args nil
   :category "emacs")

  (add-to-list 'gptel-tools (gptel-get-tool "compile"))

  (setf (alist-get 'default gptel-directives)
        "You are a large language model living in Emacs and a helpful assistant. Respond concisely.
Write Org Mode and NEVER Markdown as the output will be rendered inside an Emacs buffer and it must be valid Org Mode.")

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
              (gptel-mcp-connect '("fetch" "websearch")))))

(unless noninteractive
  (llm-integration-init))

(provide 'llm-integration)
;;; llm-integration.el ends here
