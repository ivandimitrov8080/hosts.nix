;;; messaging.el --- Mail, IRC and Telegram -*- lexical-binding: t; -*-

;;; Commentary:
;; Notmuch, ERC, and Telega configuration.

;;; Code:

(require 'code-intelligence)
(require 'pass-integration)
;; Telega inherits `browse-url-handlers', which media-playback populates.
(require 'media-playback)

(defun messaging-init ()
  "Configure mail and messaging."
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

  (require 'erc)
  (add-to-list 'erc-modules 'sasl)
  (erc-update-modules)
  (require 'erc-sasl)
  (setq erc-sasl-user "ivand"
        erc-sasl-auth-source-function (lambda (&rest _)
                                        (password-store-get-cached "soc/nickserv/erc")))

  (setq telega-use-images t
        telega-emoji-font-family (font-spec :family "Noto Color Emoji")
        telega-emoji-use-images nil
        telega-chat-input-markups '("markdown2" "org" nil)
        telega-msg-save-dir "~/dl/telega"
        telega-browse-url-alist browse-url-handlers)

  (auto-image-file-mode 1)
  (add-hook 'telega-load-hook 'telega-notifications-mode)
  (add-hook 'telega-load-hook 'telega-autoplay-mode)
  (add-hook 'telega-chat-mode-hook 'telega-completions-setup-capf)
  (add-hook 'telega-chat-mode-hook #'company-dictionary-mode)

  (require 'telega))

(unless noninteractive
  (messaging-init))

(provide 'messaging)
;;; messaging.el ends here
