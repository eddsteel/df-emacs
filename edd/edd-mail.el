(defun edd-mailbox ()
  (interactive)
  "Open default mailbox"
  (mu4e))

(setq mail-specify-envelope-from t
      mail-envelope-from 'header
      send-mail-function 'sendmail-send-it
      message-sendmail-envelope-from 'header
      message-send-mail-function 'message-send-mail-with-sendmail)

(use-package notmuch
  :ensure t
  :commands notmuch
  :config
  (edd-with-secrets "mail" '())
  (require 'org-notmuch)
  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox" :key "i")
          (:name "home-in" :query "folder:gmail/INBOX and tag:inbox")
          (:name "unread" :query "tag:unread" :key "u")
          (:name "flagged" :query "tag:flagged" :key "f")
          (:name "sent" :query "tag:sent" :key "t")
          (:name "drafts" :query "tag:draft" :key "d")
          (:name "all mail" :query "*" :key "a")
          (:name "scala-queue" :query "tag:scala-user and tag:unread")
          (:name "akka-queue" :query "tag:akka-user and tag:unread")
          (:name "arch-queue" :query "tag:arch-general and tag:unread")))
  (defun edd-notmuch-mark-search-read ()
    (interactive)
    (notmuch-search-tag-all '("-unread")))
  (bind-key "M" 'edd-notmuch-mark-search-read 'notmuch-search-mode-map)
  (bind-key "g" 'notmuch-refresh-this-buffer 'notmuch-search-mode-map)
  (bind-key "g" 'notmuch-refresh-this-buffer 'notmuch-show-mode-map)
  (bind-key "g" 'notmuch-refresh-this-buffer 'notmuch-tree-mode-map)
  :bind
  ("C-c n" . notmuch))

(use-package mu4e
  :commands mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :init
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-maildir "~/spool")
  (setq mu4e-hide-index-messages t)

  :config
  (edd-with-secrets "mail"
                    (setq mu4e-contexts
                          `( ,(make-mu4e-context
                               :name "home"
                               :enter-func (lambda () (mu4e-message "context: home"))
                               :match-func
                               (lambda (msg)
                                 (when msg
                                   (mu4e-message-contact-field-matches
                                    msg :to edd-home-mail-address)))
                               :vars `(
                                       (mu4e-drafts-folder     . "/gmail/drafts")
                                       (mu4e-sent-folder       . "/gmail/sent")
                                       (mu4e-trash-folder      . "/gmail/trash")
                                       (mu4e-refile-folder     . "/gmail/archive")
                                       (mu4e-maildir-shortcuts .
                                                               (("/gmail/INBOX"   . ?i)
                                                                ("/gmail/sent"    . ?s)
                                                                ("/gmail/trash"   . ?t)
                                                                ("/gmail/archive" . ?a)))
                                       (user-mail-address . ,edd-home-mail-address)))
                             ,(make-mu4e-context
                               :name "work"
                               :enter-func (lambda () (mu4e-message "context: work"))
                               :match-func
                               (lambda (msg)
                                 (when msg
                                   (mu4e-message-contact-field-matches
                                    msg :to edd-work-mail-address)))
                               :vars `(
                                       (mu4e-drafts-folder     . "/work/drafts")
                                       (mu4e-sent-folder       . "/work/sent")
                                       (mu4e-trash-folder      . "/work/trash")
                                       (mu4e-refile-folder     . "/work/archive")
                                       (mu4e-maildir-shortcuts .
                                                               (("/work/INBOX"   . ?i)
                                                                ("/work/sent"    . ?s)
                                                                ("/work/trash"   . ?t)
                                                                ("/work/archive" . ?a)))

                                       (user-mail-address . ,edd-work-mail-address)
                                       (mu4e-compose-signature . ,edd-work-mail-signature))))))
  (setq mu4e-get-mail-command "offlineimap -o")
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-sent-messages-behavior 'delete)
  :bind
  ("C-c m" . mu4e))

(provide 'edd-mail)
