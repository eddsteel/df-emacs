(use-package emacs
  :config
  (setq mail-specify-envelope-from t
        mail-envelope-from 'header
        send-mail-function 'sendmail-send-it
        message-sendmail-envelope-from 'header
        message-send-mail-function 'message-send-mail-with-sendmail))

(use-package mu4e
  :load-path "/usr/share/emacs/site-lisp/mu4e"
  :config
  (defun edd-mailbox ()
    (interactive)
    "Open default mailbox"
    (mu4e))
  (setq mu4e-use-fancy-chars t)
  (setq mu4e-maildir (expand-file-name "~/var/spool"))
  (setq mu4e-hide-index-messages t)
  (setq mu4e-headers-skip-duplicates t)
  (setq mu4e-change-filenames-when-moving t)
  (setq smtpmail-queue-mail nil
        stmpmail-queue-dir (expand-file-name "~/var/spool/queue/cur/"))
  (setq mail-user-agent 'mu4e-user-agent)
  (add-hook 'message-mode-hook
            '(lambda ()
               (edd-with-secrets "mail"
                                 (setq user-mail-address edd-home-mail-address))))
  :config
  (edd-with-secrets "mail"
                    (setq mu4e-contexts
                          `(,(make-mu4e-context
                               :name "home"
                               :enter-func (lambda () (mu4e-message "context: home"))
                               :match-func
                               (lambda (msg)
                                 (when msg
                                   (mu4e-message-contact-field-matches
                                    msg :to edd-home-mail-address)))
                               :vars `(
                                       (mu4e-drafts-folder     . "/home/drafts")
                                       (mu4e-sent-folder       . "/home/sent")
                                       (mu4e-trash-folder      . "/home/trash")
                                       (mu4e-refile-folder     . "/home/Archive")
                                       (mu4e-maildir-shortcuts .
                                                               (("/home/INBOX"   . ?i)
                                                                ("/home/sent"    . ?s)
                                                                ("/home/trash"   . ?t)
                                                                ("/home/Archive" . ?a)))
                                       (user-mail-address . ,edd-home-mail-address)
                                       (mu4e-compose-signature . ""))))))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-context-policy 'pick-first)
  (setq mu4e-compose-context-policy 'ask-if-none)
  (setq mu4e-sent-messages-behavior 'sent)
  (setq mu4e-change-filenames-when-moving t)

  :bind
  ("C-c m" . mu4e)
  ("C-c n" . edd-mailbox))

(provide 'edd-mail)
