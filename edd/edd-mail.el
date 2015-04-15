(use-package nm
  :ensure t)

(edd-with-secrets "mail"
                  (setq mail-specify-envelope-from t
                        mail-envelope-from 'header
                        send-mail-function 'sendmail-send-it
                        message-sendmail-envelope-from 'header
                        message-send-mail-function 'message-send-mail-with-sendmail))

;; mail with notmuch
(defun edd-notmuch-mark-search-read ()
  (interactive)
  (notmuch-search-tag-all '("-unread")))


(require 'org-notmuch)

(defun edd-mailbox ()
  (interactive)
  "Open default mailbox"
  (nm))

(use-package notmuch-unread
  :ensure notmuch
  :ensure t
  :config
  (setq notmuch-unread-search-term "tag:unread and tag:inbox")
  (defun notmuch-unread-update-handler ()
    "Update the mode line."
    (require 'notmuch)
    (setq notmuch-unread-mode-line-string
          (format " M %d" (notmuch-unread-count)))))

(provide 'edd-mail)
