(load-secrets "mail")

(setq send-mail-function 'sendmail-send-it)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq mail-specify-envelope-from t
      mail-envelope-from 'header)
(setq message-sendmail-envelope-from 'header)

;; mail with notmuch
(defun edd-notmuch-mark-search-read ()
  (interactive)
  (notmuch-search-tag-all '("-unread")))

;; notmuch-search-mode-map "" 'edd-notmuch-mark-search-read


;; mail indicator
(setq notmuch-unread-search-term "tag:unread and tag:inbox")

(require 'org-notmuch)


(defun mailbox ()
  (interactive)
  "Open default mailbox"
  (nm))

(provide 'edd-mail)
