(load-secrets "gtalk")

(defun gtalk-connect ()
  (interactive)
  (setq jabber-account-list-nil)
  (add-to-list 'jabber-account-list
             `(,gchat-uid
               (:network-server . "talk.google.com")
               (:port . 5223)
               (:connection-type . ssl)
               (:password . ,gchat-password)))
  (jabber-connect-all)
  (setq jabber-account-list-nil))

(defun gtalk-connect2 ()
  (interactive)
  (setq jabber-account-list-nil)
  (add-to-list 'jabber-account-list
             `(,gchat-uid2
               (:network-server . "talk.google.com")
               (:port . 5223)
               (:connection-type . ssl)
               (:password . ,gchat-password2)))
  (jabber-connect-all)
  (setq jabber-account-list-nil))

(add-hook 'jabber-chat-mode-hook 'goto-address)
(setq jabber-chat-buffer-show-avatar nil)
(setq jabber-alert-presence-hooks nil)

(provide 'edd-gtalk)
