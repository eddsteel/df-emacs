;; HipChat 
 
(setq ssl-program-name "gnutls-cli"
      ssl-program-arguments '("--insecure" "-p" service host)
      ssl-certificate-verification-policy 1)
 
;; Connect using jabber.el
;; M-x jabber-connect <RET>
 
;; Config
(setq jabber-account-list '(("67322_481500@chat.hipchat.com")))
(defvar hipchat-number "67322")
(defvar hipchat-nickname "Edward Steel")
 
;; Join a room
(defun hipchat-join (room)
  (interactive "sRoom name: ")
  (jabber-groupchat-join
   (jabber-read-account)
   (concat hipchat-number "_" room "@conf.hipchat.com")
   hipchat-nickname
   t))
 
;; Mention nicknames in a way that HipChat clients will pickup
(defun hipchat-mention (nickname)
  (interactive
    (list (jabber-muc-read-nickname jabber-group "Nickname: ")))
      (insert (concat "@\"" nickname "\" ")))
