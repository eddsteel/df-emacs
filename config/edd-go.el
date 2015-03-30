;; Autoloads and stuff I want to manually initiate
;;
;; TODO: make this autoloads only, move aggregate commands somewhere else.
;;
(autoload 'gtalk-connect "edd-gtalk" "First google talk account" t)
(autoload 'gtalk-connect2 "edd-gtalk" "Second google talk account" t)
(autoload 'hipchat-connect "edd-hipchat" "Connect to hipchat" t)
(autoload 'edd-erc "edd-erc" "ERC with defaults" t)
(autoload 'mailbox "edd-mail" "Open default mailbox" t)

(defun go-jabber ()
  (interactive)
  (edd-hipchat-connect)
  (edd-gtalk-connect)
  (edd-gtalk-connect2))

;; TODO This defers require edd-org because of secret loading
;; ideally, we should have simple settings in edd-org, require that
;; and the stuff that's manual should be fired off here.
(defun go-org ()
  (interactive)
  (require 'edd-org))

(provide 'edd-go)
