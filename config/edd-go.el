;; Autoloads and stuff I want to manually initiate

(defun go-jabber ()
  (interactive)
  (require 'edd-hipchat)
  (require 'edd-gtalk)
  (hipchat-connect)
  (gtalk-connect)
  (gtalk-connect2))


(autoload 'hipchat-connect "edd-hipchat")
(defun go-hipchat ()
  (interactive)
  (hipchat-connect))

(autoload 'erc "edd-erc")
(defun go-erc ()
  (interactive)
  (erc))

(provide 'edd-go)
