(load-secrets "erc")
;;(setq erc-port 6697)
(setq erc-port 6667)
(setq erc-server-auto-reconnect nil)
(setq erc-server "chat.freenode.net")

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; for autoload
(defun edd-erc ()
  (erc))

(provide 'edd-erc)
