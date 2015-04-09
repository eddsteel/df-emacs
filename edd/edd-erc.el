(setq erc-port 6667)
(setq erc-server-auto-reconnect nil)
(setq erc-server "chat.freenode.net")

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; for autoload
(defun edd-erc ()
  (interactive)
  (edd-with-secrets "erc" (erc)))

(provide 'edd-erc)
