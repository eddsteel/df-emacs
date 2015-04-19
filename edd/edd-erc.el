(setq erc-server-auto-reconnect nil)

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;; for autoload
(defun edd-erc ()
  (interactive)
  (edd-with-secrets "erc" (erc :password erc-password)))

(provide 'edd-erc)
