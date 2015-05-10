(setq erc-server-auto-reconnect nil)

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(defun edd-erc-shorten-names (names)
  (setq edd-erc-to-shorten nil)
  (setq edd-erc-shortened nil)
  (mapc (lambda (n)
          (cond ((string= "#vandev" n) (add-to-list 'edd-erc-shortened "#vd"))
                ((string= "#vanops" n) (add-to-list 'edd-erc-shortened "#vo"))
                (t (add-to-list 'edd-erc-to-shorten n)))) names)
  (append edd-erc-shortened (erc-track-shorten-names edd-erc-to-shorten)))

(setq erc-track-shorten-function 'edd-erc-shorten-names)





;; for autoload
(defun edd-erc ()
  (interactive)
  (edd-with-secrets "erc" (erc :password erc-password)))

(provide 'edd-erc)
