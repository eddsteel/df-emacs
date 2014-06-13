; public key
(setq epa-file-encrypt-to "edd@eddandkrista.com")

; use pinentry for os x, and agent
(if (eq system-type 'darwin)
    (setq epg-gpg-program "gpg2"))

(defun load-secrets (stub)
  (load-file (emacsd (concat "secrets/" stub ".el.gpg"))))

(provide 'edd-secrets)
