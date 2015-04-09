;; Only do this if it's installed.
;;
(use-package org-gcal
  :config
  (defun edd-org-gcal-fetch ()
    (interactive)
    (delete-file (orgfile "cal-work"))
    (org-gcal-fetch))

  (edd-with-secrets "org-gcal"
                    (progn
                      (run-at-time 3300 3300 'org-gcal-refresh-token)
                      (run-at-time 900 900 'edd-org-gcal-fetch))))

(provide 'edd-org-gcal)
