(require 'magit)
(require 'projectile)
(require 'dash)

(defun edd-git-web-link-capture (&rest args)
  ""
  (shell-command (apply #'concat (-interpose " " (cons "git web-link" args))))
  (substring
   (shell-command-to-string (apply #'concat (-interpose " " (cons "git web-link" args)))) 0 -1))

(defun edd-git-web-link-remote ()
  "Work out the current remote (uses magit, falls back to 'origin')"
  (or
   (magit-get-remote)
   "origin"))

(defun edd-git-web-link-current-file ()
  "Derive link for current file in the web provider."
  (interactive)
  (edd-git-web-link-capture (edd-git-web-link-remote)
                            (file-relative-name (buffer-file-name))))

(defun edd-git-web-link-browse-current-file ()
  "Open current file in the web provider."
  (interactive)
  (browse-url (call-interactively #'edd-git-web-link-current-file)))


(defun edd-git-web-link-current-line ()
  "Derive link for current line in the web provider."
  (interactive)
  (edd-git-web-link-capture (edd-git-web-link-remote)
                            (file-relative-name (buffer-file-name))
                            (int-to-string (line-number-at-pos))))

(defun edd-git-web-link-browse-current-line ()
  "Open current line in the web provider."
  (interactive)
  (browse-url (call-interactively #'edd-git-web-link-current-line)))

(defun edd-git-web-link-current-region ()
  "Derive link for current region in the web provider."
  (interactive)
  (edd-git-web-link-capture (edd-git-web-link-remote)
                            (file-relative-name (buffer-file-name))
                            (int-to-string (line-number-at-pos (region-beginning)))
                            (int-to-string (line-number-at-pos (region-end)))))

(defun edd-git-web-link-browse-current-region ()
  "Open current region in the web provider."
  (interactive)
  (browse-url (call-interactively #'edd-git-web-link-current-region)))

(provide 'edd-git-web-link)
