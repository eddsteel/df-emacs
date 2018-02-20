(require 'magit)
(require 'projectile)
(require 'dash)
(require 'hydra)

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

(defun edd-git-web-link-current-file (&rest args)
  "Derive link for current file in the web provider."
  (interactive)
  (edd-git-web-link-capture (concat (edd-git-web-link-remote) args)
                            (file-relative-name (buffer-file-name))))

(defun edd-git-web-link-browse-current-file ()
  "Open current file in the web provider on master."
  (interactive)
  (browse-url (call-interactively #'edd-git-web-link-current-file)))

(defun edd-git-web-link-browse-current-file-master ()
  "Open current file in the web provider on master."
  (interactive)
  (browse-url (call-interactively #'edd-git-web-link-current-file '("-b master"))))


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

(defun edd-git-web-link-browse-current-line-master ()
  "Open current line in the web provider on master."
  (interactive)
  (browse-url (call-interactively #'edd-git-web-link-current-line '("-b master"))))


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

(defun edd-git-web-link-browse-current-region-master ()
  "Open current region in the web provider."
  (interactive)
  (browse-url (call-interactively #'edd-git-web-link-current-region '("-b master"))))

;; TODO: how to jump out to s
(defhydra hydra-edd-git-web-link (:exit nil :columns 3)
  ("l" edd-git-web-link-browse-current-line "browse current line")
  ("f" edd-git-web-link-browse-current-file "browse current file")
  ("r" edd-git-web-link-browse-current-region "browse current line")
  ("L" edd-git-web-link-browse-current-line-master "browse current line on master")
  ("F" edd-git-web-link-browse-current-file-master "browse current file on master")
  ("R" edd-git-web-link-browse-current-region-master "browse current line on master"))

(provide 'edd-git-web-link)
