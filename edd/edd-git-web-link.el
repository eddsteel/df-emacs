(require 'magit)
(require 'projectile)
(require 'dash)
(require 'hydra)

(defun edd-git-web-link-capture (&rest args)
  ""
  (let
      ((cmd (apply #'concat (-interpose " " (cons "git web-link" args)))) )
    (message cmd)
    (substring
     (shell-command-to-string cmd) 0 -1)))

(defun edd-git-web-link-remote ()
  "Work out the current remote (uses magit, falls back to 'origin')"
  (or
   (magit-get-remote)
   "origin"))

(defun edd-git-web-link-current-file (&rest args)
  "Derive link for current file in the web provider."
  (apply 'edd-git-web-link-capture
         (append
          args
          (list
           (edd-git-web-link-remote)
           (file-relative-name (buffer-file-name))))))

(defun edd-git-web-link-browse-current-file ()
  "Open current file in the web provider on master."
  (interactive)
  (browse-url (edd-git-web-link-current-file "-d")))

(defun edd-git-web-link-browse-current-file-master ()
  "Open current file in the web provider on master."
  (interactive)
  (browse-url (edd-git-web-link-current-file "-b" "master" "-d")))


(defun edd-git-web-link-current-line (&rest args)
  "Derive link for current line in the web provider."
  (apply 'edd-git-web-link-capture
         (append
          args
          (list
           (edd-git-web-link-remote)
           (file-relative-name (buffer-file-name))
           (int-to-string (line-number-at-pos))))))

(defun edd-git-web-link-browse-current-line ()
  "Open current line in the web provider."
  (interactive)
  (browse-url (edd-git-web-link-current-line "-d")))

(defun edd-git-web-link-browse-current-line-master ()
  "Open current line in the web provider on master."
  (interactive)
  (browse-url (edd-git-web-link-current-line "-b" "master" "-d")))


(defun edd-git-web-link-current-region (&rest args)
  "Derive link for current region in the web provider."
  (apply 'edd-git-web-link-capture
         (append
          args
          (list
           (edd-git-web-link-remote)
           (file-relative-name (buffer-file-name))
           (int-to-string (line-number-at-pos (region-beginning)))
           (int-to-string (line-number-at-pos (region-end)))))))

(defun edd-git-web-link-browse-current-region ()
  "Open current region in the web provider."
  (interactive)
  (browse-url (edd-git-web-link-current-region "-d")))

(defun edd-git-web-link-browse-current-region-master ()
  "Open current region in the web provider."
  (interactive)
  (browse-url (edd-git-web-link-current-region "-b" "master" "-d")))

(defun edd-git-review ()
  (interactive)
  (git-gutter:set-start-revision "origin/master"))

;; TODO: move to hydra
(defhydra hydra-edd-git-web-link (:exit nil :columns 3)
  ("b" magit-blame "blame")
  ("B" magit-blame-popup "blame...")
  ("c" edd-git-review "code review")
  ("d" magit-diff-buffer-file "diff")
  ("D" magit-diff-buffer-file-popup "diff..")
  ("g" magit-log-buffer-file "log")
  ("G" magit-log-buffer-file-popup "log...")
  ("l" edd-git-web-link-browse-current-line "browse current line")
  ("f" edd-git-web-link-browse-current-file "browse current file")
  ("r" edd-git-web-link-browse-current-region "browse current line")
  ("L" edd-git-web-link-browse-current-line-master "browse current line on master")
  ("F" edd-git-web-link-browse-current-file-master "browse current file on master")
  ("n" git-gutter:next-hunk "Next hunk")
  ("p" git-gutter:previous-hunk "Previous hunk")
  ("N" magit-blob-next "Next blob")
  ("P" magit-blob-previous "Previous blob")
  ("R" edd-git-web-link-browse-current-region-master "browse current line on master")
  ("s" git-gutter:popup-hunk "show hunk diff")
  ("S" git-gutter:statistic "show current git change stats"))

(provide 'edd-git-web-link)
