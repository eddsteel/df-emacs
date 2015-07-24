(use-package nm
  :ensure t
  :config
  (defun edd-theme-nm ()
    (set-face-attribute 'nm-authors-face nil :inherit font-lock-function-name-face)
    (set-face-attribute 'nm-header-face nil :inherit font-lock-function-name-face :underline t :weight 'bold)
 (set-face-attribute 'nm-read-face nil :inherit font-lock-function-name-face)
 (set-face-attribute 'nm-unread-face  nil :inherit font-lock-keyword-face :weight 'bold))
  (add-hook 'edd-load-theme-hook 'edd-theme-nm)
  (edd-theme-nm))



(defun edd-mailbox ()
  (interactive)
  "Open default mailbox"
  (notmuch))

(use-package notmuch
  :ensure t
  :commands notmuch
  :init
  (edd-with-secrets "mail"
                    (setq mail-specify-envelope-from t
                          mail-envelope-from 'header
                          send-mail-function 'sendmail-send-it
                          message-sendmail-envelope-from 'header
                          message-send-mail-function 'message-send-mail-with-sendmail))
  :config
  (require 'org-notmuch)
  (setq notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox" :key "i")
          (:name "home-in" :query "folder:gmail/INBOX and tag:inbox")
          (:name "unread" :query "tag:unread" :key "u")
          (:name "flagged" :query "tag:flagged" :key "f")
          (:name "sent" :query "tag:sent" :key "t")
          (:name "drafts" :query "tag:draft" :key "d")
          (:name "all mail" :query "*" :key "a")
          (:name "scala-queue" :query "tag:scala-user and tag:unread")
          (:name "akka-queue" :query "tag:akka-user and tag:unread")
          (:name "arch-queue" :query "tag:arch-general and tag:unread")))
  (defun edd-notmuch-mark-search-read ()
    (interactive)
    (notmuch-search-tag-all '("-unread")))
  (bind-key "M" 'edd-notmuch-mark-search-read 'notmuch-search-mode-map)
  (bind-key "g" 'notmuch-refresh-this-buffer 'notmuch-search-mode-map)
  (bind-key "g" 'notmuch-refresh-this-buffer 'notmuch-show-mode-map)
  (bind-key "g" 'notmuch-refresh-this-buffer 'notmuch-tree-mode-map)
  :bind
  ("C-c n" . notmuch))

(use-package notmuch-unread
  :ensure t
  :config
  (setq notmuch-unread-search-term "tag:unread and tag:inbox")

  (defun notmuch-unread-update-handler ()
    "Update the mode line."
    (setq notmuch-unread-mode-line-string
          (format " ✉%d" (notmuch-unread-count)))))

(provide 'edd-mail)
