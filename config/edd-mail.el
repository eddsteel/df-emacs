(load-secrets "mail")

(setq send-mail-function 'sendmail-send-it)
(setq message-send-mail-function 'message-send-mail-with-sendmail)
(setq mail-specify-envelope-from 't)
(setq message-sendmail-envelope-from 'header)
(setq mail-envelope-from 'header)

;; mail with notmuch
(defun edd-notmuch-mark-search-read ()
  (interactive)
  (notmuch-search-tag-all '("-unread")))

;; notmuch-search-mode-map "" 'edd-notmuch-mark-search-read


;; mail with mu4e
(add-to-list 'load-path "/usr/local/Cellar/mu/0.9.9.5/share/emacs/site-lisp/mu4e")

(require 'mu4e)
(setq mu4e-maildir "~/Mail")

(defun mail-folder (msg stub)
    (if (and msg ;; msg may be nil
          (mu4e-message-contact-field-matches msg :to mail-work-address))
        (concat mail-work-root stub)
        (concat mail-home-root stub)))


(setq mu4e-trash-folder
      (lambda (msg)
        (mail-folder msg "/trash")))

(setq mu4e-refile-folder
      (lambda (msg)
        (mail-folder msg "/archive")))


;; mail from mutt
(add-to-list 'auto-mode-alist '(".*mutt.*" . message-mode))
(setq mail-header-separator "")
(add-hook 'message-mode-hook
          'turn-on-auto-fill
          (function
           (lambda ()
             (progn
               (local-unset-key "\C-c\C-c")
               (define-key message-mode-map "\C-c\C-c" '(lambda ()
                                                          "save and exit quickly"
                                                          (interactive)
                                                          (save-buffer)
                                                          (server-edit)))))))

(provide 'edd-mail)
