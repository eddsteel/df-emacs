;; Google Talk and hipchat through jabber.

;; GTALK

(use-package jabber
  :ensure t
  :init
  (setq starttls-use-gnutls t
        starttls-gnutls-program "gnutls-cli"
        starttls-extra-arguments '("--starttls" "--insecure"))
  (setq jabber-chat-buffer-show-avatar nil)
  (setq jabber-alert-presence-hooks nil)
  (bind-key "C-c" 'jabber-connect-all jabber-global-keymap)
  :config
  (add-hook 'jabber-post-connect-hooks 'edd-jabber-hook)
  (add-hook 'jabber-chat-mode-hook (lambda () (local-set-key (kbd "@") 'edd-hipchat-mention)))
  (bind-key "x" 'edd-jabber-clear-activity jabber-global-keymap)
  (bind-key "j" 'edd-hipchat-join jabber-global-keymap)
  (bind-key "b" 'hipchat-switch-to-room jabber-global-keymap)
  (edd-with-secrets "gtalk" (progn
                              (add-to-list 'jabber-account-list
                                           `(,gchat-uid
                                             (:network-server . "talk.google.com")
                                             (:port . 5223)
                                             (:connection-type . ssl)
                                             (:password . ,gchat-password)) t)
                              (add-to-list 'jabber-account-list
                                           `(,gchat-uid2
                                             (:network-server . "talk.google.com")
                                             (:port . 5223)
                                             (:connection-type . ssl)
                                             (:password . ,gchat-password2)) t)))
  (edd-with-secrets "hipchat"
                    (add-to-list 'jabber-account-list
                                 `(,(concat hipchat-gid "_" hipchat-uid "@chat.hipchat.com")
                                   (:password . ,hipchat-password)) t)))

(defun edd-jabber-clear-activity ()
  (interactive)
  (setq jabber-activity-jids nil)
  (jabber-activity-mode-line-update))


(defun edd-hipchat-join (room)
  (interactive "sRoom name: ")
  (jabber-groupchat-join
   (hipchat-account)
   (concat hipchat-gid "_" room "@conf.hipchat.com")
   hipchat-nickname
   t))

(defun edd-hipchat-joinall ()
  (interactive)
  (dolist (room hipchat-autojoin) (edd-hipchat-join room)))


;; find the jabber account for hipchat.
(defun hipchat-account ()
    (car (cl-member-if (lambda (x) (member (concat hipchat-gid "_" hipchat-uid) (plist-get x :state-data)))
              jabber-connections)))

(defun hipchat-rooms ()
  (interactive)
  (jabber-ahc-get-list (hipchat-account) "conf.hipchat.com"))

;; From https://github.com/shosti/.emacs.d/blob/master/personal/p-jabber.el p-hipchat-rooms
(defun hipchat-open-rooms ()
  (cons
   '("roster" . "*-jabber-roster-*")
   (->> (buffer-list)
     (-map
      (lambda (b)
        (-if-let
            (chat-name
             (nth 2 (s-match
                     "\\*-jabber-\\(groupchat-[0-9]+_\\|chat-\\)\\([^*]+\\)-\\*"
                     (buffer-name b))))
            (cons (->> chat-name
                    (s-replace "_" " ")
                    (s-chop-suffix "@conf.hipchat.com"))
                  (buffer-name b)))))
     (-filter 'car))))

(defun hipchat-switch-to-room ()
  (interactive)
  (let* ((chatrooms (hipchat-open-rooms))
         (room-names (-map 'car chatrooms))
         (room
          (completing-read "Room: " room-names nil nil nil nil
                           (car room-names))))
    (switch-to-buffer (cdr (assoc room chatrooms)))))

(defun edd-is-hipchat (conn)
    (string-equal
     (concat hipchat-gid "_" hipchat-uid)
     (plist-get (plist-get conn :state-data) :username)))

(defun edd-jabber-hook (conn)
  (when (edd-is-hipchat conn)
    (progn
      (edd-hipchat-joinall)
      (edd-hipchat-load-users))))

;; User-complete
(use-package request
  :ensure t)
(use-package json
  :ensure t)

(defvar hipchat-users nil
  "All users on the hipchat server, used only for mentions")

(defun edd-hipchat-load-users ()
  (let ((args "max-results=400"))
    (request
     (concat "https://api.hipchat.com/v2/user?" args)
     :type "GET"
     :parser 'json-read
     :headers (list (cons "Authorization"
                          (concat "Bearer " hipchat-api-token)))
     :success (function* (lambda (&key data &allow-other-keys)
                           (when data
                             (setq hipchat-users
                                   (edd-hipchat-extract-users-from-json data))))))))

(defun edd-hipchat-extract-users-from-json (json)
  "Extract the user name info, JSON, returned from hipchat into
an association list, name -> mention name"
  (let ((items (cdr (assq 'items json))))
    (mapcar
     (lambda (item)
       (let ((name (cdr (assq 'name item)))
            (mention (cdr (assq 'mention_name item))))
         (cons name mention)))
     items)))

(defun edd-hipchat-find-users (re)
  "Searches in hipchat-users for users whose name or mention name
  matches RE"
  (let ((hc-results nil))
    (mapcar
     (lambda (pair)
       (if (or
            (string-match re (car pair))
            (string-match re (cdr pair)))
           (add-to-list 'hc-results pair)))
     hipchat-users)
    hc-results))

(defun who-hipchat (re)
  (interactive "Mwho: ")
  (unless hipchat-users (edd-hipchat-load-users))
  (message
   (mapconcat 'identity
              (mapcar (lambda (pair)
                        (concat (car pair) " is @" (cdr pair)))
                      (edd-hipchat-find-users re))
              ", ")))

(defun edd-hipchat-mention ()
  (interactive)
  (progn
    (unless hipchat-users (edd-hipchat-load-users))
    (insert "@")
    (insert (if (and hipchat-users
                     (s-ends-with-p "@conf.hipchat.com-*" (buffer-name)))
                (completing-read "" (mapcar 'cdr hipchat-users) nil nil nil nil nil nil)
              ""))))

(provide 'edd-jabber)
