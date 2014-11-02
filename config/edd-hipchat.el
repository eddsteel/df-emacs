(require 'jabber)
(require 'dash)
(require 's)

; mostly taken from https://github.com/bodil/emacs.d/blob/master/bodil-chat.el

(load-secrets "hipchat")

(setq starttls-use-gnutls t
      starttls-gnutls-program "gnutls-cli"
      starttls-extra-arguments '("--starttls" "--insecure"))


;; To join HipChat rooms easily
(defun hipchat-join (room)
  (interactive "sRoom name: ")
  (jabber-groupchat-join
   (hipchat-account)
   (concat hipchat-gid "_" room "@conf.hipchat.com")
   hipchat-nickname
   t))


(defun hipchat-joinall ()
  (interactive)
  (dolist (room hipchat-autojoin) (hipchat-join room)))

;; Mention nicknames in a way that HipChat clients will pickup
(defun hipchat-mention (nickname)
  (interactive
   (list (jabber-muc-read-nickname jabber-group "Nickname: ")))
  (insert (concat "@\"" nickname "\" ")))

(defun hipchat-connect ()
  (interactive)
  (setq jabber-account-list nil)
  (add-to-list 'jabber-account-list
        `(,(concat hipchat-gid "_" hipchat-uid "@chat.hipchat.com")
          (:password . ,hipchat-password)))
  (jabber-connect-all)
  (setq jabber-account-list nil))

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


(global-set-key (kbd "C-x C-j j") 'hipchat-join)
(global-set-key (kbd "C-x C-j b") 'hipchat-switch-to-room)

(defun edd-jabber-hook (conn)
  (when
      (string-equal
       (concat hipchat-gid "_" hipchat-uid)
       (plist-get (plist-get conn :state-data) :username))
    (hipchat-joinall)))
(add-hook 'jabber-post-connect-hooks 'edd-jabber-hook)


;; let's steal from erc-image-mode and browse-url
(require 'erc-image)
(defun edd-show-img-inline ()
  (interactive)
  (let
      ((url (browse-url-url-at-point)))
    (erc-image-show-url-image url)))

(setq erc-image-display-func 'erc-image-insert-other-buffer)
(setq erc-image-inline-rescale nil)

;; HipChat Users
(require 'request)
(require 'json)

(defvar hipchat-users nil
  "All users on the hipchat server, used only for mentions")

(defun edd-hc-load-users ()
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
                                   (edd-hc-extract-users-from-json data))))))))
               
(defun edd-hc-extract-users-from-json (json)
  "Extract the user name info, JSON, returned from hipchat into
an association list, name -> mention name"
  (let ((items (cdr (assq 'items json))))
    (mapcar
     (lambda (item)
       (let ((name (cdr (assq 'name item)))
            (mention (cdr (assq 'mention_name item))))
         (cons name mention)))
     items)))

(defun edd-hc-find-users (re)
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
  (message
   (mapconcat 'identity
              (mapcar (lambda (pair)
                        (concat (car pair) " is @" (cdr pair)))
                      (edd-hc-find-users re))
              ", ")))



(provide 'edd-hipchat)

