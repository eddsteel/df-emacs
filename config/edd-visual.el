;; Visual

;; TODO:
;; * Use theme colours somehow
;; * bring colours set in custom.el here
;; * theme jabber notifications face too
;; * theme which-func-mode face too

(eval-after-load "abbrev" '(diminish 'abbrev-mode "🆘"))
(eval-after-load "auto-highlight-symbol" '(diminish 'auto-highlight-symbol-mode))
(eval-after-load "company" '(diminish 'company-mode "🎩"))
(eval-after-load "flycheck" '(diminish 'flycheck-mode "🚨"))
(eval-after-load "flyspell" '(diminish 'flyspell-mode "💬"))
(eval-after-load "git-gutter" '(diminish 'git-gutter-mode))
(eval-after-load "guide-key" '(diminish 'guide-key-mode "🐼"))
(eval-after-load "hardcore-mode" '(diminish 'hardcore-mode "💀"))
(eval-after-load "helm" '(diminish 'helm-mode "👷"))
(eval-after-load "magit" '(diminish 'magit-auto-revert-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode "💫"))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "whitespace" '(diminish 'whitespace-mode "🚀"))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode "📎"))
(eval-after-load "mml" '(diminish 'mml-mode "📩"))



;; copied shamelessly from http://www.lunaryorn.com/2014/07/26/make-your-emacs-mode-line-more-useful.html
(defvar edd-vc-mode-line
  '(" " (:propertize
         ;; Strip the backend name from the VC status information
         (:eval (let ((backend (symbol-name (vc-backend (buffer-file-name)))))
                  (substring vc-mode (+ (length backend) 2))))
         face font-lock-variable-name-face))
  "Mode line format for VC Mode.")
(put 'edd-vc-mode-line 'risky-local-variable t)

(setq-default mode-line-format
              '("%e"
                mode-line-front-space mode-line-mule-info mode-line-client mode-line-modified mode-line-remote
                " "
                mode-line-misc-info
                mode-line-frame-identification mode-line-buffer-identification " "
                mode-line-position
                (vc-mode edd-vc-mode)
                " " mode-line-modes mode-line-end-spaces))


(eval-after-load "notmuch-unread"
  '(defun notmuch-unread-update-handler ()
     "Update the mode line."
     (setq notmuch-unread-mode-line-string
           (format "📨 %d" (notmuch-unread-count)))
     (force-mode-line-update)))

(eval-after-load "ensime"
'(defun ensime-modeline-string ()
  "Return the string to display in the modeline.
  \"ENSIME\" only appears if we aren't connected.  If connected, include
  connection-name, and possibly some state
  information."
  (when ensime-mode
    (condition-case err
	(let ((conn (ensime-connection-or-nil)))
	  (cond ((and ensime-mode (not conn))
		 (cond
		  ((ensime-owning-server-process-for-source-file buffer-file-name)
		   "[E:Starting]")
		  (t "Ø")))

		((and ensime-mode (ensime-connected-p conn))
		 (concat "["
			 (or (plist-get (ensime-config conn) :name)
			     "E:Connected")
			 (let ((status (ensime-modeline-state-string conn))
			       (unready (not (ensime-analyzer-ready conn))))
                           "")
			 (concat (format " : %s/%s"
					 (ensime-num-errors conn)
					 (ensime-num-warnings conn)))
			 "]"))
		(ensime-mode " [E: Dead Connection]")
		))
      (error (progn
	       "[E: wtf]"
	       ))))))



(provide 'edd-visual)
