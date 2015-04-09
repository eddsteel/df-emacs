(dolist
    (feature '(upcase-region downcase-region set-goal-column narrow-to-region))
  (put feature 'disabled nil))

(setq make-backup-files nil)
(winner-mode t)

;; server
;;
(when window-system
  (add-hook 'after-init-hook 'server-start t))

;; abbrevs
;;
(read-abbrev-file
 (expand-file-name "abbreviations" user-emacs-directory))
(add-hook 'prog-mode-hook #'abbrev-mode)
(eval-after-load "abbrev" '(diminish 'abbrev-mode ""))

;; custom file
(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(load custom-file 'noerror)

;; save history
(setq save-hist-file
      (expand-file-name "history" user-emacs-directory))
(savehist-mode t)

;; goto address mode
(dolist (hook '(text-mode conf-mode-hook jabber-chat-mode-hook))
  (add-hook hook #'goto-address-mode))

;; uniquify buffers
(setq uniquify-buffer-name-style 'post-forward)

;; flyspell
(dolist (hook '(text-mode-hook org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))
(eval-after-load "flyspell" '(diminish 'flyspell-mode))

;; Make scripts executable
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

(eval-after-load "hideshow" '(diminish 'hs-minor-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))

(provide 'edd-features)
