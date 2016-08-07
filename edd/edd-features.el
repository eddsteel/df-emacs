;; Features -- tweaking of stuff that's built-in and doesn't require packages.
;;
;;

(dolist
    (feature '(upcase-region downcase-region set-goal-column narrow-to-region))
  (put feature 'disabled nil))

(setq make-backup-files nil)
(winner-mode t)

;; server
;;
(require 'server)
(when (and window-system (not (server-running-p)))
  (add-hook 'after-init-hook 'server-start t))

;; text increase
(global-set-key (kbd "s-<up>") 'text-scale-increase)
(global-set-key (kbd "s-<down>") 'text-scale-decrease)

;; abbrevs
;;
(read-abbrev-file
 (expand-file-name "abbreviations" user-emacs-directory))
(add-hook 'prog-mode-hook #'abbrev-mode)
(add-hook 'term-mode-hook #'abbrev-mode)
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
(dolist (hook '(text-mode conf-mode-hook jabber-chat-mode-hook term-mode-hook))
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

;; Cycle spaces
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; make C-v M-v symmetrical
(setq scroll-preserve-screen-position 'always)

;; use the sensible counter
(global-set-key (kbd "M-=") 'count-words)

;; Terminal -- kill on exit
(defadvice term-handle-exit
  (after term-kill-buffer-on-exit activate)
  (kill-buffer))

;; From http://echosa.github.io/blog/2012/06/06/improving-ansi-term
;; with an addition: strip space/newlines from the end.
;;
(defun edd-term-paste (&optional string)
 (interactive)
 (process-send-string
  (get-buffer-process (current-buffer))
  (replace-regexp-in-string "[ \n]*\\'" ""
                            (if string string (current-kill 0)))))


;; From http://echosa.github.io/blog/2012/06/06/improving-ansi-term
;;
(defun edd-term-hook ()
    (goto-address-mode)
    (define-key term-raw-map (kbd "C-y") 'edd-term-paste)
    (define-key term-raw-map (kbd "C-c C-r") 'rename-buffer))

(add-hook 'term-mode-hook 'edd-term-hook)

;; bind C-c ! to reload config (like org)
(defun edd-config-reload ()
  (interactive)
  (load user-init-file))

(global-set-key (kbd "C-c !") 'edd-config-reload)

;; yank over region
(delete-selection-mode 1)

;; ssh is "faster", and ask for dumb prompts.
(setq tramp-terminal-type "dumb")
;(setq tramp-default-method "ssh")

;; zap up to char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; some associations
(add-to-list 'auto-mode-alist '("\\.apib\\'" . markdown-mode))

;; use english dictionary (there's no canadian or british one)
(setq ispell-dictionary "english")

;; next/previous buffers
(global-set-key (kbd "C-(") 'previous-buffer)
(global-set-key (kbd "C-)") 'next-buffer)


(provide 'edd-features)
