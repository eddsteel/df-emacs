;; mac annoyances
(when (memq window-system '(mac ns))
  (global-unset-key (kbd "<f11>")) ; it doesn't work
  (global-set-key (kbd "<f10>") 'toggle-frame-fullscreen)
  (global-unset-key (kbd "s-h"))
  (global-unset-key (kbd "s-z"))
  (global-unset-key (kbd "C-z"))
  (global-unset-key (kbd "C-x C-z")))

;; C-x C-m for M-x
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; C-c C-s for shell
(global-set-key (kbd "C-c C-s") 'shell)

;; C-c q for auto-fill mode
(global-set-key (kbd "C-c q") 'auto-fill-mode)

;; C-c ! for a shell
(global-set-key (kbd "C-c !") 'shell-here)

;; C-h for delete
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; C-w for backward-kill-word / kill region
(global-set-key (kbd "C-w") 'kill-region-or-backward-kill-word)

;; ace jump
(global-set-key (kbd "C-c j") 'ace-jump-mode)
(global-set-key (kbd "C-c k") 'ace-jump-mode-pop-mark)

;; org
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)

;; notmuch
(global-set-key (kbd "C-c n") 'notmuch)

;; urls
(global-set-key (kbd "C-c C-p") 'edd-jump-to-prev-url)
(global-set-key (kbd "C-c C-n") 'edd-jump-to-next-url)

(provide 'edd-bindings)
