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

;; C-c q for auto-fill mode
(global-set-key (kbd "C-c q") 'auto-fill-mode)

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
(global-set-key (kbd "C-c n") 'go-mail)

;; magit
(global-set-key (kbd "C-c m") 'magit-status)

;; urls
(global-set-key (kbd "C-c C-p") 'edd-jump-to-prev-url)
(global-set-key (kbd "C-c C-n") 'edd-jump-to-next-url)
(global-set-key (kbd "C-c C-m") 'browse-url-at-point)
(global-set-key (kbd "C-c C-i") 'edd-show-img-inline)

;; helm-dash for docs
(global-set-key (kbd "C-c h d") 'helm-dash)

;; ansi-term
(global-set-key (kbd "C-c x") (lambda () (interactive) (ansi-term "bash")))

;; global haskell repl
(global-set-key (kbd "C-c h h") 'switch-to-haskell)

;; global scala repl
(global-set-key (kbd "C-c h s") 'ensime-inf-switch)

;; ace-window
(global-set-key (kbd "C-x o") 'ace-window)

;; vagrant
(defun edd-vagrant-edit ()
  (interactive)
  (find-file (concat (vagrant-locate-vagrantfile) "Vagrantfile")))


(global-set-key (kbd "C-c v u") 'vagrant-up)
(global-set-key (kbd "C-c v p") 'vagrant-provision)
(global-set-key (kbd "C-c v d") 'vagrant-destroy)
(global-set-key (kbd "C-c v v") 'vagrant-ssh)
(global-set-key (kbd "C-c v e") 'edd-vagrant-edit)

(global-set-key (kbd "C-c o") (lambda () (interactive) (find-file initial-buffer-choice)))

(provide 'edd-bindings)
