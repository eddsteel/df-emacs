;; Features -- tweaking of stuff that's built-in
;;
;;

(use-package term
  :functions edd/term
  :init
  (defun edd/term (pfx)
    (interactive "p")
    "Open my currently favourite kind of terminal, smartly.

     With the prefix argument, opens term.
     If the current buffer is an ansi-term, opens a new one.
     If there's no ansi-term, open a new one.
     Otherwise will switch to *ansi-term*"
    (let ((bn (buffer-name))
          (tl "*ansi-term*")
          (newterm (lambda () (ansi-term "bash"))))
      (if (and (<= pfx 1) (get-buffer tl) (not (string-prefix-p tl bn)))
          (switch-to-buffer tl)
        (funcall newterm))))

  ;; From http://echosa.github.io/blog/2012/06/06/improving-ansi-term
  ;;
  (defun edd-term-hook ()
    (goto-address-mode)
    (define-key term-raw-map (kbd "C-y") 'edd-term-paste)
    (define-key term-raw-map (kbd "C-c C-r") 'rename-buffer))

  ;; From http://echosa.github.io/blog/2012/06/06/improving-ansi-term
  ;; with an addition: strip space/newlines from the end.
  ;;
  (defun edd-term-paste (&optional string)
    (interactive)
    (process-send-string
     (get-buffer-process (current-buffer))
     (replace-regexp-in-string "[ \n]*\\'" ""
                               (if string string (current-kill 0)))))

  (add-hook 'term-mode-hook 'edd-term-hook)

  ;; Terminal -- kill on exit
  ;;
  (defadvice term-handle-exit
      (after term-kill-buffer-on-exit activate)
    (kill-buffer))

  :bind (("C-c x" . edd/term)
         :map term-mode-map
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)
         :map term-raw-map
         ("M-o" . other-window)
         ("M-p" . term-send-up)
         ("M-n" . term-send-down)))

(use-package server
  :config
  (when (and window-system (not (server-running-p)))
    (add-hook 'after-init-hook 'server-start t)))

(use-package flyspell
  :diminish " 💅"
  :config
  (dolist (hook '(text-mode-hook org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1)))))

(use-package executable
  :config
  (add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p))

(use-package hideshow :diminish)
(use-package yasnippet :diminish 'yas-minor-mode)
(use-package eldoc :diminish " 📜")

(use-package savehist
  :init
  (savehist-mode t)
  :config
  (expand-file-name "history" user-emacs-directory))

(use-package goto-addr
  :config
  (dolist (hook '(text-mode conf-mode-hook jabber-chat-mode-hook term-mode-hook))
  (add-hook hook #'goto-address-mode)))

(use-package tramp
  :config
  (setq tramp-terminal-type "dumb")
  (setq tramp-default-method "scp")
  (add-to-list 'tramp-methods '("vcsh"
                                (tramp-login-program "vcsh")
                                (tramp-login-args
                                 (("enter")
                                  ("%h")))
                                (tramp-remote-shell "/bin/sh")
                                (tramp-remote-shell-args
                               ("-c")))))
(use-package ispell
  :config
  ;; use english dictionary (there's no canadian or british one)
  (setq ispell-dictionary "english"))

(use-package imenu
  :bind
  ("M-i" . imenu))

(use-package autorevert
  :diminish 'auto-revert-mode)

;; colorize compilation buffers
;; From http://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(use-package ansi-color
  :config
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  :init
    (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer)
  )

(use-package ibuffer
  :bind
  ("C-x C-b" . ibuffer))

(use-package autorevert
  :init
  (global-auto-revert-mode 1))


;; --- tidy below --

;; do sentences like a normal person
(setq sentence-end-double-space nil)


(dolist
    (feature '(upcase-region downcase-region set-goal-column narrow-to-region))
  (put feature 'disabled nil))

(setq make-backup-files nil)
(winner-mode t)

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

;; uniquify buffers
(setq uniquify-buffer-name-style 'post-forward)

(diminish 'visual-line-mode "")

;; make C-v M-v symmetrical
(setq scroll-preserve-screen-position 'always)

;; Cycle spaces
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; use the sensible counter
(global-set-key (kbd "M-=") 'count-words)

;; make M-x more available
(global-set-key (kbd "C-x C-m") 'execute-extended-command)

;; zap up to char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; next/previous buffers
(global-set-key (kbd "C-(") 'previous-buffer)
(global-set-key (kbd "C-)") 'next-buffer)
(global-set-key (kbd "M-o") 'other-window)

;; comint should be easy to run
(global-set-key (kbd "C-c r") 'comint-run)

;; keep system clipboard in kill ring when overwriting it
(setq save-interprogram-paste-before-kill t)

;; full width cursor
(setq x-stretch-cursor t)

;; hurt me plenty
(setq
 large-file-warning-threshold 100000000)

(use-package subword
  :diminish
  :init
  (add-hook 'prog-mode-hook 'subword-mode))


(global-set-key (kbd "C-c - m") (lambda () (interactive)(insert "—")))
(global-set-key (kbd "C-c - n") (lambda () (interactive)(insert "–")))

(provide 'edd-features)
