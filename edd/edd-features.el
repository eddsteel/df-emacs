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
(eval-after-load "flyspell" '(diminish 'flyspell-mode " 💅"))

;; Make scripts executable
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

(eval-after-load "hideshow" '(diminish 'hs-minor-mode))
(eval-after-load "yasnippet" '(diminish 'yas-minor-mode))
(diminish 'visual-line-mode "")
(diminish 'auto-revert-mode "")

(eval-after-load "eldoc" '(diminish 'eldoc-mode " 📜"))

;; Cycle spaces
(global-set-key (kbd "M-SPC") 'cycle-spacing)

;; make C-v M-v symmetrical
(setq scroll-preserve-screen-position 'always)

;; use the sensible counter
(global-set-key (kbd "M-=") 'count-words)

;; make M-x more available
(global-set-key (kbd "C-x C-m") 'execute-extended-command)


;; bind C-c ! to reload config (like org)
(defun edd-config-reload ()
  (interactive)
  (load user-init-file))

(global-set-key (kbd "C-c !") 'edd-config-reload)

;; ssh is "faster", and ask for dumb prompts.
(setq tramp-terminal-type "xterm-256color")
;(setq tramp-default-method "scp")

;; zap up to char
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;; use english dictionary (there's no canadian or british one)
(setq ispell-dictionary "english")

;; next/previous buffers
(global-set-key (kbd "C-(") 'previous-buffer)
(global-set-key (kbd "C-)") 'next-buffer)

;; kill current buffer by default
;; http://irreal.org/blog/?p=5585
(defun edd-kill-a-buffer (askp)
  (interactive "P")
  (if askp
      (kill-buffer (funcall completing-read-function
                            "Kill buffer: "
                            (mapcar #'buffer-name (buffer-list))))
    (kill-this-buffer)))

(global-set-key (kbd "C-x k") 'edd-kill-a-buffer)

(global-set-key (kbd "M-o") 'other-window)

;; imenu
(global-set-key (kbd "M-i") 'imenu)

;; diminish autorevert
(diminish 'auto-revert-mode)
(diminish 'global-auto-revert-mode)

;; keep system clipboard in kill ring when overwriting it
(setq save-interprogram-paste-before-kill t)

;; colorize compilation buffers
;; From http://stackoverflow.com/questions/13397737/ansi-coloring-in-compilation-mode
(ignore-errors
  (require 'ansi-color)
  (defun my-colorize-compilation-buffer ()
    (when (eq major-mode 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))
  (add-hook 'compilation-filter-hook 'my-colorize-compilation-buffer))

;; Use ibuffer instead of list buffers
;;
(global-set-key (kbd "C-x C-b") 'ibuffer)

(provide 'edd-features)
