;; quieter, simpler
(dolist
    (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq visible-bell 1)
(mouse-avoidance-mode 'banish)

(setq initial-scratch-message ";; go nuts!\n\n")
(setq inhibit-startup-message t)
(setq make-backup-files nil)

(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq global-hl-line-mode t)
(show-paren-mode)

(when (eq system-type 'darwin) ;; If using the mac port, restore keyboard. I'll set where the keys should be.
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super))

(defalias 'yes-or-no-p 'y-or-n-p)

;; stop the flicker when reloading
(let ((my-theme 'spolsky))
  (if (not (eq my-theme (car custom-enabled-themes)))      
      (if (not (member my-theme custom-enabled-themes))
          (load-theme my-theme 1)
        (enable-theme my-theme))))

(when window-system
  (set-face-attribute 'default nil :height 140 :font "Fira Mono")
;;  (set-face-attribute 'default nil :height 140 :font "Droid Sans Mono")
;;  (set-face-attribute 'default nil :height 140 :font "hasklig")
  (server-start)
  ;; fade when inactive
  (set-frame-parameter (selected-frame) 'alpha '(100 80))
  (put 'default-frame-alist 'alpha '(100 80)))


(setq-default 
 whitespace-style '(face trailing tabs empty indentation)
 indent-tabs-mode nil)

(column-number-mode 't)
(winner-mode 't)

;; abbrevs
(read-abbrev-file (emacsd "abbrev_defs"))


(defun kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region 
      (kill-region (region-beginning) (region-end))
      (backward-kill-word arg)))

;; Highlight 'TK', used as a placeholder generally
(add-hook 'find-file-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<TK\\>" . 'font-lock-warning-face)))) t)


;; history saving
(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode t)

;; custom file
(setq custom-file (emacsd "custom.el"))
(load custom-file 'noerror)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; ace-jump
(require 'ace-jump-mode)

;; diminish
(require 'diminish)

;; guide key
(require 'guide-key)
(setq guide-key/guide-key-sequence
      '("C-x r" "C-x 4" "C-x 8" "C-c C-x" "C-c C-v"))
(guide-key-mode 1)


;; hardcore mode
(require 'hardcore-mode)
(global-hardcore-mode)


;; projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)


;; goto-address for gtalk, hipchat
(add-hook 'jabber-chat-mode-hook 'goto-address)

;; flyspell

(dolist (hook '(text-mode-hook org-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; winner mode
(winner-mode 1)

;; font-lock for all (e.g. for TK)
(global-font-lock-mode t)

;; useful for goto-address-mode
(defun edd-jump-to-next-url ()
  (interactive)
  (point-at-eol) ; so we don't jump to the end of current URL
  (search-forward-regexp goto-address-url-regexp)
  (backward-char))

(defun edd-jump-to-prev-url ()
  (interactive)
  (point-at-bol)
  (search-backward-regexp goto-address-url-regexp)
  (forward-char))

;; time
(display-time-mode 1)

;; This causes the current time in the mode line to be displayed in
;; `egoge-display-time-face' to make it stand out visually.
(setq display-time-string-forms
      '((propertize (concat " " 24-hours ":" minutes " "))))

;; auto-revert when files change
(setq global-auto-revert-mode t)

;; nuff said
(nyan-mode)

;; helm
(require 'helm-config)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-m") 'helm-M-x)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(global-set-key (kbd "C-c h o") 'helm-occur)

(helm-mode 1)

;; RE-Builder
(require 're-builder)
(setq reb-re-syntax 'string)

;; Smart Parens
(require 'smartparens)
(smartparens-global-mode)
(sp-pair "'" nil :actions :rem) ; too annoying for scala/elisp


;; Make scripts executable
(add-hook 'after-save-hook
  'executable-make-buffer-file-executable-if-script-p)

(provide 'edd-editor)
