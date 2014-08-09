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



(defalias 'yes-or-no-p 'y-or-n-p)

;; stop the flicker when reloading
(let ((my-theme 'spolsky))
  (if (not (eq my-theme (car custom-enabled-themes)))      
      (if (not (member my-theme custom-enabled-themes))
          (load-theme my-theme 1)
        (enable-theme my-theme))))

(when window-system
  (set-face-attribute 'default nil :height 140 :font "source code pro")
  (server-start)
  ;; fade when inactive
  (set-frame-parameter (selected-frame) 'alpha '(100 80))
  (put 'default-frame-alist 'alpha '(100 80)))


;; shell here
(maybe-install-and-require 'shell-here)

(setq-default 
 whitespace-style '(face trailing tabs empty indentation)
 indent-tabs-mode nil)

(column-number-mode 't)

;; rainbow parens
(maybe-install-and-require 'rainbow-delimiters)

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

;; ido
(maybe-install-and-require 'ido)
(ido-mode t)
(ido-vertical-mode t)

;; flx-ido
(maybe-install-and-require 'flx-ido)
(flx-ido-mode 1)
(setq ido-use-faces nil)

;; uniquify
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward)

;; ace-jump
(maybe-install-and-require 'ace-jump-mode)

;; guide key
(maybe-install-and-require 'guide-key)
(setq guide-key/guide-key-sequence
      '("C-x r" "C-x 4" "C-x 8" "C-c C-x" "C-c C-v"))
(guide-key-mode 1)
(diminish 'guide-key-mode " ℹ")

;; hardcore mode
(maybe-install-and-require 'hardcore-mode)
(global-hardcore-mode)
(diminish 'hardcore-mode " ☢")

;; projectile
(maybe-install-and-require 'projectile)
(projectile-global-mode)
(diminish 'projectile-mode " ☄")

;; other diminishments
(eval-after-load "auto-complete" '(diminish 'auto-complete-mode " ↪"))
(eval-after-load "whitespace" '(diminish 'whitespace-mode " ✼"))
(eval-after-load "flyspell" '(diminish 'flyspell-mode " ⎁"))
(eval-after-load "abbrev" '(diminish 'abbrev-mode "⇝ "))

;; potentially useful diminishments
;; ࿏ ࿊ ࿃ ࿎ ࿂ ࿁
;; ‽ ⁋ ‣ ‿  ∿
;; ⋌ ⊾ ⌁ ⌕ ⌚ ⌥
;; ⎁ ♫ ☯ 

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


(provide 'edd-editor)
