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


;; shell here
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
(diminish 'guide-key-mode "🐼")

;; hardcore mode
(require 'hardcore-mode)
(global-hardcore-mode)
(diminish 'hardcore-mode "💀")

;; projectile
(require 'projectile)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(diminish 'projectile-mode "💫")

;; other diminishments
(eval-after-load "whitespace" '(diminish 'whitespace-mode "🚀"))
(eval-after-load "flyspell" '(diminish 'flyspell-mode "💬"))
(eval-after-load "abbrev" '(diminish 'abbrev-mode "🆘"))
(eval-after-load "flycheck" '(diminish 'flycheck-mode "🚨"))
(eval-after-load "git-gutter" '(diminish 'git-gutter-mode))
(eval-after-load "smartparens" '(diminish 'smartparens-mode))
(eval-after-load "auto-highlight-symbol" '(diminish 'auto-highlight-symbol-mode))

(eval-after-load "company" '(diminish 'company-mode "🎩"))
(eval-after-load "helm" '(diminish 'helm-mode "👷"))

(eval-after-load "yasnippet" '(diminish 'yas-minor-mode "✂"))

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
               mode-line-misc-info
               mode-line-frame-identification mode-line-buffer-identification " "
               mode-line-position
               (vc-mode edd-vc-mode)
               " " mode-line-modes mode-line-end-spaces))


(provide 'edd-editor)
