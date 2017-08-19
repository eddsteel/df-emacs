;; remove distractions.
(dolist
    (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))
(mouse-avoidance-mode 'jump)

;; don't yell.
(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

;; don't insist.
(defalias 'yes-or-no-p 'y-or-n-p)

;; show me where I am.
(column-number-mode t)
(show-paren-mode t)

(add-hook 'prog-mode-hook (lambda () (hl-line-mode t)))

;; use when themes don't set hl-line
(defun edd-fix-hl-line-mode ()
  (interactive)
  (set-face-background 'hl-line (face-background 'highlight)))

;; use that font I like.
(defun edd-prep-frame (frame)
  (with-selected-frame frame
    (when (display-graphic-p)
      (progn
        (when (member "Noto Emoji" (font-family-list))
          (set-fontset-font t '(#x1F300 . #x1F6FF) "Noto Emoji"))
        (if (eq 'darwin system-type)
            (progn
              (set-frame-font "-*-Fira Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1" 't)
              ;; fade when inactive
              (set-frame-parameter (selected-frame) 'alpha '(100 80))
              (put 'default-frame-alist 'alpha '(100 80)))
          (progn
            (set-face-attribute 'default nil :font "Fira Mono-13")
            (when (member "Symbola" (font-family-list))
              (set-fontset-font t nil "Symbola"))

            ;; don't fade, WM will do that on everything.
            (set-face-background 'default "#222222")))))))


;; if we're loading non-daemon set up frame. Otherwise the hook will get it.
(when (not (daemonp)) (edd-prep-frame (selected-frame)))
(add-hook 'after-make-frame-functions 'edd-prep-frame)

;; Mode line I like.
(display-time-mode 1)
(setq display-time-string-forms
      '((propertize (concat "📆 " day " " (substring monthname 0 3) " " 24-hours ":" minutes " " load))))
(display-battery-mode t)
(setq battery-mode-line-format " %b%p%%")

;; Slightly quieter modeline.
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

;; comfortable bindings
;; C-h for delete
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
;; C-z for help, in exchange
(define-key key-translation-map (kbd "C-z") (kbd "<help>"))


(provide 'edd-ux)
