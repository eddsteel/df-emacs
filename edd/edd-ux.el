;; remove distractions.
(dolist
    (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode blink-cursor-mode))
  (when (fboundp mode) (funcall mode -1)))
(mouse-avoidance-mode 'banish)

;; don't yell.
(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

;; don't distract.
(setq inhibit-startup-message t)

;; don't insist.
(defalias 'yes-or-no-p 'y-or-n-p)

;; show me where I am.
(column-number-mode t)
(show-paren-mode t)
(add-hook 'prog-mode-hook (lambda () (hl-line-mode t)))

;; use that font I like.
(when window-system
  (if (eq 'darwin system-type)
      (progn
        (set-default-font "-*-Fira Mono-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")

        ;; fade when inactive
        (set-frame-parameter (selected-frame) 'alpha '(100 80))
        (put 'default-frame-alist 'alpha '(100 80)))
    (progn
      (set-default-font "-*-Fira Mono-normal-normal-normal-*-17-*-*-*-m-0-iso10646-1")
      ;; don't fade, WM will do that on everything.
      (set-face-background 'default "#222222"))))

(defvar edd-load-theme-hook
  '()
  "Hook to run when theme is changed with `edd-load-theme'")

;; theme loader that side-steps the load/enable distinction and has a hook.
(defun edd-load-theme (my-theme)
  "Load and enable a theme in one operation, then run configured hooks"
  (interactive "Stheme: ")
  (progn
    (unless (eq my-theme (car custom-enabled-themes))
      (if (not (member my-theme custom-enabled-themes))
          (load-theme my-theme 1)
        (enable-theme my-theme)))
    (run-hooks 'edd-load-theme-hook)))

;; Mode line I like.
(display-time-mode t)
(setq display-time-string-forms
      '((propertize (concat day " " monthname " " 24-hours ":" minutes " " load))))
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
