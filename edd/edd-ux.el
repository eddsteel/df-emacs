;; remove distractions.
(dolist
    (mode '(menu-bar-mode tool-bar-mode scroll-bar-mode tooltip-mode))
  (when (fboundp mode) (funcall mode -1)))
(mouse-avoidance-mode 'jump)

;; theme
;;
(use-package darkokai-theme
  :init
  (load-theme 'darkokai t))

;; don't yell.
(setq visible-bell nil) ;; The default
(setq ring-bell-function 'ignore)

;; don't insist.
(defalias 'yes-or-no-p 'y-or-n-p)

;; show me where I am.
(column-number-mode t)
(show-paren-mode t)

(add-hook 'prog-mode-hook (lambda () (hl-line-mode t)))
(add-to-list 'default-frame-alist '(height . 124))
(add-to-list 'default-frame-alist '(width . 82))

;; use when themes don't set hl-line
(defun edd-fix-hl-line-mode ()
  (interactive)
  (set-face-background 'hl-line (face-background 'highlight)))

(defun edd-prep-frame (frame)
  (with-selected-frame frame
    (when (display-graphic-p)
      (progn
        (if (eq 'darwin system-type)
          (progn
            ;; fade when inactive
            (set-frame-parameter (selected-frame) 'alpha '(100 80))
            (add-to-list 'default-frame-alist '(font . "FuraCode Nerd Font-12"))
            (put 'default-frame-alist 'alpha '(100 80)))
          (progn
          (set-face-attribute 'default nil :font "FuraCode Nerd Font-11")
          (set-face-attribute 'fixed-pitch nil :font "FuraCode Nerd Font-11")))))))

;; Mode line I like.
(display-time-mode 1)
(setq display-time-string-forms
      '((propertize (concat "📆 " day " " (substring monthname 0 3) " " 24-hours ":" minutes " " load))))
(display-battery-mode t)
(setq battery-mode-line-format " %b%p%%")

;; Modeline
;;
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

;;(use-package mode-icons
;;  :config
;;  (setq mode-icons-change-mode-name nil)
;;  (mode-icons-mode))

(use-package nyan-mode
    :init
    (require 'midnight)
    (midnight-mode)
    :config
    (require 'time-date)
    (setq nyan-wavy-trail 1)
    ;; animate on Wednesdays
    (when
        (string-match-p "Wed.*" (current-time-string))
      (nyan-start-animation))
    ;; animate on Wednesdays without restart
    (add-hook
     'midnight-hook
     (lambda ()
       (if
           (string-match-p "Wed.*" (current-time-string))
           (nyan-start-animation)
         (nyan-stop-animation))))
    (nyan-mode 1))

;; comfortable bindings
;; C-h for delete
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))
;; C-z for help, in exchange
(define-key key-translation-map (kbd "C-z") (kbd "<help>"))

(use-package "basic-theme"
  :commands edd-ux/basic-mode
  :config
  (defun edd-ux/basic-mode ()
    "Get super basic"
    (interactive)
    (enable-theme 'basic)
    (setq-default mode-line-format "")
    (setq mode-line-format "")
    (let ((faces-to-toggle '(mode-line mode-line-inactive mode-line-highlight mode-line-emphasis)))
      (mapcar (lambda (face)
                (set-face-attribute face nil :height 100))
              faces-to-toggle))
    (let* ((sans-font (cond ((x-list-fonts "Lucida Grande") '(:font "Lucida Grande"))
                            ((x-list-fonts "Verdana") '(:font "Verdana"))
                            ((x-family-fonts "Sans Serif") '(:family "Sans Serif"))))
           (background-color (face-background 'default nil 'default))
           (padding `(:line-width 5 :color ,background-color)))
      (custom-theme-set-faces 'org-beautify
                              `(org-level-3 ((t (:box ,padding))))
                              `(org-level-2 ((t (:box ,padding))))
                              `(org-level-1 ((t (:box ,padding))))))))

;; if we're loading non-daemon set up initial frame. Otherwise the hook will get it.
(when (not (daemonp)) (edd-prep-frame (car (frame-list))))
(add-hook 'after-make-frame-functions 'edd-prep-frame)
(provide 'edd-ux)
