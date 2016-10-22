(use-package helm
  :ensure t
  :ensure helm-ag
  :ensure helm-company
  :ensure helm-projectile
  :ensure swiper-helm
  :diminish helm-mode
  :init
  (defun edd-theme-helm ()
    (let ((string (face-foreground 'font-lock-string-face))
          (builtin (face-foreground 'font-lock-builtin-face))
          (function (face-foreground 'font-lock-function-name-face))
          (variable (face-foreground 'font-lock-variable-name-face))
          (keyword (face-foreground 'font-lock-keyword-face))
          (background (face-background 'default))
          (highlight (face-background 'highlight))
          (mode-bg (face-background 'mode-line))
          (imode-fg (face-foreground 'mode-line-inactive)))
      (set-face-attribute 'helm-candidate-number nil :background string :foreground background)
      (set-face-attribute 'helm-grep-file nil :foreground builtin :underline t)
;;      (set-face-attribute 'helm-grep-finish nil :foreground function)
;;    (set-face-attribute 'helm-grep-lineno nil :foreground variable)
;;    (set-face-attribute 'helm-grep-match nil :foreground string)
;;    (set-face-attribute 'helm-grep-running nil :foreground variable)
      (set-face-attribute 'helm-prefarg nil :foreground function)
      (set-face-attribute 'helm-selection nil :background highlight :foreground keyword :underline t)
      (set-face-attribute 'helm-source-header nil :background background :foreground imode-fg :weight 'bold :height 1.3 :family "Sans Serif")
      (set-face-attribute 'helm-visible-mark nil :background imode-fg)))
  (add-hook 'edd-load-theme-hook 'edd-theme-helm)
  (eval-after-load 'company
    '(progn
       (define-key company-mode-map (kbd "C-M-i") 'helm-company)
       (define-key company-active-map (kbd "C-M-i") 'helm-company)))
  (eval-after-load 'projectile
    '(progn
       (setq projectile-completion-system 'helm)
       (helm-projectile-on)))
  (eval-after-load 'imenu-anywhere
    '(global-set-key (kbd "C-c ,") 'helm-imenu-anywhere))
  :config
  (helm-mode 1)
  (edd-theme-helm)
  :bind
  (("C-x b" . helm-mini)
   ("C-M-y" . helm-show-kill-ring)
   ("M-x" . helm-M-x)
   ("C-x C-m" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x f" . helm-for-files)))

(use-package helm-swoop
  :ensure t
  :config
  ;; When doing isearch, hand the word over to helm-swoop
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  ;; From helm-swoop to helm-multi-swoop-all
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  ;; When doing evil-search, hand the word over to helm-swoop
  ;; (define-key evil-motion-state-map (kbd "M-i") 'helm-swoop-from-evil-search)

  ;; Instead of helm-multi-swoop-all, you can also use helm-multi-swoop-current-mode
  (define-key helm-swoop-map (kbd "M-m") 'helm-multi-swoop-current-mode-from-helm-swoop)

  ;; Move up and down like isearch
  (define-key helm-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-swoop-map (kbd "C-s") 'helm-next-line)
  (define-key helm-multi-swoop-map (kbd "C-r") 'helm-previous-line)
  (define-key helm-multi-swoop-map (kbd "C-s") 'helm-next-line)
;;  :bind
;;  (("M-i" . helm-swoop)
;;   ("M-I" . helm-swoop-back-to-last-point)
;;   ("C-c M-i" . helm-multi-swoop)))
  )

(provide 'edd-helm)
