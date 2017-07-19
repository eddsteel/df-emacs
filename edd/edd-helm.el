S(use-package helm
  :ensure helm-ag
  :ensure helm-company
  :ensure helm-projectile
  :ensure swiper-helm
  :diminish helm-mode
  :config
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
  (helm-mode 1)
  :bind
  (("C-x b" . helm-mini)
   ("C-M-y" . helm-show-kill-ring)
   ("M-x" . helm-M-x)
   ("C-x C-m" . helm-M-x)
   ("C-x C-f" . helm-find-files)
   ("C-x f" . helm-for-files)
   ("M-i" . helm-imenu)))

(use-package helm-swoop
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
