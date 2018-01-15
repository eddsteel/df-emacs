;; Mostly taken from https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
;;

(use-package ivy
  :ensure ivy-hydra
  :ensure counsel
  :ensure counsel-projectile
  :ensure rg
  :diminish " 🌱"
  :bind
  ("C-'" . avy-goto-char-timer)
  ("M-x" . counsel-M-x)
  ("C-x C-m" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-M-." . counsel-grep-or-swiper)
  ("C-M-y" . counsel-yank-pop)
  ("M-i" . counsel-imenu)
  :init
  (with-eval-after-load 'ido
    (ido-mode -1)
    ;; Enable ivy
    (ivy-mode 1))
  (ivy-mode 1)

  :config
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 10)
  ;; does not count candidates
  (setq ivy-count-format "%d/%d ")
  ;; allow selecting and using value in prompt
  (setq ivy-use-selectable-prompt t)

  ;; turns out this was the only thing I miss from helm
  (defun edd-ivy-updir ()
    "for ivy file prompts, either delete back to subdirectory prompt or go up a directory"
    (interactive)
    (when ivy--directory
      (if (= (minibuffer-prompt-end) (point))
          (ivy-backward-delete-char)
        (while (not (= (minibuffer-prompt-end) (point)))
          (ivy-backward-kill-word)))))

  (define-key ivy-minibuffer-map (kbd "C-l") 'edd-ivy-updir)

  (with-eval-after-load 'info
    (define-key Info-mode-map (kbd "C-s") 'isearch-forward))
  (with-eval-after-load 'company
    (define-key company-mode-map (kbd "C-M-i") 'counsel-company)
    (define-key company-active-map (kbd "C-M-i") 'counsel-company))
  (with-eval-after-load 'projectile
    (setq projectile-completion-system 'ivy)
    (counsel-projectile-mode))
  (eval-after-load 'imenu-anywhere
    '(global-set-key (kbd "C-c ,") 'ivy-imenu-anywhere))

  (define-key isearch-mode-map (kbd "M-i") 'swiper-from-isearch))

;; Need to use
;; https://raw.githubusercontent.com/abo-abo/helm-make/master/helm-make.el
;; for ivy support
;;
(use-package helm-make
  :ensure nil
  :commands (helm-make helm-make-projectile)
  :init
  (setq helm-make-completion-method 'ivy)
  :pin manual)


(provide 'edd-ivy)
