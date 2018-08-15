;; Mostly taken from https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
;;

(use-package ivy
  :ensure ivy-hydra
  :ensure counsel
  :ensure counsel-projectile
  :ensure rg
  :delight " 🌱"
  :bind
  (("C-'" . avy-goto-char-timer)
   ("M-x" . counsel-M-x)
   ("C-x C-m" . counsel-M-x)
   ("C-x C-f" . counsel-find-file)
   ("C-M-." . counsel-grep-or-swiper)
   ("C-M-y" . counsel-yank-pop)
   ("M-i" . counsel-imenu)
   ("C-c r" . ivy-resume)
   ("C-c u" . counsel-unicode-char)
   ("<f1> f" . counsel-describe-function)
   ("<f1> v" . counsel-describe-variable)
   ("<f1> S" . counsel-info-lookup-symbol)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history))
  :init
  (with-eval-after-load 'ido
    (ido-mode -1)
    ;; Enable ivy
    (ivy-mode 1))
  (ivy-mode 1)
  (setq enable-recursive-minibuffers t)

  :config
  ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
  (setq ivy-use-virtual-buffers t)
  ;; number of result lines to display
  (setq ivy-height 30)
  (setq max-mini-window-height 30)
  (add-to-list 'ivy-height-alist '(swiper . 15))
  ;; does not count candidates
  (setq ivy-count-format "%d/%d ")
  ;; allow selecting and using value in prompt
  (setq ivy-use-selectable-prompt t)

  (setq counsel-projectile-rg-initial-input '(projectile-symbol-or-selection-at-point))

  ;; fire once version of counsel-projectile-rg
  (defun edd/find-rg-references-projectile (&optional options)
      "Search the current project with rg for the thing under point/selected.

OPTIONS, if non-nil, is a string containing additional options to
be passed to rg. It is read from the minibuffer if the function
is called with a prefix argument."
    (interactive)
    (let* ((ignored (mapconcat (lambda (i)
                               (concat "--glob "
                                       (shell-quote-argument (concat "!" i))
                                       " "))
                             (append (projectile-ignored-files-rel)
                                     (projectile-ignored-directories-rel))
                             ""))
         (options
          (if current-prefix-arg
              (read-string (projectile-prepend-project-name "rg options: ")
                           ignored
                           'counsel-projectile-rg-options-history)
            (concat ignored options))))
      (counsel-rg (projectile-symbol-or-selection-at-point) (projectile-project-root) options (projectile-prepend-project-name "rg"))))
  (with-eval-after-load 'projectile
                       (define-key global-map [remap xref-find-references] 'edd/find-rg-references-projectile))

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
