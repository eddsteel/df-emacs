;; Mostly taken from https://sam217pa.github.io/2016/09/13/from-helm-to-ivy/
;;
(use-package ivy
  :ensure ivy-hydra
  :ensure counsel
  :ensure counsel-projectile
  :ensure rg
  :ensure projectile
  :ensure ido
  :ensure company
  :after company
  :delight
  (ivy-mode " 🌱")
  :demand
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
   ("C-c ," . ivy-imenu-anywhere)
   ([remap xref-find-references] . edd/find-rg-references-projectile)
   :map minibuffer-local-map
   ("C-r" . counsel-minibuffer-history)
   :map ivy-minibuffer-map
   ("C-l" . 'edd-ivy/updir)
   :map Info-mode-map
   ("C-s" . isearch-forward)
   :map company-mode-map
   ("C-M-i" . 'counsel-company)
   :map company-active-map
   ("C-M-i" . 'counsel-company)
   :map isearch-mode-map
   ("M-i" . swiper-from-isearch))

  :init
  (ivy-mode)

  :config
  (ido-mode -1)
  (counsel-projectile-mode)

  (setq
   projectile-completion-system 'ivy
   enable-recursive-minibuffers t
   ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
   ivy-use-virtual-buffers t
   ;; number of result lines to display
   ivy-height 30
   max-mini-window-height 30
   ivy-count-format "%d/%d "
   ivy-use-selectable-prompt t
   counsel-projectile-rg-initial-input '(projectile-symbol-or-selection-at-point))

  (add-to-list 'ivy-height-alist '(swiper . 15))

  (defhydra+ hydra-project nil "Project"
    ("a"   counsel-projectile-rg "rg")
    ("b"   counsel-projectile-switch-to-buffer "buffer")
    ("d"   counsel-projectile-find-dir "find dir")
    ("f"   counsel-projectile-find-file "find file"))


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


  ;; turns out this was the only thing I miss from helm
  (defun edd-ivy/updir ()
    "for ivy file prompts, either delete back to subdirectory prompt or go up a directory"
    (interactive)
    (when ivy--directory
      (if (= (minibuffer-prompt-end) (point))
	  (ivy-backward-delete-char)
	(while (not (= (minibuffer-prompt-end) (point)))
	  (ivy-backward-kill-word))))))

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
