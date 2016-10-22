(use-package go-eldoc
  :ensure t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-errcheck
  :bind
  ("C-c C-b c" . go-errcheck)
  :ensure t)

(use-package flymake-go
  :ensure t
  :bind
  ("M-n" . flycheck-next-error)
  ("M-p" . flycheck-previous-error))

(use-package go-mode
  :ensure t
  :init
  (if-let ((gopath (getenv "GOPATH")))
      (prog2
          (setenv "PATH" (concat (getenv "PATH") ":" gopath "/bin"))
          (setq exec-path (append exec-path (list (concat gopath "/bin"))))))
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-to-list 'load-path "~/src/go/src/github.com/dougm/goflymake")
  (add-hook 'go-mode-hook (lambda ()
                            (setq-local tab-width 4)))
  :bind
  (:map go-mode-map
        ("C-c C-r o" . go-remove-unused-imports)
        ("M-." . godef-jump)
        ("C-M-." . godef-jump-other-window)))

(use-package company-go
  :init
  (add-hook 'go-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-go))
                            (company-mode))))

(use-package go-projectile
  :ensure t)

(provide 'edd-go)
