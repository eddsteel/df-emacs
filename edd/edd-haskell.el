(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :init
  (dolist (hook '(haskell-doc-mode
                  haskell-indentation-mode
                  haskell-decl-scan-mode))
    (add-hook 'haskell-mode-hook hook))
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))
  (setq
;;   company-ghc-show-info t
   haskell-tags-on-save t
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t
   haskell-process-type 'cabal-repl
   haskell-process-log t)
  :config
  (progn
    (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
    (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
    (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
    (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
    (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
    (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
    (define-key haskell-mode-map (kbd "C-c DEL") 'haskell-hoogle)
    (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)
    (add-hook 'haskell-mode-hook (lambda () (helm-dash-activate-docset 'Haskell)))
    )
  :bind
  ("C-c h h" . switch-to-haskell))

(use-package "hi2"
  :ensure t
  :commands (turn-on-hi2)
  :init
    (add-hook 'haskell-mode-hook 'turn-on-hi2))

;; use cabal's ghc-mod instead of the package.
(add-to-list 'load-path "~/.cabal/share/x86_64-osx-ghc-7.6.3/ghc-mod-5.4.0.0/elisp")
(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

(provide 'edd-haskell)
