(use-package haskell-mode
  :ensure t
  :mode "\\.hs\\'"
  :init
  (dolist (hook '(haskell-indentation-mode
                  haskell-doc-mode
                  haskell-decl-scan-mode))
    (add-hook 'haskell-mode-hook hook))
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))
  (setq
   company-ghc-show-info t
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
    (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space))
  :bind
  ("C-c h h" . switch-to-haskell))

(use-package "ghc"
  :ensure t
  :commands (ghc-init ghc-debug)
  :init
  (add-hook 'haskell-mode-hook 'ghc-init))


(provide 'edd-haskell)
