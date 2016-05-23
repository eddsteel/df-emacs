(use-package rust-mode
  :ensure t)

(use-package cargo
  :ensure t
  :bind
   ("C-c C-t" . cargo-process-test)
   ("C-c C-c" . cargo-process-build))

(use-package toml :ensure t)
(use-package toml-mode :ensure t)
(use-package flycheck-rust
  :ensure t
    :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

(provide 'edd-rust)
