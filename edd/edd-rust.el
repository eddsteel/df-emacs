(use-package rust-mode
  :ensure t)

(use-package cargo
  :ensure t)

(use-package toml :ensure t)
(use-package toml-mode :ensure t)
(use-package flycheck-rust
  :ensure t
    :init
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))


(use-package ac-racer
  :init
  (setq racer-cmd (expand-file-name "~/.cargo/bin/racer"))
  (setq racer-rust-src-path (expand-file-name "~/src/rustc-1.9.0/src"))
  (add-hook 'rust-mode-hook #'ac-racer-setup)
  :commands (ac-racer-setup)
  :ensure t)

(provide 'edd-rust)
