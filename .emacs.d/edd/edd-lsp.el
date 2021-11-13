;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
(setq lsp-keymap-prefix "s-l")

(use-package lsp-mode
  :hook ((haskell-mode . lsp)
         (java-mode . lsp)
         (scala-mode . lsp)
         (kotlin-mode . lsp)
         (rust-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

(use-package lsp-ui :commands lsp-ui-mode)
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
(use-package dap-mode)

(use-package lsp-haskell)
(use-package lsp-java)
(use-package lsp-metals)
(use-package lsp-rust-server)
