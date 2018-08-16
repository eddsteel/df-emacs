(use-package ruby-mode
  :ensure nil
  :delight ""
  )

(use-package web-mode)
(use-package inf-ruby
  :config
  (autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(use-package string-inflection
  :bind
  ("C-c i i" . string-inflection-all-cycle)
  ("C-c i c" . string-inflection-camelcase))

(use-package rake
  :config
  (setq rake-completion-system 'default)
  :bind
  (:map ruby-mode-map
        ("C-c s r" . rake)
        ("C-c s a" . rake-rerun)))

(provide 'edd-ruby)
