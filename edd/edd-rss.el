(use-package elfeed
  :bind ("C-c f" . elfeed)
  :config
  (elfeed-org))
(use-package elfeed-org
  :init
  (setq rmh-elfeed-org-files (list (locate-user-emacs-file "edd/feeds.org"))))
(use-package elfeed-goodies)

(provide 'edd-rss)
