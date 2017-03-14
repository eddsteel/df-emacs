(use-package elfeed-org
  :init
  (setq rmh-elfeed-org-files (list (locate-user-emacs-file "edd/feeds.org"))))
(use-package elfeed-goodies)

(use-package elfeed
  :bind ("C-c f" . elfeed)
  :config
  (elfeed-org))

(provide 'edd-rss)
