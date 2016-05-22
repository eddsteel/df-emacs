;; org config that doesn't load packages

(setq org-html-validation-link nil)
(setq org-html-head-include-default-style nil)
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (css . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (haskell . t)
   (js . t)
   (ledger . t)
   (plantuml . t)
   (python . t)
   (ruby . t)
   (scala . t)
   (sh . t)
   (sqlite . t)))

(setq org-src-fontify-natively t)
(setq org-log-done t)
(setq org-hide-leading-stars t)
(setq org-use-speed-commands t)
(setq org-ellipsis "…")
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "DELEGATED(l@)")))

(define-key org-mode-map (kbd "M-p") 'org-shiftmetaup)
(define-key org-mode-map (kbd "M-n") 'org-shiftmetadown)
(define-key org-mode-map (kbd "C-M-o") 'org-insert-heading)
(add-hook 'org-mode-hook (lambda () (hl-line-mode t)))
(provide 'edd-org-options)
