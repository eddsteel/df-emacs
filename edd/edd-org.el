(use-package org
  :mode ("\\.org\\'" . org-mode)
  :ensure org-plus-contrib
  :ensure graphviz-dot-mode
  :config
  (setq org-html-validation-link nil)
  (setq org-html-head-include-default-style nil)
  (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
  (setq org-confirm-babel-evaluate nil
        org-src-fontify-natively t
        org-src-tab-acts-natively t)

  (defun orgfile (file)
    (expand-file-name (concat file ".org") org-directory))
  (appt-activate 1)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((clojure . t)
     (css . t)
     (ditaa . t)
     (dot . t)
     (emacs-lisp . t)
     (haskell . t)
     (http . t)
     (js . t)
     (ledger . t)
     (plantuml . t)
     (python . t)
     (ruby . t)
     (scala . t)
     (sh . t)
     (shell . t)
     (sqlite . t)))

  (setq org-src-fontify-natively t)
  (setq org-log-done t)
  (setq org-hide-leading-stars t)
  (setq org-use-speed-commands t)
  (setq org-directory "~/.org")
  (setq org-ellipsis "…")
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "DELEGATED(l@)")))
  (setq org-refile-targets
        `((nil . (:level . 1)) ; current buffer headlines
          (,(mapcar 'orgfile '("work" "home" "read-review")) . (:tag . "in"))))
  (setq org-capture-templates
        `(("t" "todo" entry (file ,(orgfile "refile"))
           "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
          ("w" "org-protocol" entry plain (file+headline ,(orgfile "read-review") "unread")
           "** TODO review %c\n%U\n" :immediate-finish t)
          ("u" "URL to read" plain (file+headline ,(orgfile "read-review") "unread")
           "[[%i]] %t\n")))
  (setq org-agenda-files (mapcar 'orgfile '("work" "dorp/home" "cal-home" "cal-work")))
  (setq org-agenda-include-diary t)
  (setq org-agenda-work-files
        (mapcar 'orgfile '("work" "cal-work")))
  (setq org-agenda-custom-commands
        '(("w" "work agenda"
           ((agenda "" ((org-agenda-ndays 1)))
            (tags-todo "next")
            (todo "WAIT")
            (tags "goal"))
           ((org-agenda-files org-agenda-work-files)
            (org-agenda-compact-blocks t)))
          ("s" "standup"
           ((todo "DONE")
            (tags-todo "next")
            (todo "WAIT"))
           ((org-agenda-files org-agenda-work-files)
            (org-agenda-compact-blocks t)))))
  (define-key org-mode-map (kbd "M-p") 'org-shiftmetaup)
  (define-key org-mode-map (kbd "M-n") 'org-shiftmetadown)
  (define-key org-mode-map (kbd "C-M-o") 'org-insert-heading)
  (add-hook 'org-mode-hook (lambda () (hl-line-mode t)))
  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c c" . org-capture)))

(use-package weather-metno
  :ensure t
  :commands weather-metno-forecast
  :init
  (setq weather-metno-location-name "Vancouver, Canada"
        weather-metno-location-latitude 49
        weather-metno-location-longitude -123)
  (setq weather-metno-get-image-props
        '(:width 16 :height 16 :ascent center)))

(use-package ox-confluence
  :commands (org-confluence-export-as-confluence edd-ox-confluence)
  :config
  (defun edd-ox-confluence ()
    (interactive)
    (org-confluence-export-as-confluence)
    (beginning-of-buffer)
    (replace-regexp "`\\([^']*\\)'" "{{\\1}}")
    (beginning-of-buffer)
    (replace-string "=/=" "}}/{{")
    (beginning-of-buffer)
    (replace-string "[" "\\[")
    (beginning-of-buffer)
    (replace-string "]" "\\]")
    (beginning-of-buffer)
    (replace-regexp "\\\\\\[\\(http.*\\)\\\\\\]" "[\\1]")))

(use-package ox-reveal
  :config
  (setq org-reveal-title-slide-template
          "<h1>%t</h1>
<p><strong>%a</strong>, Platform Team</p>
<p>%e</p>")
  (setq org-reveal-transition "fade"))

(use-package org-notmuch)
(use-package edd-org-gcal
  :commands (org-gcal-refresh-token org-gcal-fetch))


(use-package ob-http
  :ensure t
  :defer nil
  :init
  (add-to-list 'org-babel-load-languages '(http . t)))

(use-package edd-gtd
  :load-path "~/.gtd")

(provide 'edd-org)
