(use-package org-plus-contrib
  :mode ("\\.org\\'" . org-mode)
  :ensure graphviz-dot-mode
  :ensure htmlize
  :ensure org-download
  :ensure org-bullets
  :pin "org"
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'org-mode-hook 'turn-on-visual-line-mode)
  (add-hook 'org-mode-hook (lambda () (org-display-inline-images t t)))
  (setq org-bullets-bullet-list
        '("​" "​" "​" "​" "​" "​" "​" "​"))
;;        '("🐺" "🐸" "🐳" "🐻" "🐬" "🐤" "🐷" "🐴"))

  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c c" . org-capture)))


(use-package weather-metno
  :commands weather-metno-forecast
  :init
  (setq weather-metno-location-name "Vancouver, Canada"
        weather-metno-location-latitude 49
        weather-metno-location-longitude -123)
  (setq weather-metno-get-image-props
        '(:width 16 :height 16 :ascent center)))

(use-package ox-confluence
  :ensure nil
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

(defun edd/create-ticket-notes (project number)
  (let ((url (concat "[[j:" project "-" number "]]"))
        (file (concat "~/txt/work-notes/" project "/" number ".org")))
    (find-file file)
    (beginning-of-buffer)
    (insert "* " url)
    (newline-and-indent)))

(defun edd/parse-jira-ticket-near-point ()
  (save-excursion
    (backward-word-strictly 3)
    (re-search-forward ".*[^A-Z]\\([A-Z]+\\)-\\([0-9]+\\).*")
    (cons (match-string 1) (match-string 2))))

(defun edd/create-jira-notes ()
  (interactive)
  (let ((ticket (edd/parse-jira-ticket-near-point)))
    (edd/create-ticket-notes (car ticket) (cdr ticket))))

(use-package ob-http
  :defer
  :config
  (org-babel-do-load-languages 'org-babel-load-languages '((http . t))))

(use-package ob-async)

(use-package edd-org-options
  :ensure nil)

(use-package edd-gtd
  :ensure nil
  :commands (edd/go-home)
  :bind
  (("C-c w" . edd/go-to-work)))

(use-package interleave
  :config
  (setq interleave-org-notes-dir-list '("." "~/txt/notes")))

(use-package org-journal
  :init
  (setq org-journal-dir "~/txt/journal"))

(use-package org-beautify-theme
  :init
  (load-theme 'org-beautify))

(provide 'edd-org)
