(use-package org
  :mode ("\\.org\\'" . org-mode)
  :ensure org-plus-contrib
  :ensure ob-http
  :ensure graphviz-dot-mode
  :ensure htmlize
  :ensure org-download
  :ensure org-bullets
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'org-mode-hook (lambda () (hl-line-mode t)))

;;   most :config replaced with edd-org-options
  :config
  (setq org-bullets-bullet-list
        '("🍣" "🐸" "🐳" "🐻" "◉" "○" "✸" "✿"))
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
<p><strong>%a</strong>, Publishing Team</p>
<p>%e</p>")
  (setq org-reveal-transition "fade"))

(use-package org-notmuch)

(use-package ob-http
  :ensure t
  :defer nil
  :init
  (org-babel-do-load-languages 'org-babel-load-languages '((http . t))))

(use-package edd-org-options
  :config ;; additional
  (appt-activate 1)
  (add-to-list 'org-babel-load-languages
               '(shell . t)))

(use-package edd-gtd
  :demand
  :bind
  (("C-c w" . edd/go-to-work)))

(use-package interleave
  :ensure t
  :config
  (setq interleave-org-notes-dir-list '("." "~/txt/notes")))

(provide 'edd-org)
