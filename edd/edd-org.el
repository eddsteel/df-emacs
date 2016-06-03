(use-package org
  :mode ("\\.org\\'" . org-mode)
  :ensure org-plus-contrib
  :ensure graphviz-dot-mode
  :ensure htmlize
;;   most :config replaced with edd-org-options
  :config
  (setq org-directory "~/.org")
  (defun orgfile (file)
    (expand-file-name (concat file ".org") org-directory))
  (setq org-refile-targets
      `((nil . (:level . 1)) ; current buffer headlines
        (,(mapcar 'orgfile '("work" "home" "read-review")) . (:tag . "in"))))

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

(use-package edd-org-options
  :config ;; additional
  (appt-activate 1)
  (add-to-list 'org-babel-load-languages
               '(shell . t)))

(use-package edd-gtd
  :load-path "~/txt/gtd")

(use-package interleave
  :ensure t)

(provide 'edd-org)
