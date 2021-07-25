(straight-use-package '(org-plus-contrib :includes (org)))

(defun org-plus-contrib ())

(use-package org
  :defer
  :straight org-plus-contrib
  :mode ("\\.(org\\|org.txt)\\'" . org-mode)
  :hook ((org-mode . #'turn-on-visual-line-mode)
         (org-mode . (lambda () (org-bullets-mode 1)))
         (org-mode . (lambda () (org-display-inline-images t t)))
         (org-mode . (lambda ()
                       "Beautify Org Checkbox Symbol"
                       (push '("[ ]" . "☐") prettify-symbols-alist)
                       (push '("[X]" . "☑" ) prettify-symbols-alist)
                       (push '("[-]" . "❍" ) prettify-symbols-alist)
                       (prettify-symbols-mode))))

  :commands (org-confluence-export-as-confluence edd-ox-confluence)
  :init
  (when (eq 'darwin system-type)
    (add-to-list 'org-modules 'org-mac-iCal))
  ;;  (add-to-list 'org-modules 'ox-confluence)
  (load-file (locate-user-emacs-file "edd/edd-org-options.el"))
  :config
  (require 'ob-async)
  (setq org-bullets-bullet-list
        '("​" "​" "​" "​" "​" "​" "​" "​"))

  (defun edd/create-ticket-notes (project number)
    (let ((url (concat "[[j:" project "-" number "]]"))
          (file (concat "~/txt/work-notes/" project "/" number ".org")))
      (find-file file)
      (beginning-of-buffer)
      (insert "#+TITLE:" project "-" number)
      (newline-and-indent)
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

  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)
  (load-file
   (expand-file-name
    (concat user-emacs-directory
            "edd/edd-org-options.el")))

  :bind
  (("C-c l" . org-store-link)
   ("C-c a" . org-agenda)
   ("C-c b" . org-iswitchb)
   ("C-c c" . org-capture)
   ("C-c j" . org-journal-new-entry))

  :custom-face
  (org-document-title ((nil :height 1.0)))
  (org-agenda-structure ((nil :height 1.0)))
  (org-link ((nil :foreground "#06d8ff")))
  (org-verbatim ((nil :inherit font-lock-keyword-face)))
  (org-block-begin-line ((nil :background "#444444")))
  (org-block-end-line ((nil :background "#444444")))
  (org-block ((nil :background "#444444")))
  (org-checkbox-done-text ((t (:foreground "#71696A" :strike-through t)))))


(use-package weather-metno
  :commands weather-metno-forecast
  :after org-agenda
  :init
  (setq weather-metno-location-name "Vancouver, Canada"
        weather-metno-location-latitude 49
        weather-metno-location-longitude -123)
  (setq weather-metno-get-image-props
        '(:width 16 :height 16 :ascent center)))

(use-package ob-http
  :init
  (org-babel-do-load-languages 'org-babel-load-languages '((http . t))))

(use-package ob-async)
(use-package ob-kotlin)

(use-package interleave
  :config
  (setq interleave-org-notes-dir-list '("." "~/txt/notes")))

(use-package org-journal
  :config
  (setq org-journal-dir "~/txt/journal")
  (setq org-journal-date-format "%A, %d/%m")
  (setq org-journal-file-format "%Y%m%d.org"))

(use-package org-beautify-theme
  :init
  (load-theme 'org-beautify t))

(use-package ox-gfm
  :after org
  :commands org-gfm-export-to-markdown)

(use-package graphviz-dot-mode)
(use-package htmlize)
(use-package org-download)
(use-package org-bullets)

(provide 'edd-org)

