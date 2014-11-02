(require 'org-install)
(require 'ox-reveal)
(require 'org-gcal)
(require 'org-protocol)
(require 'org-notmuch)

(setq weather-metno-location-name "Vancouver, Canada"
      weather-metno-location-latitude 49
      weather-metno-location-longitude -123)
(setq weather-metno-get-image-props
      '(:width 16 :height 16 :ascent center))

(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((dot . t)))


(defun edd-gitlab-project (project)
  (concat gl-root project))

(defun edd-gitlab-file (project file)
  (concat (edd-gitlab-project project) "/blob/master/" file))  

(defun edd-gitlab-commit (project commit)
  (concat (edd-gitlab-project project) "/commit/" commit))

(defun edd-gitlab-line (project file line)
  (concat (edd-gitlab-file project file) "#L" line))

(defun org-link-abbrev-gitlab-commit (tag)
  (and (string-match "^\\(.*\\)/\\([^/]*\\)$" tag)
       (let ((project (match-string 1 tag))
             (commit (match-string 2 tag)))
         (edd-gitlab-commit project commit))))
             
(defun org-link-abbrev-gitlab-line (tag)
  (and (string-match "^\\(.*\\)/\\(.*\\)#\\([[:digit:]]+\\)$" tag)
       (let ((project (match-string 1 tag))
             (file (match-string 2 tag))
             (line (match-string 3 tag)))
         (edd-gitlab-line project file line))))
             
(defun org-link-owly (tag)
  (and (string-match "^\\(.*\\)#\\([[:digit:]]+\\)$" tag)
       (let ((file (match-string 1 tag))
             (line (match-string 2 tag)))
         (edd-gitlab-line "jonasc/owly_svnimport" file line))))


(setq org-link-abbrev-alist
      '(("j" . "https://jira.hootsuitemedia.com/browse/")
        ("gh" . "https://github.com/")
        ("gr" . "https://gitlab.hootsuitemedia.com/%s/tree/master")
        ("gl" . org-link-abbrev-gitlab-line)
        ("ol" . org-link-owly)
        ("gc" . org-link-abbrev-gitlab-commit)))

      
(defun my-org-confirm-babel-evaluate (lang body)
  (not (string= lang "dot")))  ; don't ask for dot
(setq org-confirm-babel-evaluate 'my-org-confirm-babel-evaluate)
(setq org-src-fontify-natively t)

(setq org-log-done t)
(setq org-directory "~/.org")
(defun orgfile (file)
  (concat org-directory "/" file ".org"))

(setq org-default-notes-file (orgfile "in"))
(setq org-mobile-inbox-for-pull (orgfile "in"))
(setq org-mobile-directory "/org@eddandkrista.com:mobile")
(setq org-agenda-files (mapcar 'orgfile '("work" "home" "cal-home" "cal-work" "cal")))

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
        
(setq org-hide-leading-stars t)
(setq org-use-speed-commands t)

(autoload 'org-confluence-export-as-confluence "ox-confluence")

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
  (replace-regexp "\\\\\\[\\(http.*\\)\\\\\\]" "[\\1]"))



(load-secrets "org-gcal")
(appt-activate 1)

(run-at-time 3300 3300 'org-gcal-refresh-token)
(run-at-time 900 900 'org-gcal-fetch)

;; Agenda
;;
;; This is great: http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
;;
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

(provide 'edd-org)
