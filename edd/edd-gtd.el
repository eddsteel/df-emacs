(appt-activate 1)

(require 'org)
;(require 'org-protocol)

;; Log state changes
(setq org-log-done t)
(setq org-hide-leading-stars t)
(setq org-use-speed-commands t)
(setq org-directory "~/txt/gtd")
(setq org-ellipsis "…")


;; Action state changes
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "WAIT(w@/!)" "|" "DONE(d!)" "DELEGATED(l@)")
        (sequence "INPROG(i)" "PR(p)" "DEV(v)" "STG(s)" "PRD(x)")))

;; Project tags are non-hierarchical
(setq org-tags-exclude-from-inheritance
      '("project"))

;; Refilen
;; - tickler (the only heading)
;; - calendar (:inbox:)
;; - any project in projects
;; - back to the inbox
;;
(setq org-refile-targets
      `((nil . (:level . 1)) ; current buffer headlines
        ("tickler.org" . (:level . 1))
        ("calendar.org" . (:tag . "inbox"))
        ("projects.org" . (:tag . "project"))
        ("projects.org" . (:tag . "inbox"))
        ("inbox.org" . (:tag . "inbox"))
        ("someday.org" . (:level . 1))))


;; Capture to inbox or Read/Review
;;
(setq org-capture-templates
      `(("t" "todo" entry (file+headline "inbox.org" "In")
         "* TODO %?\n%a\n")
        ("r" "retro item" plain (file "~/txt/work-notes/retro.org")
         "* ")
        ("u" "URL to read" plain (file "read-review.org")
           "[[%i]] %t\n")

        ("p" "Protocol" entry (file "read-review.org")
         "* %^{description}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
        ("L" "Protocol Link" entry plain (file "read-review.org")
         "* [[%u][%^{description}]]")))

(setq org-agenda-files
      (mapcar (lambda (f) (concat org-directory "/" f))
              '("calendar.org" "tickler.org" "projects.org" "birthdays.org")))

;; Stuck unless
;; - there's a next action
;; - the next action has a date (STAY)
;; - it's in the tickler
;; - it's to pick something up while out
(setq org-stuck-projects
      '("+project/-tickler-maybe" ("NEXT" "STAY") ("tickler" "store") ""))

;; Thanks http://stackoverflow.com/questions/10074016/org-mode-filter-on-tag-in-agenda-view
;;
(defun edd/org-agenda-filter-tag (tag)
  "Include only entries that correspond to TAG"
  (let ((next-headline (save-excursion (or (outline-next-heading) (point-max))))
        (current-headline (or (and (org-at-heading-p)
                                   (point))
                              (save-excursion (org-back-to-heading)))))
    (progn
      (if (member tag (org-get-tags-at current-headline))
          nil
          next-headline))))

(defun edd/go-to-work ()
  (interactive)
  (progn
    (find-file "~/txt/gtd/projects.org")
    (widen)
    (beginning-of-buffer)
    (search-forward-regexp "^* Work Projects")
    (org-narrow-to-element)))

(defun edd/go-home ()
  (interactive)
  (progn
    (find-file "~/txt/gtd/projects.org")
    (widen)
    (beginning-of-buffer)
    (search-forward-regexp "^* Projects")
    (org-narrow-to-element)))

(setq org-agenda-custom-commands
      '(("p" "project list"
         ((tags "project"))
         ((org-show-following-heading nil)
          (org-show-hierarchy-above nil)
          (org-show-context 'minimal))
         )
        ("n" "actions"
          ((tags-todo "-work&TODO=\"NEXT\""))
          ((org-agenda-compact-blocks t))
          )
        ("work" . "work things")
        ("ws" "work sup"
         ((tags "work&TODO=\"NEXT\"")
          (tags "work&TODO=\"INPROG\"")
          (tags "work&TODO=\"PR\"")
          (tags "work&TODO=\"DEV\"")
          (tags "work&TODO=\"STG\"")
          (agenda ""
                  ((org-agenda-span 3)
                   (org-agenda-start-day "-1d")
                   (org-agenda-skip-function '(edd/org-agenda-filter-tag "work")))))

         (
          (org-agenda-compact-blocks t))
          (org-agenda-tag-filter "work"))
        ("wa" "work agenda"
         ((agenda ""
                  ((org-agenda-span 3)
                   (org-agenda-start-day "-1d")
                   (org-agenda-skip-function '(edd/org-agenda-filter-tag "work")))))

         ((org-agenda-compact-blocks t))
         (org-agenda-tag-filter "work"))
        ("wS" "standup"
         ((tags "work&TODO=\"DONE\"")
          (tags "work&TODO=\"NEXT\"")
          (tags "work&TODO=\"INPROG\"")
          (tags "work&TODO=\"DEV\"")
          (tags "work&TODO=\"PR\"")
          (tags "work&TODO=\"STG\"")
          (tags "work&TODO=\"PRD\"")
          (agenda ""
                  ((org-agenda-span 2)
                   (org-agenda-start-day "-1d")
                   (org-agenda-skip-function '(edd/org-agenda-filter-tag "work"))))
          (tags "work&TODO=\"TODO\""))
         (
          (org-agenda-compact-blocks t))
          (org-agenda-tag-filter "work"))
        ("wp" "work project list"
         ((tags "+work+project"))
         ((org-show-following-heading nil)
          (org-show-hierarchy-above nil)
          (org-show-context 'minimal))
         )
        ("wn" "work next actions"
         ((tags-todo "work&TODO=\"NEXT\""))
         ((org-agenda-compact-blocks t)))))

(if (eq 'darwin system-type)
  (add-to-list 'org-agenda-custom-commands
               '("I" "Import from ical" agenda "" ((org-agenda-mode-hook (lambda () (edd-mac/agenda-iCal)))))))

(setq org-agenda-default-appointment-duration 60)
(setq org-icalendar-timezone "America/Vancouver")
(setq org-icalendar-use-deadline '(event-if-todo todo-due))
;; diary stuff
(setq org-agenda-diary-file diary-file)
(add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
(add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)
(setq org-agenda-include-diary t)

;; a no-dep "import all my configured calendars" deal
;;
(defun edd/batch-import-calendars ()
  (interactive)
  (require 'edd-secrets (expand-file-name "~/.emacs.d/edd/edd-secrets.el"))
  (edd-with-secrets "gcal"
                      (dolist (pair edd/calendars)
                      (edd/download-calendar (car pair) (cdr pair)))))

;; Download given URL and convert to diary format inside org dir.
;;
;; thanks jeff https://github.com/jstautz/.emacs.d/blob/9b2e405ddc3733630699179697d1c57c9f59032d/init-custom-functions.el#L22
;;
(defun edd/download-calendar (label url)
  (let ((diaryfile (concat org-directory "/" label ".diary"))
        (tmpfile (url-file-local-copy url)))
      (find-file diaryfile)
      (flush-lines "^[& ]")
      (icalendar-import-file tmpfile diaryfile t)
      (kill-buffer (current-buffer))
      (kill-buffer (current-buffer))
      (delete-file tmpfile)))

;; make projects.org export on save (use local hook)
(defun edd-gtd-export-projects-on-save ()
  (add-hook 'after-save-hook #'org-html-export-to-html nil t))

(defun edd-gtd-attach-export-hook ()
  (when (string= buffer-file-name
           (expand-file-name (concat org-directory "/" "projects.org")))
    (edd-gtd-export-projects-on-save)))

(add-hook 'org-mode-hook #'edd-gtd-attach-export-hook)

(provide 'edd-gtd)
