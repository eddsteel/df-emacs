(defun kill-region-or-backward-kill-word (&optional arg region)
  "`kill-region' if the region is active, otherwise `backward-kill-word'"
  (interactive (list (prefix-numeric-value current-prefix-arg) (use-region-p)))
  (if region
      (kill-region (region-beginning) (region-end))
    (backward-kill-word arg)))

;; useful for goto-address-mode, will fail
;; outside it.
(defun edd-jump-to-next-url ()
  (interactive)
  (point-at-eol) ; so we don't jump to the end of current URL
  (search-forward-regexp goto-address-url-regexp)
  (backward-char))

(defun edd-jump-to-prev-url ()
  (interactive)
  (point-at-bol)
  (search-backward-regexp goto-address-url-regexp)
  (forward-char))

(defun edd-initial-file-or-scratch ()
  (interactive)
  "Finds the initial buffer choice file, or if that is nil, the scratch buffer"
  (if initial-buffer-choice
      (find-file initial-buffer-choice)
    (switch-to-buffer "*scratch*")))

(defun edd-term (pfx)
  (interactive "p")
  "Open my currently favourite kind of terminal, smartly.

   With the prefix argument, opens term.
   If the current buffer is an ansi-term, opens a new one.
   If there's no ansi-term, open a new one.
   Otherwise will switch to *ansi-term*"
  (let ((bn (buffer-name))
        (tl "*ansi-term*")
        (newterm (lambda () (ansi-term "bash"))))
    (if (and (<= pfx 1) (get-buffer tl) (not (string-prefix-p tl bn)))
        (switch-to-buffer tl)
      (funcall newterm))))

(defun edd-hex-encode (start end)
  (interactive "r")
  "replace the region with a URL encoded version"
  (let* ((text (buffer-substring-no-properties start end))
         (encoded (url-hexify-string text)))
    (delete-region start end)
    (insert encoded)))

;; From https://github.com/sri/dotfiles/blob/master/emacs/emacs.d/my-fns.el#L236
;;
(defun edd-sudo-ff ()
  "Use TRAMP to `sudo' the current buffer"
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (find-alternate-file (concat "/sudo::" file-name)))))

;; Exercism submit from project root
(with-eval-after-load "projectile"
  (defun edd-ex-submit ()
    (interactive)
    (let ((file (buffer-file-name))
          (default-directory (projectile-project-root)))
      (display-buffer (process-buffer
     (start-process "exercism" "*exercism*" "exercism" "submit" file))))))


;; This is rad, copied from
(defun edd-fill-or-unfill ()
  "Like `fill-paragraph', but unfill if used twice."
  (interactive)
  (let ((fill-column
         (if (eq last-command 'edd-fill-or-unfill)
             (progn (setq this-command nil)
                    (point-max))
           fill-column)))
    (call-interactively #'fill-paragraph)))

(global-set-key [remap fill-paragraph]
                #'edd-fill-or-unfill)

;; From https://www.emacswiki.org/emacs/ReverseParagraphs
;;
(defun edd-reverse-paragraphs (beg end)
  "Reverse the order of paragraphs in a region.
From a program takes two point or marker arguments, BEG and END."
  (interactive "r")
  (when (> beg end)
    (let (mid) (setq mid end end beg beg mid)))
  (save-excursion
    ;; the last paragraph might be missing a trailing newline
    (goto-char end)
    (setq end (point-marker))
    ;; the real work.
    (goto-char beg)
    (let (paragraphs fix-newline)
      (while (< beg end)
        ;; skip to the beginning of the next paragraph instead of
        ;; remaining on the position separating the two paragraphs
        (when (= 0 (forward-paragraph 1))
          (goto-char (1+ (match-end 0))))
        (when (> (point) end)
          (goto-char end))
        (setq paragraphs (cons (buffer-substring beg (point))
                               paragraphs))
        (delete-region beg (point)))
      ;; if all but the last paragraph end with two newlines, add a
      ;; newline to the last paragraph
      (when (and (null (delete 2 (mapcar (lambda (s)
                                           (when (string-match "\n+$" s -2)
                                             (length (match-string 0 s))))
                                         (cdr paragraphs))))
                 (when (string-match "\n+$" (car paragraphs) -2)
                   (= 1 (length (match-string 0 (car paragraphs))))))
        (setq fix-newline t)
        (setcar paragraphs (concat (car paragraphs) "\n")))
      ;; insert paragraphs
      (dolist (par paragraphs)
        (insert par))
      (when fix-newline
        (delete-char -1)))))

;; From http://zck.me/emacs-move-file
;;
(defun edd-move-file (new-location)
  "Write this file to NEW-LOCATION, and delete the old one."
  (interactive (list (if buffer-file-name
                         (read-file-name "Move file to: ")
                       (read-file-name "Move file to: "
                                       default-directory
                                       (expand-file-name (file-name-nondirectory (buffer-name))
                                                         default-directory)))))
  (when (file-exists-p new-location)
    (delete-file new-location))
  (let ((old-location (buffer-file-name)))
    (write-file new-location t)
    (when (and old-location
               (file-exists-p new-location)
               (not (string-equal old-location new-location)))
      (delete-file old-location))))


(provide 'edd-util)
