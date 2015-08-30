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

(provide 'edd-util)
