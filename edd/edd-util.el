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

(defun edd-term ()
  (interactive)
  "Open my currently favourite kind of terminal"
  (ansi-term "bash"))

(provide 'edd-util)
