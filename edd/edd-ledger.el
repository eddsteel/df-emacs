;; Process is:
;; - Paste from online banking sites
;; - Remove "exchange amount" if appropriate (M-x delete-matching-lines)
;; - Run appropriate clean function on each line (use macros)
;; - Run clean entries
;; - Run `tac' on the paragraphs
;; - Initialize counter (using function)
;; - Convert each line to an item using function
;; - Convert each item to be sortable
;;
;; For each site, use a different account no for the sorting
;;
;; Then sort paragraphs on whole buffer
;; Then paste into ledger file and use next-unknown to fill out details.
;;
;;
(require 'dash)

(defun edd/ledger-clean-vancity (month)
  "Clean an entry pasted from Vancity website
   MONTH is the numeric month e.g. \"02\" for February."
  (save-excursion
    (next-line)
    (delete-indentation)
    (end-of-line)
    (let ((beg (point)))
      (search-forward-regexp "-?\\$")
      (delete-region (match-beginning 0) beg))
    (search-forward "$")
    (goto-char (match-beginning 0))
    (backward-char)
    (kill-line)
    (beginning-of-line)
    (insert "2018-" month "-")
    (forward-char 2)
    (let ((beg (point)))
      (search-forward "	")
      (delete-region (point) beg))
    (insert " ")
    (move-beginning-of-line nil)
    (let ((beg (point)))
      (end-of-line)
      (capitalize-region (point) beg))))

(defun edd/ledger-clean-visa ()
  "Clean an entry pasted from Vancity VISA site"
  (save-excursion
   (beginning-of-line)
   (delete-char 1)
   (forward-sexp)
   (backward-word)
   (transpose-words 1)
   (backward-word 2)
   (transpose-words 2)
   (kill-word 3)
   (just-one-space)
   (let ((beg (point)))
     (end-of-line)
     (perform-replace "	$" " -$" nil nil nil nil nil beg (point)))
   (beginning-of-line)
   (let ((beg (point)))
     (end-of-line)
     (perform-replace "	 	 $" " $" nil nil nil nil nil beg (point)))
   (let ((beg (point)))
     (end-of-line)
     (capitalize-region (point) beg))))


(defun edd/ledger-line-to-entry (acct)
  (search-forward-regexp "-?\\$")
  (goto-char (match-beginning 0))
  (newline)
  (ledger-magic-tab)
  (insert acct "  ")
  (end-of-line)
  (newline)
  (ledger-magic-tab)
  (insert "???")
  (let ((end (point)))
    (newline)
    (backward-paragraph)
    (indent-region (point) end))
  (forward-paragraph)
  (next-line))

(defun edd/ledger-initialize-register ()
    (set-register 'edd/ledger-i 1))


(defun edd/ledger-decorate-for-sorting (acct-num)
  (increment-register 1 'edd/ledger-i)
  (forward-word 2)
  (forward-char)
  (let ((beg (point)))
    (forward-word 1)
    (kill-ring-save beg (point)))
  (insert " " acct-num)
  (yank)
  (insert (format "%03d" (get-register 'edd/ledger-i)))
  (sentence-end)
  (next-line 4)
  (beginning-of-line))

(defun edd/ledger-next-unknown ()
  (interactive)
  (search-forward "???")
  (back-to-indentation)
  (kill-line))

(defun edd/ledger-undecorate-all ()
  (beginning-of-buffer)
  (while (re-search-forward "\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\) [[:digit:]]\\{6\\}")
    (replace-match "\\1")))


(defun edd/ledger-clean-entries ()
  (-map
   (lambda (pair) (beginning-of-buffer)(while (search-forward (car pair) nil 't)(replace-match (cdr pair))))
   '(
     ("Funds Transfer-Online" . "Transfer")
     ("Preauthorized Payment" . "Payment")
     ("Point Of Sale " . "")
     ("Preauthorized Credit " . "")
     ("Preauthorized Payment " . "")
     ("Atm Withdrawal-Interac ." . "ATM")
     (" Jj " . " JJ ")
     (" - THANK" . "")
     ("Www." . "www.")
     ("Sq *" . "")
     ("'S" . "'s")
     ("Charges Applied to Account Periodic Flat Feecapitalise" . "Fee")
     )))

(eval-after-load "ledger"
  (bind-key (kbd "C-c C-n") 'edd/ledger-next-unknown ledger-mode-map))


(provide 'edd-ledger)
