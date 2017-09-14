;; org config that doesn't load packages
(require 'org)
(require 'org-element)
(require 'ob-http)
(setq org-html-validation-link nil)
(setq org-html-head-include-default-style nil)
(add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))
(setq org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0
      org-src-fontify-natively t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t
      org-hide-emphasis-markers t)
(setq org-deck-title-slide-template
  "<h1>%t</h1>
<h2>%a</h2>")
(setq org-export-allow-bind-keywords t)
(defun edd-org-babel-code-properties
    "inserts some default properties for org-babel. Note you still need :exports per block for github support"
    (interactive)
    (insert "#+PROPERTY:header-args :results output :session :cache yes :tangle yes :comments org :exports both"))

(org-babel-do-load-languages
 'org-babel-load-languages
 '((clojure . t)
   (css . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (haskell . t)
   (js . t)
   (ledger . t)
   (plantuml . t)
   (python . t)
   (ruby . t)
   (scala . t)
   (sh . t)
   (sqlite . t)
   (http . t)))

(setq org-src-fontify-natively t)
(setq org-log-done t)
(setq org-hide-leading-stars t)
(setq org-use-speed-commands t)
(setq org-ellipsis "…")
(setq org-todo-keywords
      '((sequence "TODO(t)" "WAIT(w@/!)" "|" "DONE(d!)" "DELEGATED(l@)")))
(setq org-special-ctrl-a/e t)

(define-key org-mode-map (kbd "M-p") 'org-shiftmetaup)
(define-key org-mode-map (kbd "M-n") 'org-shiftmetadown)
(define-key org-mode-map (kbd "C-M-o") 'org-insert-heading)

;          (darkokai-yellow           "#E6DB74")
;          (darkokai-orange           "#ffac4a")
;          (darkokai-red              "#ff0066")
;          (darkokai-magenta          "#ff8eff")
;          (darkokai-violet           "#ab7eff")
;          (darkokai-blue             "#06d8ff")
;          (darkokai-cyan             "#53f2dc")
;          (darkokai-green            "#63de5d")
;          (darkokai-gray             "#35393b")

(setq org-todo-keyword-faces
      '(("DEV" . (:underline t :foreground "#53f2dc" :bold))
        ("STG" . (:underline t :foreground "#ffac4a" :bold))
        ("PRD" . (:underline t :bold))
        ("PR" . "#53f2dc")))

; Use the CSS file in ~/.emacs.d
(add-hook 'org-mode-hook
          (lambda ()
            (setq org-html-head
                  (concat
                   "<link rel=\"stylesheet\" href=\""
                   (file-relative-name user-emacs-directory)
                   "edd/org-export.css\">"))))

(provide 'edd-org-options)
