;; org config that doesn't load packages
(setq org-html-validation-link nil)
(setq org-html-head-include-default-style nil)

(eval-after-load "org"
  (lambda () (add-to-list 'org-src-lang-modes '("dot" . graphviz-dot))))
(setq org-confirm-babel-evaluate nil
      org-edit-src-content-indentation 0
      org-src-fontify-natively t
      org-src-preserve-indentation t
      org-src-tab-acts-natively t
      org-hide-emphasis-markers t
      org-startup-folded nil)
(setq org-image-actual-width nil)
(setq org-deck-title-slide-template
  "<h1>%t</h1>
<h2>%a</h2>")
(setq org-export-allow-bind-keywords t)
(defun edd-org-babel-code-properties ()
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
   (shell . t)
   (sqlite . t)))

(eval-after-load "scala" (lambda () (org-babel-do-load-languages 'org-babel-load-languages '(scala . t))))

(setq org-imenu-depth 3)
(setq org-src-fontify-natively t)
(setq org-log-done t)
(setq org-hide-leading-stars t)
(setq org-use-speed-commands t)
(setq org-ellipsis "…")
(setq org-special-ctrl-a/e t)

(setq org-todo-keyword-faces
      '(("DEV" . (:underline t :foreground "#53f2dc" :bold))
        ("STG" . (:underline t :foreground "#ffac4a" :bold))
        ("DEPLOY" . "org-todo")
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

;; those good bullets
(font-lock-add-keywords 'org-mode
                        '(("^ +\\([-*]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(defun edd-org-split-gh-pr (tag)
  (let*
      ((pieces (split-string tag "#"))
       (project (car pieces))
       (pr (cadr pieces)))
    (string-join (list project "/pull/" pr))))

(add-to-list 'org-link-abbrev-alist
             '("gh" .  "https://github.com/"))
(add-to-list 'org-link-abbrev-alist
             '("ghp" . "https://github.com/%(edd-org-split-gh-pr)"))

(provide 'edd-org-options)
