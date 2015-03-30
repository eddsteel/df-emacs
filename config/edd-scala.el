(setq scala-indent:align-parameters t)
(setq scala-indent:align-forms t)
(setq scala-indent:use-javadoc-style t)

(autoload 'ensime "ensime" "Require ensime when running it" t)
(autoload 'ensime-scala-mode-hook "ensime" "Require ensime when loading a scala file" t)

;; Preload useful docs
(setq helm-dash-common-docsets '("Akka" "Scala"))

(defun edd-scala-prefs ()
  (interactive)

  ;; clean-up whitespace at save
  (make-local-variable 'before-save-hook)
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ; show whitespace
  (whitespace-mode)

  (defun open-test () (interactive)
    (letrec ((bf buffer-file-name)
             (tf (replace-regexp-in-string 
                  ".scala$" "Test.scala"
                  (replace-regexp-in-string "/main/" "/test/" bf))))
      (find-file-other-window tf)))

  (defun open-spec () (interactive)
    (letrec ((bf buffer-file-name)
             (tf (replace-regexp-in-string
                  ".scala$" "Spec.scala"
                  (replace-regexp-in-string "/main/" "/test/" bf))))
      (find-file-other-window tf)))

  ; like ensime equiv but keeps searching for surrounding packages.
  (defun edd-package-containing-point ()
    (save-excursion
      (let ((segs '()))
        (while (search-backward-regexp
                "^package \\(\\(?:[a-z0-9_]+\\.\\)*[a-z0-9)]+\\)"
                (point-min) t)
          (let ((segment (match-string 1)))
            (add-to-list 'segs (ensime-kill-txt-props segment))))
        (mapconcat 'identity segs "."))))

  ; similarly, finds surrounding top-level class or object (but not trait)
  (defun edd-class-or-module-containing-point ()
    (save-excursion
      (when (search-backward-regexp
             "^\\(?:class\\|object\\) \\(\\(?:[A-Z]+\\)[a-zA-Z0-9_]*\\)"
             (point-min) t)
        (let  ((match (match-string 1)))
          (ensime-kill-txt-props match)))))

  (defun edd-fqn-containing-point ()
    (let ((n (edd-class-or-module-containing-point))
          (p (edd-package-containing-point)))
      (concat p "." n)))

  (defun edd-sbt-test-only ()
    (interactive)
    (let ((n (edd-fqn-containing-point)))
      (when n
        (setq edd-last-test-only n)
        (sbt-command (concat "test-only " n)))))

  (defun edd-sbt-test-only-last ()
    (interactive)
    (when edd-last-test-only
      (sbt-command (concat "test-only " edd-last-test-only))))

  ;; These aren't really scala-specific
  (local-set-key (kbd "C-c d d") 'helm-dash)
  (local-set-key (kbd "C-c d a") 'helm-dash-activate-docset)

  
  (local-set-key (kbd "C-c C-v C-l") 'edd-sbt-test-only-last)
  (local-set-key (kbd "C-c C-v C-t") 'edd-sbt-test-only)
  (local-set-key (kbd "C-c C-b C-l") 'sbt-run-previous-command))

;; Thanks tjweir
(defun set-imenu-expression ()
  (setq imenu-generic-expression
        '(
          ("var" "\\(var +\\)\\([^(): ]+\\)" 2)
          ("val" "\\(val +\\)\\([^(): ]+\\)" 2)
          ("override def" "^[ \\t]*\\(override\\) +\\(def +\\)\\([^(): ]+\\)" 3)
          ("private def" "^[ \\t]*\\(private\\(\\[.*?\\]+\\)*\\) +\\(def +\\)\\([^(): ]+\\)" 4)
          ("protected def" "^[ \\t]*\\(protected\\(\\[.*?\\]+\\)*\\) +\\(def +\\)\\([^(): ]+\\)" 4)
          ("implicit def" "^[ \\t]*\\(implicit\\) +\\(def +\\)\\([^(): ]+\\)" 3)
          ("def" "^[ \\t]*\\(def +\\)\\([^(): ]+\\)" 2)
          ("trait" "\\(trait +\\)\\([^(): ]+\\)" 2)
          ("class" "^[ \\t]*\\(class +\\)\\([^(): ]+\\)" 2)
          ("case class" "^[ \\t]*\\(case class +\\)\\([^(): ]+\\)" 2)
          ("abstract class" "^[ \\t]*\\(abstract class +\\)\\([^(): ]+\\)" 2)
          ("object" "\\(object +\\)\\([^(): ]+\\)" 2))))
 
 
;; Puts the current imenu thing in top line of the buffer.
(which-function-mode)
(setq-default header-line-format
              '((which-func-mode ("" which-func-format " "))))
(setq mode-line-misc-info
;; We remove Which Function Mode from the mode line, because it's mostly
;; invisible here anyway.
(assq-delete-all 'which-func-mode mode-line-misc-info))
 
(setq which-func-unknown "★")

(defun edd-scala-flycheck ()
  (unless (eq (file-name-extension (buffer-file-name (current-buffer))) "sbt")
    (setq flycheck-scalastyle-jar (emacsd "scalastyle/scalastyle_2.10-batch.jar"))
    (setq flycheck-scalastylerc (emacsd "scalastyle/scalastyle-config.xml"))
    (flycheck-mode 1)))
  
; ensime
(defun edd-ensime-bindings ()
  "Extra bindings for ensime/SBT"
  (local-set-key (kbd "C-c e") 'ensime)
  (local-set-key (kbd "C-c s") 'sbt-start)
  (local-set-key (kbd "C-c C-e") 'ensime-inf-eval-region))

(defun edd-scala-hook ()
  (rainbow-delimiters-mode)
  (abbrev-mode)
  (set-imenu-expression)
  (edd-scala-flycheck)
  (auto-highlight-symbol-mode 1)
  (edd-scala-prefs)
  (ensime-scala-mode-hook)
  (setq-local nyan-bar-length 16)
  (edd-ensime-bindings))

(add-hook 'scala-mode-hook 'git-gutter-mode)

(add-hook 'scala-mode-hook
          (lambda ()
            (push '("<-" . ?←) prettify-symbols-alist)
            (push '("->" . ?→) prettify-symbols-alist)
            (push '("=>" . ?⇒) prettify-symbols-alist)
            (push '(">=" . ?≥) prettify-symbols-alist)
            (push '("<=" . ?≤) prettify-symbols-alist)
            (push '("&&" . ?∧) prettify-symbols-alist)
            (push '("||" . ?∨) prettify-symbols-alist)))

(add-hook 'scala-mode-hook (lambda () (prettify-symbols-mode 1)))
(add-hook 'scala-mode-hook 'edd-scala-hook)


(defun edd-java-hook ()
  (setq compile-command "ant \-emacs compile \-find")
  (local-set-key (kbd "C-x C-k") 'recompile))

(add-hook 'java-mode-hook 'edd-java-hook)

(provide 'edd-scala)
