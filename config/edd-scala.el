(setq scala-indent:align-parameters t)
(setq scala-indent:align-forms t)
(setq scala-indent:use-javadoc-style t)

(defun edd-scala-prefs ()
  (interactive)
  (require 'whitespace)

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
        (ensime-sbt-action (concat "test-only " n))))))

  
; ensime
(require 'ensime)
(defun edd-ensime-bindings ()
  "Extra bindings for ensime"
  (local-set-key (kbd "C-c C-e") 'ensime-inf-eval-region))

(defun edd-scala-hook ()
  (ensime-scala-mode-hook)
  (edd-ensime-bindings)
  (edd-scala-prefs))

(add-hook 'scala-mode-hook 'edd-scala-hook)

(defun edd-java-hook ()
  (setq compile-command "ant \-emacs compile \-find")
  (local-set-key (kbd "C-x C-k") 'recompile))

(add-hook 'java-mode-hook 'edd-java-hook)

(provide 'edd-scala)
