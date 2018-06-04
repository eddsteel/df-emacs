(use-package scala-mode
  :ensure sbt-mode
  :mode ("\\.scala\\'" . scala-mode)

  :config
  (setenv "COURSIER_NO_TERM" "true")
  (setq scala-indent:align-parameters nil)
  (setq scala-indent:align-forms nil)
  (setq scala-indent:use-javadoc-style nil)
  (setq flycheck-scalastyle-jar "~/.local/share/scalastyle.jar")
  (setq flycheck-scalastylerc "~/.config/scalastyle.xml")
  (add-hook 'scala-mode-hook (lambda () (setq-local nyan-bar-length 16)))
  (add-hook 'scala-mode-hook (lambda () (eval-after-load "counsel" (setq-local counsel-grep-swiper-limit 1200))))
  (add-hook 'scala-mode-hook (lambda () (progn
                                    (local-set-key (kbd "C-c C-b a") 'edd-sbt-assembly)
                                    (local-set-key (kbd "C-c C-v C-l") 'edd-sbt-test-only-last)
                                    (local-set-key (kbd "C-c C-v C-t") 'edd-sbt-test-only)
                                    (local-set-key (kbd "C-c C-b C-l") 'sbt-run-previous-command))))
  (add-hook 'scala-mode-hook (lambda () (setq-local prettify-symbols-alist
                                               '(("<=" . 8804)
                                                 (">=" . 8805)
                                                 ("==" . 8801)
                                                 ("===" . 8803)
                                                 ("!" . 172)
                                                 ("!=" . 8802)
                                                 ("=!=" . 8813)
                                                 ("&&" . 8743)
                                                 ("||" . 8744)
                                                 ("true" . 8868)
                                                 ("false" . 8869)
                                                 ("Boolean" . 120121)
                                                 ("empty" . 8709)
                                                 ("sum" . 8721)
                                                 ("product" . 8719)
;;                                               ("contains" . 8717)
;;                                               ("forall" . 8704)
;;                                               ("any" . 8707)
                                                 ("intersect" . 8745)
                                                 ("union" . 8746)
                                                 ("diff" . 8783)
                                                 ("subsetOf" . 8838)
                                                 ("++" . 10746)
                                                 ("::" . 11820)
                                                 ("--" . 9548)
                                                 ("->" . 8594)
                                                 ("<-" . 8592)
                                                 ("=>" . 8658)
                                                 ("<=>" . 8660)
                                                 ("-->" . 10230)
                                                 ("<->" . 8596)
                                                 ("<--" . 10229)
                                                 ("<-->" . 10231)
                                                 ("==>" . 10233)
                                                 ("<==" . 10232)
                                                 ("<==>" . 10234)
                                                 ("~>" . 8669)
                                                 ("<~" . 8668)
                                                 ("Unit" . 8709)
                                                 ("Int" . 8484)
                                                 ("assert" . 8870)
                                                 (":=" . 8788)
                                                 ("flatMap" . 10524)
                                                 (">>=" . 10524)
                                                 ("bind" . 10524)
                                                 (">>" . 8811)
                                                 ("followedBy" . 8811)
                                                 ("<+>" . 8853)
                                                 ("_root_." . 46)))
                               (prettify-symbols-mode)))
  (defun edd-run-scala ()
    (interactive)
    (if (ensime-connection-or-nil)
        (call-interactively 'ensime-inf-run-scala)
      (if (sbt:find-root)
          (run-scala)
        (comint-run "amm"))))
  (defun edd-scala-package-containing-point ()
    (save-excursion
      (let ((segs '()))
        (while (search-backward-regexp
                "^package \\(\\(?:[a-z0-9_]+\\.\\)*[a-z0-9)]+\\)"
                (point-min) t)
          (let ((segment (match-string 1)))
            (add-to-list 'segs (ensime-kill-txt-props segment))))
        (mapconcat 'identity segs "."))))

  (defun edd-scala-class-or-module-containing-point ()
    (save-excursion
      (when (search-backward-regexp
             "^\\(?:class\\|object\\) \\(\\(?:[A-Z]+\\)[a-zA-Z0-9_]*\\)"
             (point-min) t)
        (let  ((match (match-string 1)))
          (ensime-kill-txt-props match)))))

  (defun edd-scala-fqn-containing-point ()
    (let ((n (edd-scala-class-or-module-containing-point))
          (p (edd-scala-package-containing-point)))
      (concat p "." n)))

  (defun edd-sbt-assembly ()
    (interactive)
    (sbt-command "assembly"))

  (defun edd-sbt-test-only ()
    (interactive)
    (letrec ((n (edd-scala-fqn-containing-point))
             (cmd (concat "test-only " n " -- -oDF")))
      (when n
        (setq edd-scala-last-test-only n)
        (message (concat "sbt " cmd)
                 (sbt-command cmd)))))

  (defun edd-sbt-test-only-last ()
    (interactive)
    (when edd-scala-last-test-only
      (sbt-command (concat "test-only " edd-scala-last-test-only))))

  (defun edd-scala-ivy-method ()
    (interactive)
    (funcall 'swiper "\\bdef "))

  (define-key scala-mode-map (kbd "C-c .") 'edd-scala-ivy-method)

  (defvar edd-scala/root-package "com.eddsteel" "Common root for scala packages")

  (defvar edd-scala/sort-imports-rules
    (list
       (cons (concat "^import " (regexp-quote (concat edd-scala/root-package "."))) "1")
        (cons "^import scala\\." "6")
        (cons "^import java\\." "7")
        (cons "^import javax\\." "8")
        (cons "^import " "5"))
    "Rules for sorting imports of the form (REGEXP . PRECEDENCE)")

;; TODO: support blank lines as both a rule and while traversing
  (defun edd-scala-sort-imports ()
    "Sorts imports according to rules, which are cons pairs of regexp to order"
    (interactive)
    (save-excursion
      (beginning-of-buffer)
      (let ((importre "^import\\b")
            (blankre  "^$"))
        (search-forward-regexp "^package\\b")
        (search-forward-regexp importre)
        (beginning-of-line)
        (let ((start (point)))
          (while (looking-at-p importre)
            (forward-line))
          (end-of-line)
          (let ((end (point)))
            (mapcar
             (lambda (pair)
               (progn
                 (goto-char start)
                 (while (< (point) end)
                   (let ((rule (car pair))
                         (ord (cdr pair)))
                     (when
                         (search-forward-regexp rule end 't)
                       (replace-match (concat ord "\\&")))
                     (forward-line)))))
             edd-scala/sort-imports-rules)
            (sort-lines nil start end)
            (goto-char start)
            (while (search-forward-regexp "^\\([0-9]\\)import\\b" nil 't)
              (replace-match "import")))))))

  (defun edd-scala-ignore-style (start end)
    "Ignore a scalastyle rule. If region is active it will be
   wrapped in a scalastyle:off/scalastyle:on comment pair. If not, a
   scalastyle:ignore comment will be used."
    (interactive "r")
    (let ((rule (completing-read "style-rule "
                                 '("file.size.limit" "line.size.limit" "line.contains.tab" "header.matches" "newline.at.eof" "no.newline.at.eof" "regex" "whitespace.end.of.line" "class.name" "covariant.equals" "cyclomatic.complexity" "equals.hash.code" "if.brace" "illegal.imports" "magic.number" "method.length" "method.name" "no.clone" "no.finalize" "no.whitespace.after.left.bracket" "no.whitespace.before.left.bracket" "null" "number.of.methods" "number.of.types" "object.name" "package.object.name" "parameter.number" "public.methods.have.type" "return" "simplify.boolean.expression" "spaces.after.plus" "spaces.before.plus" "structural.type" "uppercase.l" "var.field" "var.local" "while"))))
      (save-excursion
        (if (region-active-p)
            (progn
              (goto-char end)
              (end-of-line)
              (newline)
              (insert "// scalastyle:on " rule)
              (indent-according-to-mode)
              (goto-char start)
              (beginning-of-line)
              (open-line 1)
              (insert "// scalastyle:off " rule)
              (indent-according-to-mode))
          (progn
            (end-of-line)
            (insert " // scalastyle:ignore " rule)))))))

(use-package ensime
  ;; :pin melpa-stable
  :pin melpa-stable
  :init
  (setq ensime-auto-generate-config 't)
  (setq ensime-startup-notification nil)
  (setq ensime-startup-snapshot-notification nil)
  (defun edd-ensime-scala-mode-hook ()
    (when buffer-file-name ;; i.e. not org babel
      (let ((file (ensime-config-find-file (buffer-file-name))))
        (when (and file
                   (not (ensime-connection-or-nil)))
          (call-interactively 'ensime))
        (ensime-mode))))

  (add-hook 'scala-mode-hook #'edd-ensime-scala-mode-hook)
  :config
;  (setq ensime-goto-test-config-defaults
;        (plist-put ensime-goto-test-config-defaults
;                   :test-template-fn 'edd-ensime-test-template))
  (setq ensime-use-helm nil)
  (setq ensime-graphical-tooltips 't)
  (local-set-key (kbd "C-c C-e") 'ensime-inf-eval-region)
  ;; dumb jump if ensime jump fails
  (defadvice ensime-edit-definition (after dumb-jump-after-ensime)
    (when (not ad-return-value)
      (call-interactively #'dumb-jump-go)))
  (ad-activate 'ensime-edit-definition)

  :commands
  (ensime-scala-mode-hook ensime-config-find-file ensime-connection-or-nil)
  :bind
  (:map scala-mode-map
        ("C-c e" . ensime)))

;; Extra mode for .sbt files to stop them being covered in errors.
;;
(define-derived-mode sbt-file-mode scala-mode "SBT file mode"
  "A mode for editing .sbt files")
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . sbt-file-mode))


(defun edd-ensime-test-template ()
  ""
  "package %TESTPACKAGE%

import org.scalatest._

class %TESTCLASS% extends FlatSpec with Matchers {

}")

(defun edd-align-sbt-deps ()
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\(\\s-+\\)\\(%%?\\|\"\\)" 1 1 't))

(defun edd-java-hook ()
  (setq compile-command "ant \-emacs compile \-find")
  (local-set-key (kbd "C-x C-k") 'recompile))

(defun edd-scala-helm-method ()
  (interactive)
  (call-interactively 'helm-occur "\bdef\ "))

(add-hook 'java-mode-hook 'edd-java-hook)

(require 's)

(defun edd-scala/pkgize-file (file)
  (s-replace "-" "" (s-replace "/" "." file)))

(defun edd-scala/fileize-pkg (pkg)
  (s-replace "." "/" pkg))

(defun edd-scala/fix-sbt-root ()
  (interactive)
  (setq sbt:buffer-project-root (sbt:find-root))
  )



;; Gets "package com.eddsteel.project.package.name" from
;; - project src/main/scala/com/eddsteel/project/package/name/File.scala
;; - project src/main/scala/project/package/name/File.scala
;; - project src/main/scala/package/name/File.scala
;; - project src/com/eddsteel/project/package/name/File.scala
;; - project src/project/package/name/File.scala
;; - project src/package/name/File.scala
;;
;; Gets "package com.eddsteel.project" from
;; - project src/main/scala/File.scala
;; - project src/File.scala
;;
(defun edd-scala/guess-package (project relfile)
    (let*
      ((root edd-scala/root-package)
       (projectdir (file-name-as-directory project))
       (rootpkgdir (file-name-as-directory (edd-scala/fileize-pkg root)))
       (srcfile (s-chop-prefixes (list "src/" "main/scala/" rootpkgdir projectdir) relfile))
       (srcdir (if-let ((dir (file-name-directory srcfile)))
                   (directory-file-name dir))))
      (concat "package " root "." project (if srcdir (concat "." (edd-scala/pkgize-file srcdir)) ""))))

(defun edd-scala/guess-package-buffer ()
  "Guess and insert the package for this scala file."
  (interactive)
  (if (projectile-project-p)
      (insert (edd-scala/guess-package
               (file-name-nondirectory (directory-file-name (projectile-project-root)))
               (car (projectile-make-relative-to-root (list (buffer-file-name))))))
    (message "Must be in a projectile project.")))


(provide 'edd-scala)
