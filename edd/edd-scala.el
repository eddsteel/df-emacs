(use-package scala-mode
  :ensure t
  :mode ("\\.scala\\'" . scala-mode)
  :config
  (setenv "COURSIER_NO_TERM" "true")
  (setq scala-indent:align-parameters t)
  (setq scala-indent:align-forms t)
  (setq scala-indent:use-javadoc-style nil)
  (setq flycheck-scalastyle-jar "~/.local/share/scalastyle.jar")
  (setq flycheck-scalastylerc "~/.config/scalastyle.xml"))

(use-package ensime
  :pin melpa-stable
  :ensure t
  :init
  (setq ensime-auto-generate-config 't)
  (defun edd-ensime-scala-mode-hook ()
    (when buffer-file-name ;; i.e. not org babel
      (let ((file (ensime-config-find-file (buffer-file-name))))
        (when (and file
                   (not (ensime-connection-or-nil)))
          (call-interactively 'ensime))
        (ensime-mode))))

  (add-hook 'scala-mode-hook #'edd-ensime-scala-mode-hook)
  :config
  (setq ensime-goto-test-config-defaults
        (plist-put ensime-goto-test-config-defaults
                   :test-template-fn 'edd-ensime-test-template))
  (local-set-key (kbd "C-c C-e") 'ensime-inf-eval-region)
  :commands
  (ensime-scala-mode-hook ensime-config-find-file ensime-connection-or-nil)
  :bind
  ("C-c e" . ensime))

(use-package sbt-mode
  :ensure t
  :commands (sbt-start run-scala)
  :bind (("C-c s" . sbt-start)
         ("C-c h s" . edd-run-scala))
  :init
  (defun edd-run-scala ()
    (interactive)
    (if (ensime-connection-or-nil)
        (call-interactively 'ensime-inf-run-scala)
      (if (sbt:find-root)
          (run-scala)
        (comint-run "scala"))))
  :config
  (local-set-key (kbd "C-c C-b a") 'edd-sbt-assembly)
  (local-set-key (kbd "C-c C-v C-l") 'edd-sbt-test-only-last)
  (local-set-key (kbd "C-c C-v C-t") 'edd-sbt-test-only)
  (local-set-key (kbd "C-c C-b C-l") 'sbt-run-previous-command))


(define-derived-mode sbt-file-mode scala-mode "SBT file mode"
  "A mode for editing .sbt files")
(add-to-list 'auto-mode-alist '("\\.sbt\\'" . sbt-file-mode))


(defun edd-ensime-test-template ()
  ""
  "package %TESTPACKAGE%

import org.scalatest._

class %TESTCLASS% extends FlatSpec with Matchers {

}")


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

(add-hook 'scala-mode-hook (lambda () (setq-local nyan-bar-length 16)))


(defun edd-align-sbt-deps ()
  (interactive)
  (align-regexp (region-beginning) (region-end) "\\(\\s-+\\)\\(%%?\\|\"\\)" 1 1 't))


(defun edd-java-hook ()
  (setq compile-command "ant \-emacs compile \-find")
  (local-set-key (kbd "C-x C-k") 'recompile))

(defun edd-scala-helm-method ()
  (interactive)
  (call-interactively 'helm-occur "\bdef\ "))
(add-hook 'scala-mode-hook (lambda () (local-set-key (kbd "C-c .") 'edd-scala-helm-method)))


(setq edd-scala-sort-imports-rules
      '(("^import com\\.hootsuite\\." . "1") ("^import scala\\." . "6") ("^import java\\." . "7") ("^import " . "5")))

;; TODO: support blank lines as both a rule and while traversing
(defun edd-scala-sort-imports ()
  "Sorts imports according to rules, which are cons pairs of regexp to order"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (search-forward-regexp "^package\\b")
    (search-forward-regexp "^import\\b")
    (beginning-of-line)
    (let ((start (point)))
      (while (looking-at-p "^import\\b")
        (forward-line))
      (end-of-line)
      (let ((end (point)))
        (goto-char start)
        (while (looking-at-p "^import\\b")
          (mapcar
           (lambda (pair)
             (let ((rule (car pair))
                   (ord (cdr pair)))
               (when
                   (search-forward-regexp rule (line-end-position) 't)
                 (replace-match (concat ord "\\&")))))
           edd-scala-sort-imports-rules)

          (forward-line))
        (sort-lines nil start end)
        (goto-char start)
        (while (search-forward-regexp "^\\([0-9]\\)import\\b")
          (replace-match "import"))
        ))
      ))



(add-hook 'java-mode-hook 'edd-java-hook)

(provide 'edd-scala)
