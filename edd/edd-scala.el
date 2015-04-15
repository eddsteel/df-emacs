(use-package scala-mode2
  :ensure t
  :mode ("\\.scala\\'" . scala-mode)
  :config
  (setq scala-indent:align-parameters t)
  (setq scala-indent:align-forms t)
  (setq scala-indent:use-javadoc-style t))

(use-package ensime
  :ensure t
  :init
  (add-hook 'scala-mode-hook #'ensime-scala-mode-hook)
  :config
  (setq ensime-goto-test-config-defaults
        (plist-put ensime-goto-test-config-defaults
                   :test-template-fn 'edd-ensime-test-template))
  (local-set-key (kbd "C-c C-e") 'ensime-inf-eval-region)
  (defun ensime-modeline-string ()
    "Return the string to display in the modeline.
  \"ENSIME\" only appears if we aren't connected.  If connected, include
  connection-name, and possibly some state
  information."
    (when ensime-mode
      (condition-case err
          (let ((conn (ensime-connection-or-nil)))
            (cond ((and ensime-mode (not conn))
                   (cond
                    ((ensime-owning-server-process-for-source-file buffer-file-name)
                     " [E:Starting]")
                    (t " Ø")))

                  ((and ensime-mode (ensime-connected-p conn))
                   (concat " ["
                           (or (plist-get (ensime-config conn) :name)
                               "E:Connected")
                           (let ((status (ensime-modeline-state-string conn))
                                 (unready (not (ensime-analyzer-ready conn))))
                             "")
                           (concat (format " : %s/%s"
                                           (ensime-num-errors conn)
                                           (ensime-num-warnings conn)))
                           "]"))
                  (ensime-mode " [E: Dead Connection]")
                  ))
        (error (progn
                 " [E: wtf]"
                 )))))
  :commands
  (ensime-scala-mode-hook)
  :bind
  ("C-c e" . ensime))

(use-package sbt-mode
  :ensure t
  :commands (sbt-start run-scala)
  :bind ("C-c s" . sbt-start)
  :bind ("C-c h s" . run-scala)
  :config
  (local-set-key (kbd "C-c C-v C-l") 'edd-sbt-test-only-last)
  (local-set-key (kbd "C-c C-v C-t") 'edd-sbt-test-only)
  (local-set-key (kbd "C-c C-b C-l") 'sbt-run-previous-command))


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

(defun edd-sbt-test-only ()
  (interactive)
  (let ((n (edd-scala-fqn-containing-point)))
    (when n
      (setq edd-scala-last-test-only n)
      (sbt-command (concat "test-only " n)))))

(defun edd-sbt-test-only-last ()
  (interactive)
  (when edd-scala-last-test-only
    (sbt-command (concat "test-only " edd-scala-last-test-only))))



(add-hook 'scala-mode-hook (lambda () (setq-local nyan-bar-length 16)))


(defun edd-java-hook ()
  (setq compile-command "ant \-emacs compile \-find")
  (local-set-key (kbd "C-x C-k") 'recompile))

(add-hook 'java-mode-hook 'edd-java-hook)

(provide 'edd-scala)
