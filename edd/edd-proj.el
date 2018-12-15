;; projectile
;; http://endlessparentheses.com/improving-projectile-with-extra-commands.html
;; https://emacs.stackexchange.com/questions/40553/projectile-run-project-without-prompt
;;
(use-package projectile
  :demand t
  :init
  ;; counsel-projectile will die without this
  (setq projectile-keymap-prefix (kbd "C-x p"))
  :hook
  ((text-mode prog-mode) . projectile-mode)
  :config
  (setq projectile-mode-line
        '(:eval (format " 🔖%s" (projectile-project-name))))

  (plist-put (gethash 'gradlew projectile-project-types) 'run-command "./gradlew run")

  (defun edd-proj/run-comint ()
    (interactive)
    (projectile-with-default-dir (projectile-project-root)
      (call-interactively 'comint-run)))

  (defun edd-proj/magit-and-fetch ()
    (interactive)
    (progn (magit-status)(call-interactively #'magit-fetch-from-upstream)))
    
  (defun edd-proj/term ()
    (interactive)
    (projectile-run-term "/bin/bash"))

  (defun edd-proj/make-or-compile ()
    (interactive)
    (let ((res (ignore-errors (helm-make-projectile))))
      (if res res
        (projectile-compile-project t))))

  (defun edd-proj/compile-no-prompt ()
    (interactive)
    (let ((compilation-read-command nil))
      (projectile-compile-project nil)))

  (defun edd-proj/test-no-prompt ()
    (interactive)
    (let ((compilation-read-command nil))
      (projectile-test-project nil)))

  (defun edd-proj/run-no-prompt ()
    (interactive)
    (let ((compilation-read-command nil))
      (projectile-run-project nil)))

  :bind
  ("C-c r" . edd-proj/run-comint))

(provide 'edd-proj)
