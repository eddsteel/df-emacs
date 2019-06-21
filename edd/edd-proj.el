;; projectile
;; http://endlessparentheses.com/improving-projectile-with-extra-commands.html
;; https://emacs.stackexchange.com/questions/40553/projectile-run-project-without-prompt
;;
(use-package projectile
  :ensure projectile-ripgrep
  :demand t
  :hook
  ((text-mode prog-mode) . projectile-mode)
  :init
  ;; counsel-projectile will die without this
  (setq projectile-keymap-prefix (kbd "C-x p"))

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

  (defun edd-proj/modeline ()
    (format " 🏉%s" (projectile-project-name)))

  (setq projectile--mode-line " 🏉")
  (setq projectile-mode-line-function #'edd-proj/modeline)

  :config
  (plist-put (alist-get 'gradlew projectile-project-types) 'run-command "./gradlew run")
  (defhydra+ hydra-project nil "Project"
    ("A"   projectile-grep "grep")
    ("B"   projectile-switch-to-buffer-other-window "buffer other window")
    ("c"   projectile-run-command-in-root "command")
    ("C"   projectile-run-async-shell-command-in-root "shell command")
    ("D"   projectile-find-dir-other-window "find dir other window")
    ("e"   project-find-file "find file (any file under root)")
    ("E"   project-find-regexp "find regexp (any file under root)")
    ("F"   projectile-find-file-other-window "find file other window")
    ("g"   edd-proj/magit-and-fetch "git fetch origin")
    ("h"   projectile-dired "dired")
    ("i"   projectile-project-info "info")
    ("kc"  projectile-invalidate-cache "invalidate cache")
    ("kd"  projectile-remove-known-project "remove a project")
    ("kk"  projectile-cache-current-file "cache current file")
    ("K"   projectile-kill-buffers "kill buffers")
    ("ks"  projectile-cleanup-known-projects "cleanup projects")
    ("l"   projectile-find-file-dwim "find-file DWIM")
    ("L"   projectile-find-file-dwim-other-window "find-file DWIM other window")
    ("m"   edd-proj/make-or-compile "run Make target or compile command")
    ("M"   edd-proj/compile-no-prompt "compile")
    ("o"   projectile-find-other-file "find other file")
    ("O"   projectile-find-other-file-other-window "find other file other window")
    ("p"   projectile-switch-project "switch project >>")
    ("r"   projectile-recentf "recent files")
    ("R"   edd-proj/run-no-prompt)
    ("s"   projectile-multi-occur "occur")
    ("S"   projectile-replace "replace")
    ("t"   projectile-toggle-between-implementation-and-test "flip between test and implementation")
    ("T"   edd-proj/test-no-prompt "test project")
    ("v"   projectile-display-buffer "display buffer")
    ("V"   projectile-ibuffer "ibuffer")
    ("X"   edd-proj/term "term")
    ("x"   projectile-run-shell "shell"))

  (setq projectile-switch-project-action
        (lambda ()
          (progn
            (projectile-dired)
            (hydra-project/body))))

  :bind
  (("C-c r" . edd-proj/run-comint)
   :map projectile-mode-map
   ("C-c p" . hydra-project/body)))

(provide 'edd-proj)
