(use-package hydra
  :ensure t
  :init

  ;; WANT:

  ;; run a build shell e.g. sbt
  ;; run a repl

  ;; possible to do generic dev:
  ;; - build shell
  ;; - repl
  ;; - compile
  ;; - test
  ;; - build executable
  ;; - search docs
  ;; ???



  ;; projectile
  ;; https://github.com/abo-abo/hydra/wiki/Projectile-&-Fixmee
  (defhydra hydra-project (:color blue :idle 0.4 :columns 3) "Projectile"
    ("<ESC>" nil "quit")
    ("a"   helm-projectile-ag "ag")
    ("A"   helm-projectile-grep "grep")
    ("b"   projectile-switch-to-buffer "buffer")
    ("B"   projectile-switch-to-buffer-other-window "buffer other window")
    ("c"   projectile-run-async-shell-command-in-root "shell command")
    ("C"   projectile-run-command-in-root "command")
    ("d"   projectile-find-dir "find dir")
    ("D"   projectile-find-dir-other-window "find dir other window")
    ("f"   projectile-find-file "find file")
    ("F"   projectile-find-file-other-window "find file other window")
    ("g"   projectile-vc "VC")
    ("h"   projectile-dired "dired")
    ("i"   projectile-project-info "info")
    ("kc"  projectile-invalidate-cache "invalidate cache")
    ("kd"  projectile-remove-known-project "remove a project")
    ("kk"  projectile-cache-current-file "cache current file")
    ("K"   projectile-kill-buffers "kill buffers")
    ("ks"  projectile-cleanup-known-projects "cleanup projects")
    ("l"   projectile-find-file-dwim "find-file DWIM")
    ("L"   projectile-find-file-dwim-other-window "find-file DWIM other window")
    ("m"   projectile-compile-project "compile")
    ("o"   projectile-find-other-file "find other file")
    ("O"   projectile-find-other-file-other-window "find other file other window")
    ("p"   projectile-switch-project "switch project >>" :exit t )
    ("r"   projectile-recentf "recent files")
    ("s"   projectile-multi-occur "occur")
    ("S"   projectile-replace "replace")
    ("t"   projectile-find-tag "find tag")
    ("T"   projectile-regenerate-tags "generate tags")
    ("u"   projectile-find-test-file "find test file")
    ("U"   projectile-test-project "test project")
    ("v"   projectile-display-buffer "display buffer")
    ("V"   projectile-ibuffer "ibuffer")
    ("X"   projectile-run-shell "shell")
    ("x"   (lambda ()(interactive)(projectile-run-term "/bin/bash")) "term"))

  ;; Thanks http://kitchingroup.cheme.cmu.edu/blog/2015/09/28/A-cursor-goto-hydra-for-emacs/
  (defhydra hydra-goto (:color blue :columns 4) "goto"

    ("c" avy-goto-char-2 "2 char")
    ("C" avy-goto-char "char")
    ("L" avy-goto-char-in-line "char in line")
    ("w" avy-goto-word-1 "word")
    ("S" avy-goto-subword-1 "subword")
    ("l" avy-goto-line "line")
    ("I" ace-window "window")

    ("h" helm-org-headlines "org headline")
    ("a" helm-org-agenda-files-headings "agenda headline")
    ("q" helm-multi-swoop-org "org swoop")

    ("o" helm-occur "helm occur")
    ("i" swiper-helm "swiper")

    ("s" isearch-forward "search >")
    ("r" isearch-backward "search <")

    ("nn" flycheck-next-error "next error")
    ("np" flycheck-previous-error "prev error")
    ("nl" list-flycheck-errors "error list")
    ("nc" flycheck-compile "refresh errors")

    ("b" helm-mini "buffer")
    ("R" helm-recentf "recentf")
    ("P" hydra-project/body "project >>"))

  (global-set-key (kbd "C-c o") 'hydra-goto/body)
  (setq projectile-switch-project-action
        (lambda ()
          (progn
            (projectile-dired)
            (hydra-project/body))))

  (define-key projectile-mode-map (kbd "C-c p") nil)
  (global-set-key (kbd "C-c p") 'hydra-project/body))

(provide 'edd-hydra)
