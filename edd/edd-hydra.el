(use-package hydra
  :init

  ;; WANT:

  ;; run a build shell e.g. sbt
  ;; run a repl

  ;; possible to do generic dev:
  ;; - build shell
  ;; - repl
  ;; - compile - projectile supports it but want to override based on project type
  ;; - test - ditto
  ;; - run
  ;; - search docs
  ;; ???

  ;; emms
  (defhydra hydra-music (:color blue :columns 3) "Music"
    ("a"   emms-browse-by-album "browse (album)")
    ("SPC" emms-pause "play/pause")
    ("i"   emms-show "show info")
    ("I"   emms-show-all "show tags")
    ("n"   emms-next "next track")
    ("p"   emms-previous "previous track")
    ("+"   emms-volume-raise "volume up")
    ("-"   emms-volume-lower "volume down")
    ("P"   emms-playlist-mode-switch-buffer "playlist"))

  ;; projectile
  ;; https://github.com/abo-abo/hydra/wiki/Projectile-&-Fixmee
  ;;
  (defhydra hydra-project (:color blue :idle 0.4 :columns 5) "Projectile"
    ("<ESC>" nil "quit")
    ("a"   projectile-ripgrep "rg")
    ("A"   projectile-grep "grep")
    ("b"   counsel-projectile-switch-to-buffer "buffer")
    ("B"   projectile-switch-to-buffer-other-window "buffer other window")
    ("c"   projectile-run-async-shell-command-in-root "shell command")
    ("C"   projectile-run-command-in-root "command")
    ("d"   projectile-find-dir "find dir")
    ("D"   projectile-find-dir-other-window "find dir other window")
    ("e"   project-find-file "find file (any file under root)")
    ("E"   project-find-regexp "find regexp (any file under root)")
    ("f"   projectile-find-file "find file")
    ("F"   projectile-find-file-other-window "find file other window")
    ("g"
     (lambda ()(interactive)(progn (magit-status)(call-interactively #'magit-fetch-from-upstream)))
     "git fetch")
    ("h"   projectile-dired "dired")
    ("i"   projectile-project-info "info")
    ("kc"  projectile-invalidate-cache "invalidate cache")
    ("kd"  projectile-remove-known-project "remove a project")
    ("kk"  projectile-cache-current-file "cache current file")
    ("K"   projectile-kill-buffers "kill buffers")
    ("ks"  projectile-cleanup-known-projects "cleanup projects")
    ("l"   projectile-find-file-dwim "find-file DWIM")
    ("L"   projectile-find-file-dwim-other-window "find-file DWIM other window")
    ("m"   helm-make-projectile "run Make target")
    ("M"   projectile-compile-project "compile")
    ("o"   projectile-find-other-file "find other file")
    ("O"   projectile-find-other-file-other-window "find other file other window")
    ("p"   projectile-switch-project "switch project >>" :exit t )
    ("r"   projectile-recentf "recent files")
    ("s"   projectile-multi-occur "occur")
    ("S"   projectile-replace "replace")
    ("T"   projectile-find-test-file "find test file")
    ("t"   projectile-test-project "test project")
    ("v"   projectile-display-buffer "display buffer")
    ("V"   projectile-ibuffer "ibuffer")
    ("X"   (lambda ()(interactive)(projectile-run-term "/bin/bash")) "term")
    ("x"   projectile-run-shell "shell"))

  ;; Thanks http://kitchingroup.cheme.cmu.edu/blog/2015/09/28/A-cursor-goto-hydra-for-emacs/
  (defhydra hydra-goto (:color blue :columns 4) "goto"

    ("c" avy-goto-char-2 "2 char")
    ("C" avy-goto-char "char")
    ("L" avy-goto-char-in-line "char in line")
    ("w" avy-goto-word-1 "word")
    ("S" avy-goto-subword-1 "subword")
    ("l" avy-goto-line "line")
    ("I" ace-window "window")

    ("i" swiper "swiper")

    ("s" isearch-forward "search >")
    ("r" isearch-backward "search <")

    ("nn" flycheck-next-error "next error")
    ("np" flycheck-previous-error "prev error")
    ("nl" list-flycheck-errors "error list")
    ("nc" flycheck-compile "refresh errors")

    ("Nn" git-gutter:next-hunk "next git hunk")
    ("Np" git-gutter:previous-hunk "previous git hunk")
    ("Nl" git-gutter:statistic "count git changes")
    ("Ns" git-gutter:popup-hunk "show current git change")

    ("b" ivy-switch-buffer "buffer")
    ("R" counsel-recentf "recentf")
    ("P" hydra-project/body "project >>"))

  (global-set-key (kbd "C-c u") 'hydra-goto/body)
  (setq projectile-switch-project-action
        (lambda ()
          (progn
            (projectile-dired)
            (hydra-project/body))))

  (define-key projectile-mode-map (kbd "C-c p") nil)
  (global-set-key (kbd "C-c p") 'hydra-project/body)

  (global-set-key (kbd "C-c SPC") 'hydra-music/body)
  (add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c SPC") 'hydra-music/body)))

;;  (global-set-key (kbd "C-c s") 'hydra-scala-dev/body))
  (global-set-key (kbd "C-c s") 'sbt-hydra))

(provide 'edd-hydra)
