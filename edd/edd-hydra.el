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
  (defhydra hydra-music (:columns 3 :timeout 2)
    "Music"
    ("a"   emms-browse-by-album "browse (album)" :exit t)
    ("SPC" emms-pause "play/pause")
    ("i"   emms-show "show info")
    ("I"   emms-show-all "show tags" :exit t)
    ("n"   emms-next "next track")
    ("p"   emms-previous "previous track")
    ("+"   emms-volume-raise "volume up")
    ("-"   emms-volume-lower "volume down")
    ("P"   emms-playlist-mode-switch-buffer "playlist" :exit t))

  ;; projectile
  ;; https://github.com/abo-abo/hydra/wiki/Projectile-&-Fixmee
  ;;
  (defhydra hydra-project (:exit t :idle 0.4 :columns 5) "Project"
    ("<ESC>" nil "quit")
    ("a"   counsel-projectile-rg "rg")
    ("A"   projectile-grep "grep")
    ("b"   counsel-projectile-switch-to-buffer "buffer")
    ("B"   projectile-switch-to-buffer-other-window "buffer other window")
    ("c"   projectile-run-command-in-root "command")
    ("C"   projectile-run-async-shell-command-in-root "shell command")
    ("d"   counsel-projectile-find-dir "find dir")
    ("D"   projectile-find-dir-other-window "find dir other window")
    ("e"   project-find-file "find file (any file under root)")
    ("E"   project-find-regexp "find regexp (any file under root)")
    ("f"   counsel-projectile-find-file "find file")
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
    ("p"   projectile-switch-project "switch project >>")
    ("r"   projectile-recentf "recent files")
    ("s"   projectile-multi-occur "occur")
    ("S"   projectile-replace "replace")
    ("t"   projectile-toggle-between-implementation-and-test "flip between test and implementation")
    ("T"   projectile-test-project "test project")
    ("v"   projectile-display-buffer "display buffer")
    ("V"   projectile-ibuffer "ibuffer")
    ("X"   (lambda ()(interactive)(projectile-run-term "/bin/bash")) "term")
    ("x"   projectile-run-shell "shell"))

  ;; from wiki
  (defhydra hydra-goto-line (goto-map ""
                                      :pre (linum-mode 1)
                                      :post (linum-mode -1))
    "goto-line"
    ("g" goto-line "go")
    ("m" set-mark-command "mark" :bind nil)
    ("q" nil "quit"))


  (defhydra hydra-mark-modify
    (:columns 3)  "Move/Modify"
    ("p" mc/mark-previous-lines "mark previous line (mc)")
    ("P" mc/mark-previous-like-this-symbol "mark prev match/symbol (mc)")
    ("n" mc/mark-next-lines "mark next line (mc)")
    ("N" mc/mark-next-like-this-symbol "mark next match/symbol (mc)")

    ("a" mc/edit-beginnings-of-lines "edit beginnings of lines (mc)")
    ("e" mc/edit-ends-of-lines "edit beginnings of lines (mc)")

    ("*" mc/mark-all-symbols-like-this-in-defun "mark all in defun (mc)")
    ("m" er/expand-region "expand region (er)")
    (";" er/mark-comment "mark comment (er)")
    ("{" er/mark-defun "mark defun (er)")
    ("." er/mark-next-accessor "mark accessor (er)")

    ("(" er/mark-inside-pairs "mark pair contents (er)")
    (")" er/mark-outside-pairs "mark pair (er)")
    ("'" er/mark-inside-quotes "mark quote contents (er)")
    ("\"" er/mark-outside-quotes "mark quote (er)")

    ("#" mc/insert-numbers "insert numbers (mc)"))


  ;; Thanks http://kitchingroup.cheme.cmu.edu/blog/2015/09/28/A-cursor-goto-hydra-for-emacs/
  (defhydra hydra-goto (:exit nil :columns 4) "goto"

    ("c" avy-goto-char-2 "2 char")
    ("C" avy-goto-char "char")
    ("L" avy-goto-char-in-line "char in line")
    ("w" avy-goto-word-1 "word")
    ("S" avy-goto-subword-1 "subword")
    ("l" avy-goto-line "line")
    ("I" ace-window "window")

    ("i" swiper "swiper" :exit t)

    ("s" isearch-forward "search >")
    ("r" isearch-backward "search <")

    ("nn" flycheck-next-error "next error")
    ("np" flycheck-previous-error "prev error")
    ("nl" list-flycheck-errors "error list" :exit t)
    ("nc" flycheck-compile "refresh errors")

    ("Nn" git-gutter:next-hunk "next git hunk")
    ("Np" git-gutter:previous-hunk "previous git hunk")
    ("Nl" git-gutter:statistic "count git changes")
    ("Ns" git-gutter:popup-hunk "show current git change")

    ("b" ivy-switch-buffer "buffer")
    ("R" counsel-recentf "recentf" :exit t)
    ("P" hydra-project/body "project >>" :exit t))

  (setq projectile-switch-project-action
        (lambda ()
          (progn
            (projectile-dired)
            (hydra-project/body))))

  (define-key projectile-mode-map (kbd "C-c p") nil)

  (add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c SPC") 'hydra-music/body)))

  (global-set-key (kbd "C-c SPC") 'hydra-music/body)
  (global-set-key (kbd "C-c m") 'hydra-mark-modify/body)
  (global-set-key (kbd "C-c p") 'hydra-project/body)
  (global-set-key (kbd "C-c s") 'sbt-hydra)
  (global-set-key (kbd "C-c o") 'hydra-goto/body))

(provide 'edd-hydra)
