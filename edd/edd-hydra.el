(use-package hydra
  :ensure t
  :init


  ;; WANT:

  ;; run a build shell e.g. sbt
  ;; run a repl
  ;; run a term

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
  (defhydra hydra-project (:color blue :hint nil :idle 0.4)
    "
    ^^                ^^              ^^                 ^^     ╭──────────────────┐
    ^Files^           ^Search^        ^Buffer^           ^Do^   │ P %s(truncate-string-to-width (projectile-project-name) 15 0 ?  ?…)│
  ╭─^─────^───────────^──────^────────^──────^───────────^──^───┴──────────────────╯
    _f_ file          _a_ ag          _b_ switch         _g_ magit
    _l_ file dwim     _A_ grep        _v_ show all       _i_ info
    _r_ recent file   _s_ occur       _V_ ibuffer        _p_ switch
    _d_ dir           _S_ replace     _K_ kill all
    _o_ other         _t_ find tag
    _u_ test file     _T_ make tags
    _h_ root


    ^^                ^^              ^^                 ^^     ╭──────────────────┐
    ^Other Window^    ^Run^           ^Cache^            ^Do^   │      Fixmee      │
  ╭─^^────────────────^^──────────────^^──────────────╯╭──^^────┴──────────────────╯
    _F_ file          _U_ test        _kc_ clear         _x_ TODO & FIXME
    _L_ dwim          _m_ compile     _kk_ add current   _X_ toggle
    _D_ dir           _c_ shell       _ks_ cleanup
    _O_ other         _C_ command     _kd_ remove
    _B_ buffer


  --------------------------------------------------------------------------------
        "
;        ("<tab>" hydra-master/body "back")
        ("<ESC>" nil "quit")
        ("a"   helm-projectile-ag)
        ("A"   helm-projectile-grep)
        ("b"   projectile-switch-to-buffer)
        ("B"   projectile-switch-to-buffer-other-window)
        ("c"   projectile-run-async-shell-command-in-root)
        ("C"   projectile-run-command-in-root)
        ("d"   projectile-find-dir)
        ("D"   projectile-find-dir-other-window)
        ("f"   projectile-find-file)
        ("F"   projectile-find-file-other-window)
        ("g"   projectile-vc)
        ("h"   projectile-dired)
        ("i"   projectile-project-info)
        ("kc"  projectile-invalidate-cache)
        ("kd"  projectile-remove-known-project)
        ("kk"  projectile-cache-current-file)
        ("K"   projectile-kill-buffers)
        ("ks"  projectile-cleanup-known-projects)
        ("l"   projectile-find-file-dwim)
        ("L"   projectile-find-file-dwim-other-window)
        ("m"   projectile-compile-project)
        ("o"   projectile-find-other-file)
        ("O"   projectile-find-other-file-other-window)
        ("p"   projectile-switch-project :exit t)
        ("r"   projectile-recentf)
        ("s"   projectile-multi-occur)
        ("S"   projectile-replace)
        ("t"   projectile-find-tag)
        ("T"   projectile-regenerate-tags)
        ("u"   projectile-find-test-file)
        ("U"   projectile-test-project)
        ("v"   projectile-display-buffer)
        ("V"   projectile-ibuffer)
        ("X"   fixmee-mode)
        ("x"   fixmee-view-listing))

  ;; Thanks http://kitchingroup.cheme.cmu.edu/blog/2015/09/28/A-cursor-goto-hydra-for-emacs/
  (defhydra hydra-goto (:color blue :hint nil)
    "
^^^^^^^^                                                                  ╭────────────┐
    ^Char^           ^Word^                ^org^              ^find^      │  Navigate  │
  ╭─^^───────────────^^────────────────────^^──────────────────^^─────────┴────────────╯
    _c_ 2 chars      _w_: word by char     _h_: headline       _o_: helm-occur
    _C_ char         _W_: some word        _a_: agenda heading _i_: helm-swiper
    _L_ char in line _s_: subword by char  _q_: swoop buffers  _f_: search forward
    ^ ^              _S_: some subword     ^ ^                 _b_: search backward

    ^^Flycheck^
  ^^^ ───────────────^^────────────────────^^──────────────────^^─────────┴────────────╯
    _nn_ next issue
    _np_ prev issue
    _nl_ list issues
    _nc_ compile


    _B_: helm-buffers       _l_: avy-goto-line
    _m_: helm-mini          _I_: ace-window
    _R_: helm-recentf

    _P_: Project            _._: mark position _/_: jump to mark
"
    ("c" avy-goto-char-2)
    ("C" avy-goto-char)
    ("L" avy-goto-char-in-line)
    ("w" avy-goto-word-1)
    ;; jump to beginning of some word
    ("W" avy-goto-word-0)
    ;; jump to subword starting with a char
    ("s" avy-goto-subword-1)
    ;; jump to some subword
    ("S" avy-goto-subword-0)

    ("l" avy-goto-line)
    ("I" ace-window)

    ("h" helm-org-headlines)
    ("a" helm-org-agenda-files-headings)
    ("q" helm-multi-swoop-org)

    ("o" helm-occur)
    ("i" swiper-helm)

    ("f" isearch-forward)
    ("b" isearch-backward)

    ("nn" flycheck-next-error)
    ("np" flycheck-previous-error)
    ("nl" list-flycheck-errors)
    ("nc" flycheck-compile)

    ("." org-mark-ring-push :color red)
    ("/" org-mark-ring-goto :color blue)
    ("B" helm-buffers-list)
    ("m" helm-mini)
    ("R" helm-recentf)
    ("P" hydra-project/body))

  (global-set-key (kbd "C-c .") 'hydra-goto/body)
  (setq projectile-switch-project-action
        (lambda ()
          (progn
            (projectile-dired)
            (hydra-project/body))))

  (define-key projectile-mode-map (kbd "C-c p") nil)
  (global-set-key (kbd "C-c p") 'hydra-project/body))

(provide 'edd-hydra)
