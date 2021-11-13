;;(toggle-debug-on-error)
(add-to-list 'load-path (locate-user-emacs-file "edd"))
(require 'edd-bootstrap)
(edd/maybe-load-config "local-pre.el")
(defvar edd/emms-consul-p t "Whether to do consul stuff with emms")

;; Do this stuff early to avoid flicker
;;
(use-package edd-ux :straight nil)

;; built-in features
;;
(use-package edd-features :straight nil)

;; org-mode, as good as built-in
;;
(use-package edd-org :straight nil)

;; System-specific stuff.
;;
(use-package edd-mac :if (eq 'darwin system-type) :straight nil)
(use-package edd-linux :if (not (eq 'darwin system-type)) :straight nil)
(use-package restart-emacs)

;; whitespace
;;
(use-package whitespace
  :delight whitespace-mode
  :hook
  (((prog-mode text-mode conf-mode) . whitespace-mode))
  :config
  (setq-default
   whitespace-style '(face trailing tabs empty indentation)
   indent-tabs-mode nil)

  :custom-face
  (whitespace-empty ((nil :foreground "disabledControlTextColor" :background "#333333")))
  (whitespace-line ((nil :background "disabledControlTextColor" :foreground "#333333")))
  (whitespace-indentation ((nil :background "disabledControlTextColor" :foreground "#333333")))
  (whitespace-trailing ((nil :background "disabledControlTextColor" :foreground "#333333"))))

(use-package whitespace-cleanup-mode
  :config
  (global-whitespace-cleanup-mode 1))

(use-package ace-window
  :bind
  ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq avi-keys '(?a ?s ?d ?e ?f ?h ?j ?k ?l ?n ?m ?v ?r ?u))
  (setq aw-background t))

(use-package which-key :config (which-key-mode))

(use-package edd-hydra :demand t :straight nil)

(use-package edd-proj :straight nil)

(use-package edd-ivy :after projectile :straight nil)

;; utilities that are too small to live alone
;;
(use-package edd-util
  :demand t
  :straight nil
  :bind
  (("C-w" . kill-region-or-backward-kill-word)
   ("C-c M-p" . edd-jump-to-prev-url)
   ("C-c M-n" . edd-jump-to-next-url)
   ("C-x k". edd-kill-a-buffer)
   ("C-c !". edd-config-reload)))

(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

(use-package emacs
  :hook
  ((scheme-mode elisp-mode) . my-pretty-lambda)
  :init
  (global-prettify-symbols-mode 1)
  :config
  (defun my-pretty-lambda ()
    "make some word or string show as pretty Unicode symbols"
    (setq prettify-symbols-alist
          '(
          ("lambda" . 955) ;; λ
          ))))

;; secret config -- used below.
(use-package edd-secrets :commands edd-with-secrets :straight nil)

(use-package edd-erc :straight nil)

(use-package company
  :commands (company-mode)
  :demand
  :delight
  (company-search-mode)
  (company-mode))

(use-package edd-gtd
  :straight nil
  :commands (edd/go-home)
  :bind
  (("C-c w" . edd/go-to-work)))
;(use-package edd-mail :straight nil)
(use-package pdf-tools
  :init
  (pdf-tools-install))

(use-package edd-scala :straight nil)
(use-package edd-haskell :straight nil)
(use-package edd-ruby :straight nil)

(use-package flycheck
  :delight " 🛂"
  :hook
  ((rust-mode go-mode scala-mode ruby-mode) . flycheck-mode)
  ((sbt-file-mode) . (lambda () (flycheck-mode -1)))
  :config
  (setq flycheck-scalastyle-jar
        (expand-file-name "scalastyle/scalastyle_2.10-batch.jar"))
  (setq flycheck-scalastylerc
        (expand-file-name "scalastyle/scalastyle-config.xml")))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package auto-highlight-symbol
  :hook
  (prog-mode . auto-highlight-symbol-mode)
  :delight)

(use-package git-gutter
  :delight
  :config
  (global-git-gutter-mode 1))

(use-package magit
  :demand t
  :delight with-editor-mode
  :mode ("CODEOWNERS$" . gitignore-mode)  
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-commit-arguments '("--gpg-sign")))

(use-package magit-filenotify :demand t)

(use-package magit-delta
  :hook
  ((magit-mode-hook) . (lambda () (magit-delta-mode +1))))

(use-package edd-ledger :straight nil)

(use-package tea-time)

(use-package markdown-mode+
  :mode ("\\.apib\\$" . markdown-mode))

(use-package imenu-anywhere
  :hook
  (emacs-lisp-mode . jcs-use-package)
  :config
  (defun jcs-use-package ()
    (add-to-list
     'imenu-generic-expression
     '("Used Packages"
       "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2))))

;; smoother scrolling
(use-package smooth-scrolling
  :hook
  ((term-mode comint) . (lambda () (setq-local scroll-margin 0)))
  :config
  (setq smooth-scroll-margin 5
        scroll-conservatively 101
        scroll-preserve-screen-position t
        auto-window-vscroll nil
        scroll-margin 5))

(use-package company
  :config
  (global-company-mode))

(use-package company-emoji
  :config
  (add-to-list 'company-backends 'company-emoji))

(use-package quickrun
  :bind
  (("C-c q q" . quickrun)
   ("C-c q r" . quickrun-region)))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

;; wrap-region
(use-package wrap-region
  :delight wrap-region-mode
  :hook
  ((org-mode latex-mode prog-mode) . wrap-region-mode)
  :config
  (wrap-region-add-wrappers
   '(("*" "*" nil org-mode)
     ("~" "~" nil org-mode)
     ("/" "/" nil org-mode)
     ("=" "=" "+" org-mode)
     ("_" "_" nil org-mode)
     ("/*" "*/" "/" (scala-mode java-mode))
     ("$" "$" nil (org-mode latex-mode)))))

(use-package docker-tramp)
(use-package dockerfile-mode)

(use-package edd-rust :straight nil)

(use-package edd-go :straight nil)

;;preview files in dired
(use-package peep-dired
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package volatile-highlights
  :delight
  :config
  (volatile-highlights-mode t))

(use-package multi-term)

(use-package smartparens
  :delight " 🎷"
  :hook
  ((prog-mode markdown-mode org-mode) . turn-on-smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "(" ")" :wrap "C-c (")
  (sp-pair "[" "]" :wrap "C-c [")
  (sp-pair "{" "}" :wrap "C-c {")
  (sp-pair "'" "'" :wrap "C-c '")
  (sp-pair "\"" "\"" :wrap "C-c \"")

  (defun edd/rww-paren (&optional arg)
    "rewrap with ()"
    (interactive "p")
    (sp-rewrap-sexp '("(" . ")")))

  (defun edd/rww-bracket (&optional arg)
    "rewrap with []"
    (interactive "p")
    (sp-rewrap-sexp '("[" . "]")))

  (defun edd/rww-brace (&optional arg)
    "rewrap with {}"
    (interactive "p")
    (sp-rewrap-sexp '("{" . "}")))

  :bind
  (:map smartparens-mode-map
        ("C-M-a" . sp-beginning-of-sexp)
        ("C-M-e" . sp-end-of-sexp)

        ("C-M-d"   . sp-down-sexp)
        ("C-M-S-u" . sp-up-sexp)
        ("C-M-S-d" . sp-backward-down-sexp)
        ("C-M-u"   . sp-backward-up-sexp)

        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)

        ("C-M-n" . sp-next-sexp)
        ("C-M-p" . sp-previous-sexp)

        ("C-S-f" . sp-forward-symbol)
        ("C-S-b" . sp-backward-symbol)

        ("C-<right>" . sp-forward-slurp-sexp)
        ("C-<left>"  . sp-backward-slurp-sexp)
        ("C-M-<right>" . sp-forward-barf-sexp)
        ("C-M-<left>"  . sp-backward-barf-sexp)

        ("C-M-t" . sp-transpose-sexp)
        ("C-M-k" . sp-kill-sexp)
        ("C-k"   . sp-kill-hybrid-sexp)
        ("M-k"   . sp-backward-kill-sexp)
        ("C-M-w" . sp-copy-sexp)

        ("C-<backspace>" . sp-backward-kill-word)

        ("M-[" . sp-backward-unwrap-sexp)
        ("M-]" . sp-unwrap-sexp)

        ("C-c )"  . edd/rww-paren)
        ("C-c ]"  . edd/rww-bracket)
        ("C-c }"  . edd/rww-brace)
        ("C-x C-t" . sp-transpose-hybrid-sexp)
        :map prog-mode-map
        ;; This conflicts in org mode
        ("M-<left>"  . sp-backward-barf-sexp)
        ("M-<right>" . sp-forward-barf-sexp)
))

(use-package anzu
  :delight anzu-mode
  :init
  (global-anzu-mode +1)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))

(use-package emms
  :load-path "../src/emms/lisp"
  :hook
  (emms-player-started . edd/emms-tell-consul)
  :commands (emms-smart-browse emms-pause emms-browse-by-album)
  :init
  (setq default-major-mode 'fundamental-mode) ;; shim for emms to work
  (require 'emms-setup)
  (setq emms-source-file-default-directory (expand-file-name "~/media/music"))
  (setq emms-playing-time-display-format " %s")
  (setq emms-playing-time-display-short-p 1)

  (require 'emms-tag-editor)
  (require 'emms-info)

  ;; Use only libtag for tagging.
  (require 'emms-info-libtag)
  (setq emms-info-functions '(emms-info-libtag))
  (setq emms-info-libtag-program-name (expand-file-name "~/bin/emms-print-metadata"))
  (setq emms-volume-change-function 'emms-volume-pulse-change)
  (setq emms-show-format "%s")

  :config
  (emms-all)
  (emms-default-players)
  (require 'json)
  ;;; Display album in playlist
  (defun ambrevar/emms-artist-album-track-and-title-format (bdata fmt)
    (concat
     "%i"
     (let ((artist (emms-browser-format-elem fmt "a")))
       (if (not artist)
           "%n"                    ; If unknown, display the filename.
         (concat
          "%a - "
          (let ((album (emms-browser-format-elem fmt "A")))
            (if album "%A - " ""))
          (let ((disc (emms-browser-format-elem fmt "D")))
            (if (and disc (not (string= disc ""))) "%D/" ""))
          (let ((track (emms-browser-format-elem fmt "T")))
            (if (and track (not (string= track "0")))
                "%T. "
              ""))
          "%t [%d]")))))
  (setq emms-browser-playlist-info-title-format 'ambrevar/emms-artist-album-track-and-title-format)

;; Display disc number in browser
(defun ambrevar/emms-browser-track-artist-and-title-format (bdata fmt)
  (concat
   "%i"
   (let ((disc (emms-browser-format-elem fmt "D")))
     (if (and disc (not (string= disc "")))
         "%D/"))
   (let ((track (emms-browser-format-elem fmt "T")))
     (if (and track (not (string= track "0")))
         "%T. "
       ""))
   "%n"))
(setq emms-browser-info-title-format 'ambrevar/emms-browser-track-artist-and-title-format)
  (defun edd/emms-modeline ()
    (concat " 🎶 "
            (let ((s (emms-track-get (emms-playlist-current-selected-track) 'info-title
                                     (emms-mode-line-playlist-current))))
              (substring s
                         0 (min 20 (length s))))))

  (defun edd/emms-tell-consul ()
    (when edd/emms-consul-p
      (let*
          ((artist (emms-track-get (emms-playlist-current-selected-track) 'info-artist))
           (title (emms-track-get (emms-playlist-current-selected-track) 'info-title))
           (album (emms-track-get (emms-playlist-current-selected-track) 'info-album))
           (time (string-to-number (format-time-string "%s000")))
           (json (json-encode-alist
                  (list (cons :artist artist)
                        (cons :title title)
                        (cons :album album)
                        (cons :time time)))))
        (start-process "np" "*tell-consul*" "b" "np" "set" json))))

  (setq emms-mode-line-mode-line-function 'edd/emms-modeline)
  (defun edd/emms-start-or-previous ()
    (interactive)
    (when emms-player-playing-p
      (emms-stop))
    (when (< emms-playing-time 10)
        (emms-playlist-current-select-previous))
    (emms-start))

  (defun edd/emms-info-track-description (track)
    "Return a description of TRACK."
    (let ((artist (emms-track-get track 'info-artist))
          (title  (emms-track-get track 'info-title)))
      (cond
       ((and artist title)
        (concat artist " — " title))
       (title
        title)
       (t
        (emms-track-simple-description track)))))
  (setq emms-track-description-function #'edd/emms-info-track-description)

  ;; This is like (emms-show) but doesn't display or insert, just returns the string
  (defun edd/emms-now-playing ()
    (if emms-player-playing-p
                    (format emms-show-format
                            (emms-track-description
                             (emms-playlist-current-selected-track)))
      ""))

  (setq emms-mode-line-mode-line-function 'edd/emms-modeline)
  :bind (("<f8>" . emms-pause)
         ("<f7>" . edd/emms-start-or-previous)
         ("<f9>" . emms-next)
         ("C-M-s-p" . emms-playlist-mode-switch-buffer)
         ("C-M-s-n" . emms-browse-by-album)))

(use-package dumb-jump
  :bind
  (("M-g o" . dumb-jump-go-other-window)
   ("M-g j" . dumb-jump-go)
   ("M-g q" . dumb-jump-quick-look)
   ([remap xref-find-definitions] . dumb-jump-go)
   ([remap xref-pop-marker-stack] . dumb-jump-back)
   )
  :config         
  (setq dumb-jump-selector 'ivy))

(use-package helm-make
  :config
  (setq helm-make-completion-method 'ivy)
  (setq helm-make-comint 't))

(use-package csv)
(use-package elm-mode)
(use-package gradle-mode
  :init
  (setq gradle-gradlew-executable (expand-file-name "gradlew" (projectile-project-root))))

(use-package groovy-mode)
(use-package php-mode)
(use-package play-routes-mode)
(use-package rjsx-mode
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-strict-trailing-comma-warning nil))
(use-package less-css-mode)
(use-package yaml-mode)
(use-package idris-mode
  :delight (idris-simple-indent-mode)
  :config
  (defun edd/idris-next-hole ()
      (interactive)
      (search-forward " ?" nil t))
  :bind
  (:map idris-mode-map
        ("C-c C-j" . idris-pop-to-repl)
        ("C-c C-f" . edd/idris-next-hole)))

(use-package cider)
(use-package protobuf-mode
  :hook
  (protobuf-mode . edd-protobuf/set-style)
  :config
  (defun edd-protobuf/set-style ()
    (c-add-style
     "my-style"
     '((c-basic-offset . 2) (indent-tabs-mode . nil)))))

(use-package hcl-mode
  :mode ("\\.tf$" . hcl-mode))

(use-package dired-collapse
  :config
  (add-hook 'dired-mode-hook (lambda () (dired-collapse-mode 1))))

(use-package multiple-cursors)

(use-package edd-sow
  :straight nil
  :config
  (edd-sow-mode 1))

(use-package edd-git-web-link
  :straight nil
  :bind
  ("C-c g" . hydra-edd-git-web-link/body))

(use-package iedit)

(use-package wgrep
  :after grep
  :bind
  (:map grep-mode-map
        ("C-x C-q" . wgrep-change-to-wgrep-mode)
        ("C-c C-c" . wgrep-finish-edit)))

(use-package ivy-lobsters)
(use-package direnv
  :config
  (direnv-mode))

(use-package browse-at-remote)
(use-package atomic-chrome)
(use-package gh)

(use-package python-mode)
(use-package virtualenvwrapper
  :config
  (venv-initialize-interactive-shells)
  (setq venv-dirlookup-names '(".venv" "pyenv" ".virtual" ".env")))

(use-package evil-numbers
  :bind
  ("C-c +" . 'evil-numbers/inc-at-pt)
  ("C-c -" . 'evil-numbers/dec-at-pt))

(use-package deft
  :bind
  ("C-c d" . 'deft)
  :config
  (setq deft-directory "~/txt"
        deft-text-mode 'org-mode
        deft-extensions '("org" "txt" "md")
        deft-recursive t
        deft-new-file-format "%Y%m%d-"))

(use-package engine-mode
  :commands
  (engine/search-github engine/search-google)
  :config
  (engine-mode 1)
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "/"))

(use-package edit-server
  :init
  (edit-server-start))

(use-package copy-as-format
  :bind
  ("C-c M-w s" . copy-as-format-slack)
  ("C-c M-w j" . copy-as-format-jira)
  ("C-c M-w g" . copy-as-format-github))

(use-package epa
  :config
  (setq epa-pinentry-mode 'loopback))

;; let's use firefox
(use-package browse-url
  :config
  (setq browse-url-browser-function 'browse-url-firefox))

(use-package make-mode
  :mode ("Makefile.inc" . makefile-mode))

(use-package kotlin-mode
  :mode ("build.gradle.kts" . kotlin-mode)
  :bind
  ("C-c i" . edd-kt/sort-imports)
  :config
  (setq kotlin-tab-width 4)
  (setq gradle-use-gradlew t)
  (setq gradle-gradlew-executable (expand-file-name "gradlew" (projectile-project-root)))
  (defhydra+ hydra-project nil "Project"
    ("m" gradle-execute "execute gradle task"))
  (defun edd-kt/sort-imports ()
    (interactive)
    (let
        ((macro [?\M-< ?\C-s ?i ?m ?p ?o ?r ?t return ?\C-a ?\M-h
          ?\C-n ?\M-% ?i ?m ?p ?o ?r ?t ? ?j ?a ?v ?a return ?i
          ?m ?p ?\[ ?o ?r ?t backspace backspace backspace
          backspace ?o ?r ?t ? ?z ?z ?z ?j ?a ?v ?a return ?!
          ?\M-h ?\C-n ?\C-x ?\C-m ?s ?o ?r ?t ? ?l ?i ?n ?e ?s
          return ?\M-h ?\C-n ?\M-% ?z ?z ?z ?j ?a ?v ?a return ?j
          ?a ?v ?a return ?! ?\C-n]))
      (execute-kbd-macro macro)))

  :init
  (add-to-list
   'compilation-error-regexp-alist
   'kotlin-gradle)
  (add-to-list
   'compilation-error-regexp-alist
   'kotlin-lint)
  (add-to-list
   'compilation-error-regexp-alist-alist
   '(kotlin-gradle
     "^e: \\(.*\\): (\\([0-9]+\\), \\([0-9]+\\))" 1 2 3))
  (add-to-list
   'compilation-error-regexp-alist-alist
   '(kotlin-lint
     "^Lint error > \\(.*\\):\\([0-9]+\\):\\([0-9]+\\):" 1 2 3)))

(use-package flycheck-kotlin
  :commands flycheck-kotlin-setup
  :after flycheck
  :config
  (flycheck-kotlin-setup))

(use-package olivetti)
(use-package lua-mode)

(use-package cc-mode
  :hook
  (java-mode-hook . (lambda () (c-set-offset 'statement-cont '++))))

(use-package nov)

(use-package sml-mode)

(use-package nix-mode)

(edd/maybe-load-config "local.el")
;; acknowledgements
;;
;; http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
;; http://sachachua.com/blog/2014/12/emacs-configuration-use-package/
;; http://pages.sachachua.com/.emacs.d/Sacha.html
;; https://github.com/mwfogleman/config/blob/master/home/.emacs.d/michael.org
;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md
;; https://github.com/bodil/emacs.d/blob/master
;; https://github.com/shosti/.emacs.d/blob/master/personal/p-jabber.el
;; http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.
;; http://pragmaticemacs.com/emacs/wrap-text-in-custom-characters/
;; http://lists.madduck.net/pipermail/vcs-home/2013-August/000880.html
;; http://anbasile.github.io/2016/12/02/org-babel-is-cool/
(defun org())
