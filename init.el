(package-initialize)
(add-to-list 'load-path (locate-user-emacs-file "edd"))
(require 'edd-bootstrap)
(edd/maybe-load-config "local-pre.el")

;; Do this stuff early to avoid flicker
;;
(use-package edd-ux
  :ensure nil)

;; built-in features
;;
(use-package edd-features
  :ensure nil)

;; System-specific stuff.
;;
(use-package edd-mac
  :ensure nil
  :if (eq 'darwin system-type))

(use-package edd-linux
  :ensure nil
  :if (not (eq 'darwin system-type)))

;; whitespace
;;
(use-package whitespace
  :diminish whitespace-mode
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook 'whitespace-cleanup)
  :config
  (setq-default
   whitespace-style '(face trailing tabs empty indentation)
   indent-tabs-mode nil)
  (set-face-attribute whitespace-line nil :background "disabledControlTextColor" :foreground "controlBackgroundColor")
  (set-face-attribute whitespace-indentation nil :background "disabledControlTextColor" :foreground "controlBackgroundColor")
  (set-face-attribute whitespace-trailing nil :background "disabledControlTextColor" :foreground "controlBackgroundColor"))

;; Ace window
(use-package ace-window
  :bind
  ("C-x o" . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq avi-keys '(?a ?s ?d ?e ?f ?h ?j ?k ?l ?n ?m ?v ?r ?u))
  (setq aw-background nil))

(use-package which-key)

(use-package edd-ivy
  :ensure nil)

;; projectile
;; http://endlessparentheses.com/improving-projectile-with-extra-commands.html
;;
(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-x p"))

  :config
  (add-hook 'text-mode-hook #'projectile-mode)
  (add-hook 'prog-mode-hook #'projectile-mode)

  (setq projectile-mode-line
        '(:eval (format " 🔖%s" (projectile-project-name))))

  :bind
  ("C-c r" .
   (lambda () (interactive)
     (projectile-with-default-dir (projectile-project-root)
       (call-interactively 'comint-run)))))

;; utilities that are too small to live alone
;;
(use-package edd-util
  :ensure nil
  :demand t
  :bind
  (("C-w" . kill-region-or-backward-kill-word)
   ("C-c M-p" . edd-jump-to-prev-url)
   ("C-c M-n" . edd-jump-to-next-url)
   ("C-x k". edd-kill-a-buffer)
   ("C-c !". edd-config-reload)))

;; RE-Builder
;;
(use-package re-builder
  :config
  (setq reb-re-syntax 'string))

;; vagrant
(use-package vagrant
  :config
  (defun edd-vagrant-edit ()
    (interactive)
    "edit the local Vagrantfile"
    (require 'vagrant)
    (find-file (concat (vagrant-locate-vagrantfile) "Vagrantfile")))
  :bind
  (("C-c v u" . vagrant-up)
   ("C-c v p" . vagrant-provision)
   ("C-c v d" . vagrant-destroy)
   ("C-c v v" . vagrant-ssh)
   ("C-c v e" . edd-vagrant-edit)))



(use-package prog-mode
  :ensure nil
  :config
  (defun my-pretty-lambda ()
    "make some word or string show as pretty Unicode symbols"
    (setq prettify-symbols-alist
          '(
          ("lambda" . 955) ; λ
          )))
  (add-hook 'scheme-mode-hook 'my-pretty-lambda)
  (add-hook 'elisp-mode-hook 'my-pretty-lambda)
  (global-prettify-symbols-mode 1))


;; secret config -- used below.
(use-package edd-secrets
  :ensure nil
  :commands edd-with-secrets)

(use-package edd-erc :ensure nil)

(use-package edd-haskell :ensure nil)

(use-package "company"
  :ensure company-ghc
  :commands (company-mode)
  :diminish (company-search-mode company-mode)
  :config
  (progn
    (add-to-list 'company-backends 'company-ghc)
    (custom-set-variables '(company-ghc-show-info t))))

(use-package edd-org
  :ensure nil
  :config
  )


(use-package edd-mail
  :ensure nil
  :init
  (setq mail-specify-envelope-from t
        mail-envelope-from 'header
        send-mail-function 'sendmail-send-it
        message-sendmail-envelope-from 'header
        message-send-mail-function 'message-send-mail-with-sendmail)
  :bind ("C-c n" . edd-mailbox))

(use-package edd-pdf :ensure nil)

(use-package edd-scala :ensure nil)

(use-package flycheck
  :diminish (flycheck-mode . " 🛂")
  :config
  (setq flycheck-scalastyle-jar
        (expand-file-name "scalastyle/scalastyle_2.10-batch.jar"))
  (setq flycheck-scalastylerc
        (expand-file-name "scalastyle/scalastyle-config.xml"))
  (add-hook 'rust-mode-hook #'flycheck-mode)
  (add-hook 'go-mode-hook #'flycheck-mode)
  (add-hook 'scala-mode-hook
            (lambda () (unless (and (buffer-file-name (current-buffer)) (eq (file-name-extension (buffer-file-name (current-buffer))) "sbt"))
                    (flycheck-mode 1)))))

(use-package rainbow-delimiters
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package auto-highlight-symbol
  :init
  (add-hook 'prog-mode-hook #'auto-highlight-symbol-mode)
  :diminish auto-highlight-symbol-mode)

(use-package git-gutter
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode 1))

(use-package magit
  :demand t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq magit-commit-arguments '("--gpg-sign")))

(use-package magit-filenotify :demand t)

(use-package ledger-mode
  :mode ("\\.ledger$" "ledger\\.dat$")
  :config
  (setq ledger-post-auto-adjust-amounts t))

(use-package tea-time)

(use-package bbdb
  :config (bbdb-initialize)
  :commands bbdb)

(use-package markdown-mode+
  :mode (("\\.apib\\$" . markdown-mode)))

(use-package imenu-anywhere
  :config (defun jcs-use-package ()
            (add-to-list 'imenu-generic-expression
             '("Used Packages"
               "\\(^\\s-*(use-package +\\)\\(\\_<.+\\_>\\)" 2)))
  (add-hook 'emacs-lisp-mode-hook #'jcs-use-package))


;; smoother scrolling
(use-package smooth-scrolling
  :init
  (setq smooth-scroll-margin 5
        scroll-conservatively 101
        scroll-preserve-screen-position t
        auto-window-vscroll nil)
  :config
  (setq scroll-margin 5)
  (dolist (hook '(term-mode-hook comint-hook))
    (add-hook hook (lambda () (setq-local scroll-margin 0)))))

(use-package company-emoji
  :config
  (add-to-list 'company-backends 'company-emoji))

(global-company-mode)

(use-package ssh
  :commands ssh
  :config
  (add-hook 'ssh-mode-hook
            (lambda ()
              (setq ssh-directory-tracking-mode t)
              (shell-dirtrack-mode t)
              (setq dirtrackp nil))))

(use-package quickrun
  :bind
  (("C-c q q" . quickrun)
   ("C-c q r" . quickrun-region)))

(use-package expand-region
  :bind
  (("C-=" . er/expand-region)))

;; wrap-region
(use-package wrap-region
  :diminish 'wrap-region-mode
  :config
  (wrap-region-add-wrappers
   '(("*" "*" nil org-mode)
     ("~" "~" nil org-mode)
     ("/" "/" nil org-mode)
     ("=" "=" "+" org-mode)
     ("_" "_" nil org-mode)
     ("/*" "*/" "/" (scala-mode java-mode))
     ("$" "$" nil (org-mode latex-mode))))
  (add-hook 'org-mode-hook 'wrap-region-mode)
  (add-hook 'latex-mode-hook 'wrap-region-mode)
  (add-hook 'prog-mode-hook 'wrap-region-mode))

(use-package docker-tramp)
(use-package dockerfile-mode)

(use-package edd-hydra
  :ensure nil
  :demand t)

(use-package edd-rust :ensure nil)

(use-package edd-go :ensure nil)

;;preview files in dired
(use-package peep-dired
  :defer t ; don't access `dired-mode-map' until `peep-dired' is loaded
  :bind (:map dired-mode-map
              ("P" . peep-dired)))

(use-package volatile-highlights
  :diminish volatile-highlights-mode
  :config
  (volatile-highlights-mode t))

(use-package multi-term)

(use-package smartparens
  :diminish " 🎷"
  :config
  :init
  (add-hook 'prog-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'markdown-mode-hook 'turn-on-smartparens-strict-mode)
  (add-hook 'org-mode-hook 'turn-on-smartparens-strict-mode)
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
        ("M-<right>" . sp-forward-barf-sexp)
        ("C-<left>"  . sp-backward-slurp-sexp)
        ("M-<left>"  . sp-backward-barf-sexp)

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
        ("C-x C-t" . sp-transpose-hybrid-sexp)))

(use-package anzu
  :diminish anzu-mode
  :init
  (global-anzu-mode +1)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))


(use-package emms
  :load-path "../src/emms/lisp"
  :commands (emms-smart-browse emms-pause)
  :init
  (setq default-major-mode 'fundamental-mode) ;; shim for emms to work
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
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

  :config
  (defun edd/emms-modeline ()
    (concat " 🎶 "
            (let ((s (emms-track-get (emms-playlist-current-selected-track) 'info-title
                                     (emms-mode-line-playlist-current))))
              (substring s
                         0 (min 20 (length s))))))
  (setq emms-mode-line-mode-line-function 'edd/emms-modeline)
  :bind (("<f5>" . emms-pause)))

(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g q" . dumb-jump-quick-look))
  :config
  (with-eval-after-load 'dumb-jump
    (define-key global-map [remap xref-find-definitions] 'dumb-jump-go)
    (define-key global-map [remap xref-pop-marker-stack] 'dumb-jump-back)
    )

  (setq dumb-jump-selector 'ivy))

(use-package helm-make
  :config
  (setq helm-make-completion-method 'ivy)
  (setq helm-make-comint 't))

(use-package edd-rss :ensure nil)

(use-package csv)
(use-package elm-mode)
(use-package gradle-mode)
(use-package groovy-mode)
(use-package php-mode)
(use-package play-routes-mode)
(use-package projectile-ripgrep)
(use-package rjsx-mode
  :config
  (setq js2-strict-missing-semi-warning nil)
  (setq js2-strict-trailing-comma-warning nil))
(use-package less-css-mode)
(use-package yaml-mode)
(use-package idris-mode
  :config
  (defun edd/idris-next-hole ()
      (interactive)
      (search-forward " ?" nil t))
  (eval-after-load "idris-simple-indent" '(diminish 'idris-simple-indent-mode))
  :bind
  (:map idris-mode-map
        ("C-c C-j" . idris-pop-to-repl)
        ("C-c C-f" . edd/idris-next-hole)))


(use-package cider)

(use-package protobuf-mode
  :init
  (defconst my-protobuf-style
    '((c-basic-offset . 2)
       (indent-tabs-mode . nil)))

   (add-hook 'protobuf-mode-hook
     (lambda () (c-add-style "my-style" my-protobuf-style t))))

(use-package gitignore-mode
  :mode ("CODEOWNERS$" . gitignode-mode))

(use-package hcl-mode
  :mode ("\\.tf$" . hcl-mode))

(use-package dired-collapse
  :config
  (add-hook 'dired-mode-hook (lambda () (dired-collapse-mode 1))))

(use-package multiple-cursors)

(use-package edd-sow
  :demand
  :ensure nil
  :config
  (edd-sow-mode 1))

(use-package restart-emacs)

(use-package edd-git-web-link :ensure nil
  :bind
  ("C-c g" . hydra-edd-git-web-link/body))

(use-package iedit)

(use-package wgrep
  :init
  (eval-after-load 'grep
    '(define-key grep-mode-map
       (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode))

  (eval-after-load 'wgrep
    '(define-key grep-mode-map
       (kbd "C-c C-c") 'wgrep-finish-edit)))


(use-package ivy-lobsters)
(use-package direnv)

(use-package browse-at-remote)
(use-package atomic-chrome)
(use-package gh)

(use-package python-mode)
(use-package virtualenvwrapper
  :config
  (venv-initialize-interactive-shells)
  (setq venv-dirlookup-names '(".venv" "pyenv" ".virtual" ".env")))

(use-package editorconfig
  :diminish " 📋"
  :config
  (editorconfig-mode 1))

(use-package evil-numbers
  :bind
  ("C-c +" . 'evil-numbers/inc-at-pt)
  ("C-c -" . 'evil-numbers/dec-at-pt))

(use-package deft
  :bind
  ("C-c d" . 'deft)
  :init (setq deft-directory "~/txt"
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

(use-package web-mode)
(use-package inf-ruby
  :config
  (autoload 'inf-ruby-minor-mode "inf-ruby" "Run an inferior Ruby process" t)
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(use-package string-inflection
  :bind
  ("C-c i i" . string-inflection-all-cycle)
  ("C-c i c" . string-inflection-camelcase))

(use-package copy-as-format
  :bind
  ("C-c M-w s" . copy-as-format-slack)
  ("C-c M-w j" . copy-as-format-jira)
  ("C-c M-w g" . copy-as-format-github))

(use-package epa
  :ensure nil
  :config
  (setq epa-pinentry-mode 'loopback))

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
