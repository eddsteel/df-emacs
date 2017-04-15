(package-initialize)

(add-to-list 'load-path (locate-user-emacs-file "edd"))
(require 'edd-bootstrap)
(edd/maybe-load-config "local-pre.el")

;; theme
;;
(defvar edd-theme)
(use-package darkokai-theme
  :init
  (setq edd-theme 'darkokai))

;; Do this stuff early to avoid flicker
;;
(use-package edd-ux :ensure nil)

(eval-when-compile
  (require 'use-package)
  (require 'diminish)
  (require 'bind-key))

;; built-in features
;;
(use-package edd-features :ensure nil)

;; System-specific stuff.
;;
(use-package edd-mac
  :ensure nil
  :if (eq 'darwin system-type))


(use-package edd-linux
  :ensure nil
  :if (not (eq 'darwin system-type)))


;; nyan nyan
;;
(use-package nyan-mode
  :config
  (nyan-mode))

;; whitespace
;;
(use-package whitespace
  :diminish whitespace-mode
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook conf-mode-hook org-mode-hook))
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

;; hardcore mode
(use-package hardcore-mode
  :diminish hardcore-mode
  :config
  (setq too-hardcore-return 1)
  (global-hardcore-mode t))

;;(use-package edd-helm :ensure nil)

(use-package edd-ivy :ensure nil)


;; projectile
;;
(use-package projectile
  :init
  (setq projectile-keymap-prefix (kbd "C-c C-p"))

  :config
  (add-hook 'text-mode-hook #'projectile-mode)
  (add-hook 'prog-mode-hook #'projectile-mode)
  (setq projectile-mode-line
        '(:eval (format " ☄{%s}" (projectile-project-name)))))

;; utilities that are too small to live alone
;;
(use-package edd-util
  :ensure nil
  :bind
  (("C-w" . kill-region-or-backward-kill-word)
   ("C-c M-p" . edd-jump-to-prev-url)
   ("C-c M-n" . edd-jump-to-next-url)
   ("C-c x" . edd-term)
   ("C-c o" . edd-initial-file-or-scratch)
   ("C-c u" . edd-hex-encode)))


;; RE-Builder
;;
(use-package re-builder
  :init
  (setq reb-re-syntax 'string))

;; vagrant
(use-package vagrant
  :init
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

;; pretty lambda
(use-package pretty-lambdada
  :init (add-hook 'geiser-hook 'pretty-lambda)
  :config (pretty-lambda-for-modes))

;; secret config -- used below.
(use-package edd-secrets
  :ensure nil
  :commands edd-with-secrets)

(use-package edd-erc :ensure nil)

(use-package edd-jabber
  :ensure nil
  :bind ("C-c j" . edd-hipchat-join))

(use-package edd-haskell :ensure nil)

(use-package "company"
  :ensure company-ghc
  :commands (company-mode)
  :diminish (company-search-mode company-mode)
  :config
  (defun edd-theme-company ()
    (let ((mode-bg (face-background 'mode-line))
          (imode-fg (face-foreground 'mode-line-inactive)))
      (set-face-attribute 'company-preview nil :background mode-bg :foreground "light sky blue")
      (set-face-attribute 'company-preview-search nil :inherit 'company-preview)
      (set-face-attribute 'company-scrollbar-bg nil :inherit 'company-tooltip)
      (set-face-attribute 'company-tooltip nil :background "#363A3F" :foreground "#66D9EF")
      (set-face-attribute 'company-tooltip-annotation nil :inherit 'company-tooltip :foreground "#666")
      (set-face-attribute 'company-tooltip-common nil :inherit 'company-tooltip :foreground imode-fg)
      (set-face-attribute 'company-tooltip-common-selection nil :inherit 'company-tooltip-selection)
      (set-face-attribute 'company-tooltip-selection nil :inherit 'company-tooltip :background "#666666")))
  (add-hook 'edd-load-theme-hook 'edd-theme-company)
  (add-hook 'haskell-mode-hook 'company-mode)
  (edd-theme-company)
(progn
    (add-to-list 'company-backends 'company-ghc)
    (custom-set-variables '(company-ghc-show-info t))))

(use-package edd-org :ensure nil)

(use-package edd-mail
  :ensure nil
  :init
  (setq mail-specify-envelope-from t
        mail-envelope-from 'header
        send-mail-function 'sendmail-send-it
        message-sendmail-envelope-from 'header
        message-send-mail-function 'message-send-mail-with-sendmail)
  :bind ("C-c n" . edd-mailbox))

(use-package pdf-tools)
;; (use-package edd-pdf)

(use-package edd-scala :ensure nil)

(use-package flycheck
  :diminish (flycheck-mode . " 📈")
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
  (setq magit-commit-arguments '("--gpg-sign=33620159D40385A0")))

(use-package magit-filenotify :demand t)

(use-package ledger-mode
  :mode ("\\.ledger$" "ledger\\.dat$")
  :config
  (setq ledger-post-auto-adjust-amounts t))

(use-package tea-time)

(use-package bbdb
  :config (bbdb-initialize)
  :commands bbdb)

(use-package git-timemachine)

(use-package markdown-mode+)

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

;; "Then Emacs will understand path like /vcsh:zsh:."
(add-to-list 'tramp-methods '("vcsh"
                              (tramp-login-program "vcsh")
                              (tramp-login-args
                               (("enter")
                                ("%h")))
                              (tramp-remote-shell "/bin/sh")
                              (tramp-remote-shell-args
                               ("-c"))))

(use-package docker
  :diminish " 🐳"
  :init
  (docker-global-mode))

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

(use-package corral
  :bind
  (("M-9" . corral-parentheses-backward)
   ("M-0" . corral-parentheses-forward)
   ("M-[" . corral-brackets-backward)
   ("M-]" . corral-brackets-forward)
   ("M-{" . corral-braces-backward)
   ("M-}" . corral-braces-forward)
   ("M-\"" . corral-double-quotes-backward)))

(use-package engine-mode
  :init
  (engine-mode 1)
  :config
  (engine/set-keymap-prefix (kbd "C-x g"))
  (defengine google
    "https://google.com/search?q=%s"
    :keybinding "g"
    :browser 'eww-browse-url)
  :commands engine-mode)

(use-package anzu
  :diminish anzu-mode
  :init
  (global-anzu-mode +1)
  :bind
  (("M-%" . anzu-query-replace)
   ("C-M-%" . anzu-query-replace-regexp)))


(use-package emms
  :load-path "../src/emms/lisp"
  :commands emms-smart-browse
  :init
  (require 'emms-setup)
  (emms-all)
  (emms-default-players)
  (setq emms-source-file-default-directory (expand-file-name "~/media/music"))

  (require 'emms-tag-editor)
  (require 'emms-info)

  ;; Use only libtag for tagging.
  (require 'emms-info-libtag)
  (setq emms-info-functions '(emms-info-libtag))
  (setq emms-info-libtag-program-name (expand-file-name "~/bin/emms-print-metadata"))
  (setq emms-volume-change-function 'emms-volume-pulse-change))


(use-package dumb-jump
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g q" . dumb-jump-quick-look))
  :config
  (setq dumb-jump-selector 'ivy))

(use-package helm-make
  :config
  (setq helm-make-completion-method 'ivy)
  (setq helm-make-comint 't))

(use-package edd-rss :ensure nil)

(use-package csv-mode)
(use-package elm-mode)
(use-package gradle-mode)
(use-package groovy-mode)
(use-package php-mode)
(use-package play-routes-mode)
(use-package projectile-ripgrep)
(use-package rjsx-mode)
(use-package yaml-mode)
(use-package idris-mode
  :config
  (defun edd/idris-next-hole ()
      (interactive)
      (search-forward " ?" nil t))
  :bind
  (:map idris-mode-map
        ("C-c C-j" . idris-pop-to-repl)
        ("C-c C-f" . edd/idris-next-hole)))

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
