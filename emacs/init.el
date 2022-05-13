;;(toggle-debug-on-error)
(add-to-list 'load-path (locate-user-emacs-file "edd"))
(add-to-list 'exec-path (expand-file-name "~/.nix-profile/bin"))
(require 'edd-bootstrap)
(edd/maybe-load-config "local-pre.el")
(defvar edd/emms-consul-p t "Whether to do consul stuff with emms")

;; Do this stuff early to avoid flicker
;;
(use-package edd-ux :straight nil :if window-system :unless noninteractive)

;; built-in features
;;
(use-package edd-features :straight nil)

;; org-mode, as good as built-in
;;
(use-package edd-org :straight nil)
(use-package edd-gtd
  :straight nil
  :commands (edd/go-home)
  :bind
  (("C-c w" . edd/go-to-work)))

;; System-specific stuff.
;;
(use-package edd-mac :if (eq 'darwin system-type) :straight nil :unless noninteractive)

;; whitespace
;;
(use-package whitespace
  :unless noninteractive
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

(use-package edd-hydra :demand t :straight nil)

(use-package edd-proj :straight nil)

;;--
(use-package corfu
  :custom
  (corfu-cycle t) ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)  ;; Enable auto completion
  (corfu-quit-at-boundary t) ;; Automatically quit at word boundary
  (corfu-quit-no-match t) ;; Automatically quit if there is no match
  :init
  (corfu-global-mode))

;; A few more useful configurations...
(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (setq tab-always-indent 'complete))

(use-package vertico
  :init
  (vertico-mode)
  :config
  (setq vertico-cycle t))

;; TODO do in :straight
(add-to-list 'load-path
             (locate-user-emacs-file "straight/repos/vertico/extensions"))
(use-package vertico-directory
  :demand t
  :ensure nil
  :straight nil
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              ("C-l" . vertico-directory-up))
  ;; Tidy shadowed file names
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

(use-package orderless
  :init
  (setq completion-styles '(substring orderless)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  ;; Replace bindings. Lazily loaded due by `use-package'.
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
;;         ("C-c m" . consult-mode-command)         
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x M-:" . consult-complex-command)     ;; orig. repeat-complex-command
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
         ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
         ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame
         ;; Custom M-# bindings for fast register access
         ("M-#" . consult-register-load)
         ("M-'" . consult-register-store)          ;; orig. abbrev-prefix-mark (unrelated)
         ("C-M-#" . consult-register)
         ;; Other custom bindings
         ("C-M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
         ;; M-g bindings (goto-map)
         ("M-g e" . consult-compile-error)
         ("M-g f" . consult-flymake)               ;; Alternative: consult-flycheck
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g M-g" . consult-goto-line)           ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g m" . consult-mark)
         ("M-g k" . consult-global-mark)
         ("M-i" . consult-imenu)                   ;; orig: imenu
         ("C-c ," . consult-imenu-multi)
         ;; M-s bindings (search-map)
         ("M-s f" . consult-find)
         ("M-s r" . consult-ripgrep)
         ("M-s l" . consult-line)
         ("C-M-." . consult-line) ;; replace grep-or-swiper
         ("M-s L" . consult-line-multi)
         ("M-s m" . consult-multi-occur)
         ("M-s k" . consult-keep-lines)
         ("M-s u" . consult-focus-lines)
         ;; Isearch integration
         ("M-s e" . consult-isearch-history)
         :map isearch-mode-map
         ("M-e" . consult-isearch-history)         ;; orig. isearch-edit-string
         ("M-s e" . consult-isearch-history)       ;; orig. isearch-edit-string
         ("M-s l" . consult-line)                  ;; needed by consult-line to detect isearch
         ("M-s L" . consult-line-multi))           ;; needed by consult-line to detect isearch

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI. You may want to also
  ;; enable `consult-preview-at-point-mode` in Embark Collect buffers.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  ;; The :init configuration is always executed (Not lazy)
  :init

  ;; Optionally configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0
        register-preview-function #'consult-register-format)

  ;; Optionally tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Optionally replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

  (defhydra+ hydra-project nil "Project"
    ("a" consult-ripgrep "rg")
    ("b" consult-buffer "buffer"))


  ;; Configure other variables and modes in the :config section,
  ;; after lazily loading the package.
  :config

  ;; Optionally configure preview. The default value
  ;; is 'any, such that any key triggers the preview.
  ;; (setq consult-preview-key 'any)
  ;; (setq consult-preview-key (kbd "M-."))
  ;; (setq consult-preview-key (list (kbd "<S-down>") (kbd "<S-up>")))
  ;; For some commands and buffer sources it is useful to configure the
  ;; :preview-key on a per-command basis using the `consult-customize' macro.
  (consult-customize
   consult-theme
   :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-recent-file consult--source-project-recent-file consult--source-bookmark
   :preview-key (kbd "M-."))

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<") ;; (kbd "C-+")

  ;; Optionally make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)

  ;; Optionally configure a function which returns the project root directory.
  ;; There are multiple reasonable alternatives to chose from.
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-root-function #'projectile-project-root)
  )

(use-package embark
  :bind
  (("C-." . embark-act)
   ("C-c C-o" . embark-export)
   ("M-." . embark-dwim)
   ("<help> B" . embark-bindings))

  :init

  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config

  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))
(use-package embark-consult
  :after (embark consult)
  :demand t
  :hook (embark-collect-mode . consult-preview-at-point-mode))
(use-package marginalia
  :init
  (marginalia-mode))
;;--

;; utilities that are too small to live alone
;;
(use-package edd-util
  :demand t
  :straight nil
  :bind
  (("C-w" . kill-region-or-backward-kill-word)
   ("C-c M-p" . edd-jump-to-prev-url)
   ("C-c M-n" . edd-jump-to-next-url)
   ("C-x k". edd-kill-a-buffer)))

;; secret config -- used below.
(use-package edd-secrets :commands edd-with-secrets :straight nil)
(use-package pdf-tools
  :init
  (pdf-tools-install))

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
  (setq magit-completing-read-function 'completing-read-default)
  (setq magit-commit-arguments '("--gpg-sign")))

(use-package magit-filenotify :demand t)

(use-package magit-delta
  :hook
  ((magit-mode-hook) . (lambda () (magit-delta-mode +1))))

(use-package edd-ledger :straight nil)

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

(use-package edd-scala :straight nil)
(use-package edd-haskell :straight nil)
(use-package edd-ruby :straight nil)
(use-package edd-rust :straight nil)
(use-package edd-go :straight nil)
(use-package edd-kotlin :straight nil)

(use-package lua-mode)
(use-package cc-mode
  :hook
  (java-mode-hook . (lambda () (c-set-offset 'statement-cont '++))))
(use-package sml-mode)
(use-package csv)
(use-package groovy-mode)
(use-package php-mode)
(use-package python-mode)
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

(use-package volatile-highlights
  :delight
  :config
  (volatile-highlights-mode t))

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

(use-package edd-emms :straight nil)

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

(use-package gradle-mode
  :straight `(gradle-mode :type git :host github :repo "jacobono/emacs-gradle-mode"
                          :fork (:host github :repo "eddsteel/emacs-gradle-mode")))
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

;; TODO straight this
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

(use-package direnv
  :config
  (direnv-mode))

(use-package browse-at-remote)

(use-package evil-numbers
  :bind
  ("C-c +" . 'evil-numbers/inc-at-pt)
  ("C-c -" . 'evil-numbers/dec-at-pt))

(use-package server
  :init
  (setq server-socket-dir (expand-file-name "~/run/emacs")))

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

(use-package nix-mode
  :config
  (setenv "PATH" (concat (getenv "PATH") ":" "/nix/var/nix/profiles/default/bin")))
(use-package olivetti)

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
