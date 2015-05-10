;; Bootstrap packages.
;;
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(package-initialize)

;; Bootstrap use-package.
;;
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package)
  (require 'diminish)
  (require 'bind-key))

;; System-specific stuff.
;;
(use-package edd-mac
  :load-path "edd"
  :if (eq 'darwin system-type))

;; Basic UX (no packages)
;;
(use-package edd-ux
  :load-path "edd")

;; theme
;;
(use-package sublime-themes
  :ensure t
  :config
  (edd-load-theme 'spolsky)
  (set-face-attribute 'hl-line nil :background "#222222" :underline nil))

;; nyan nyan
;;
(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode))

;; built-in features
;;
(use-package edd-features
  :load-path "edd")

;; utilities that are too small to live alone
;;
(use-package edd-util
  :load-path "edd"
  :bind
  (("C-w" . kill-region-or-backward-kill-word)
   ("C-c M-p" . edd-jump-to-prev-url)
   ("C-c M-n" . edd-jump-to-next-url)
   ("C-c x" . edd-term)
   ("C-c o" . edd-initial-file-or-scratch)))

;; whitespace
;;
(use-package whitespace
  :ensure t
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

;; ace-jump
(use-package ace-jump-mode
  :ensure t
  :bind
  (("C-c j" . ace-jump-mode)
   ("C-c k" . ace-jump-mode-pop-mark)))

;; Ace window mode
(use-package ace-window
  :ensure t
  :bind
  ("C-x o" . ace-window)
  :init
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  (setq aw-background nil))

;; guide key
(use-package guide-key
  :ensure t
  :diminish guide-key-mode
  :init
  (setq guide-key/guide-key-sequence
        '("C-x r" "C-x 4" "C-x 8" "C-c C-x" "C-c C-v"))
  :config
  (guide-key-mode 1))

;; hardcore mode
(use-package hardcore-mode
  :ensure t
  :diminish hardcore-mode
  :config
  (setq too-hardcore-return 1)
  (global-hardcore-mode t))

;; helm
(use-package helm
  :ensure t
  :ensure helm-ag
  :diminish helm-mode
  :init
  (defun edd-theme-helm ()
    (let ((string (face-foreground 'font-lock-string-face))
          (builtin (face-foreground 'font-lock-builtin-face))
          (function (face-foreground 'font-lock-function-name-face))
          (variable (face-foreground 'font-lock-variable-name-face))
          (keyword (face-foreground 'font-lock-keyword-face))
          (background (face-background 'default))
          (mode-bg (face-background 'mode-line))
          (imode-fg (face-foreground 'mode-line-inactive)))
      (set-face-attribute 'helm-candidate-number nil :background string :foreground background)
      (set-face-attribute 'helm-grep-file nil :foreground builtin :underline t)
      (set-face-attribute 'helm-grep-finish nil :foreground function)
      (set-face-attribute 'helm-grep-lineno nil :foreground variable)
      (set-face-attribute 'helm-grep-match nil :foreground string)
      (set-face-attribute 'helm-grep-running nil :foreground variable)
      (set-face-attribute 'helm-prefarg nil :foreground function)
      (set-face-attribute 'helm-selection nil :background mode-bg :foreground keyword :underline t)
      (set-face-attribute 'helm-source-header nil :background background :foreground imode-fg :weight 'bold :height 1.3 :family "Sans Serif")
      (set-face-attribute 'helm-visible-mark nil :background imode-fg)))
  (add-hook 'edd-load-theme-hook 'edd-theme-helm)
  :config
  (helm-mode 1)
  (edd-theme-helm)
  :bind
  (("C-h d" . helm-dash)
   ("C-x b" . helm-mini)
   ("M-y" . helm-show-kill-ring)
   ("M-x" . helm-M-x)
   ("C-x C-m" . helm-M-x)
   ("C-x C-f" . helm-find-files)))

;; helm-dash
(use-package helm-dash
  :bind ("C-c h d" . helm-dash)
  :init
  (setq helm-dash-common-docsets '("Akka" "Scala")))

;; projectile
;;
(use-package projectile
  :ensure t
  :ensure helm-projectile
  :config
  (projectile-global-mode)
  (setq projectile-mode-line
        '(:eval (format " ☄{%s}" (projectile-project-name))))
  (setq projectile-completion-system 'helm)
  (def-projectile-commander-method ?s "Run a build shell (e.g. SBT)"
    ;; TODO: support others
    (sbt-start))
  (helm-projectile-on))

;; RE-Builder
;;
(use-package re-builder
  :ensure t
  :init
  (setq reb-re-syntax 'string))

;; iy-go-to-char
(use-package iy-go-to-char
  :ensure t
  :bind
  (("C-c f" . iy-go-to-char)
   ("C-c F" . iy-go-to-char-backward)
   ("C-c ;" . iy-go-to-or-up-to-continue)
   ("C-c t" . iy-go-up-to-char)
   ("C-c T" . iy-go-up-to-char-backward)))

;; vagrant
(use-package vagrant
  :ensure t
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
  :ensure t
  :init (add-hook 'geiser-hook 'pretty-lambda)
  :config (pretty-lambda-for-modes))

;; secret config -- used below.
(use-package edd-secrets
  :load-path "edd"
  :commands edd-with-secrets)

(use-package edd-erc
  :load-path "edd")

(use-package edd-jabber
  :load-path "edd")

(use-package edd-haskell
  :load-path "edd")


(use-package "company"
  :ensure t
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

(use-package edd-mail
  :load-path "edd")

(use-package edd-org
  :load-path "edd")

(use-package edd-pdf
  :load-path "edd")

(use-package helm-dash
  :ensure t
  :init
  (setq helm-dash-common-docsets '("Akka" "Scala" "Java_SE7"))
  :bind (("C-c d d" . helm-dash)
         ("C-c d a" . helm-dash-activate-docset)))

(use-package edd-scala
  :load-path "edd")

(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  :config
  (setq flycheck-scalastyle-jar
        (expand-file-name "scalastyle/scalastyle_2.10-batch.jar"))
  (setq flycheck-scalastylerc
        (expand-file-name "scalastyle/scalastyle-config.xml"))
  (add-hook 'scala-mode-hook
            (lambda () (unless (eq (file-name-extension (buffer-file-name (current-buffer))) "sbt")
                    (flycheck-mode 1)))))

(use-package rainbow-delimiters
  :ensure t
  :init
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package auto-highlight-symbol
  :init
  (add-hook 'prog-mode-hook #'auto-highlight-symbol-mode)
  :diminish auto-highlight-symbol-mode
  :ensure t)

(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :config
  (global-git-gutter-mode 1))

(use-package magit
  :ensure t
  :diminish magit-auto-revert-mode
  :bind ("C-c m" . magit-status))

(let
    ((localel (locate-user-emacs-file "local.el")))
  (when (file-readable-p localel)
    (load-file localel)))

(use-package ledger-mode
  :ensure t
  :mode ("\\.ledger$" "ledger\\.dat$")
  :config
  (setq ledger-post-auto-adjust-amounts t))

(use-package tea-time
  :ensure t)

(use-package bbdb
  :ensure t
  :config (bbdb-initialize)
  :commands bbdb)

(use-package git-timemachine
  :ensure t)

;; acknowledgements
;;
;; http://www.lunaryorn.com/2015/01/06/my-emacs-configuration-with-use-package.html
;; http://sachachua.com/blog/2014/12/emacs-configuration-use-package/
;; http://pages.sachachua.com/.emacs.d/Sacha.html
;; https://github.com/mwfogleman/config/blob/master/home/.emacs.d/michael.org
;; https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md
;; https://github.com/bodil/emacs.d/blob/master
;; https://github.com/shosti/.emacs.d/blob/master/personal/p-jabber.el
;; http://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
