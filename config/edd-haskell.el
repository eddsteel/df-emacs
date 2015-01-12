(add-hook 'haskell-mode-hook 'turn-on-haskell-decl-scan)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc)
(add-hook 'haskell-mode-hook 'turn-on-hi2)

;; much of this copied from https://github.com/serras/emacs-haskell-tutorial/blob/master/tutorial.md
;;

(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
  (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
  (add-to-list 'exec-path my-cabal-path))

(custom-set-variables
  '(haskell-tags-on-save t)
  '(haskell-process-suggest-remove-import-lines t)
  '(haskell-process-auto-import-loaded-modules t)
  '(haskell-process-type 'cabal-repl)
  '(haskell-process-log t))

(eval-after-load 'haskell-mode '(progn
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "SPC") 'haskell-mode-contextual-space)))

(eval-after-load 'haskell-cabal '(progn
  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))

(autoload 'ghc-init "ghc" nil t)
(autoload 'ghc-debug "ghc" nil t)
(add-hook 'haskell-mode-hook 'ghc-init)

(require 'company)
(add-hook 'haskell-mode-hook 'company-mode)
(add-to-list 'company-backends 'company-ghc)
(custom-set-variables '(company-ghc-show-info t))

(defun prettify-haskell ()
  (push '("<-" . ?←) prettify-symbols-alist)
  (push '("->" . ?→) prettify-symbols-alist)
  (push '("\\" . ?λ) prettify-symbols-alist)
  (push '(".." . ?…) prettify-symbols-alist)
  (push '("++" . ?⧺) prettify-symbols-alist)
  (push '(">>" . ?») prettify-symbols-alist)
  (push '("<<" . ?«) prettify-symbols-alist))

(add-hook 'haskell-mode-hook 'prettify-haskell)
(add-hook 'haskell-mode-hook (lambda () (prettify-symbols-mode 1)))
(add-hook 'literate-haskell-mode-hook 'prettify-haskell)
(add-hook 'literate-haskell-mode-hook (lambda () (prettify-symbols-mode 1)))

(provide 'edd-haskell)
