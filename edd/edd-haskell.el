(use-package haskell-mode
  :mode "\\.hs\\'"
  :init
  (dolist (hook '(haskell-doc-mode
                  haskell-indentation-mode
                  haskell-decl-scan-mode))
    (add-hook 'haskell-mode-hook hook))
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))
  (setq
;;   company-ghc-show-info t
   haskell-tags-on-save t
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t
   haskell-process-type 'cabal-repl
   haskell-process-log t)
  :config
  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)
  (define-key haskell-mode-map (kbd "C-c DEL") 'haskell-hoogle)
  (eval-after-load "haskell-doc" '(diminish 'haskell-doc-mode " 📜"))
  (eval-after-load "hi2" '(diminish 'hi2-mode))
  (when (not (string-match-p (regexp-quote (expand-file-name "~/.local/bin")) (getenv "PATH")))
    (setenv "PATH"
            (concat
             (getenv "PATH") ":" (expand-file-name "~/.local/bin") )))

  (add-hook 'haskell-mode-hook (lambda ()
                                 (setq-local prettify-symbols-alist

  '(;; Double-struck letters
    ("&&" . ?∧)
    ("++" . ?⧺)
    ("+++" . ?⧻)
    ("--" . 9548)
    ("-->" . ?⟶)
    ("-/->" . ?↛)
    ("->" . ?→)
    ("->>" . ?↠)
    ("-o->" . ?⇴)
    ("-|->" . ?⇸)
    ("-|>" . ?⇾)
    ("-||->" . ?⇻)
    ("..." . ?…)
    ("/<" . ?≮)
    ("/=" . ?≠)
    ("/>" . ?≯)
    ("::" . ?∷)
    (":=" . ?≔)
    ("<*>" . ?⊛)
    ("<+>" . 8853)
    ("<-" . ?∈)

    ("<--" . ?⟵)

    ("<-->" . ?⟷)
    ("<--|" . ?⟻)
    ("<-/-" . ?↚)
    ("<-<" . ?↢)

    ("<->" . ?↔)
    ("<-o-" . ?⬰)
    ("<-|" . ?↤)
    ("<-|-" . ?⇷)
    ("<-|->" . ?⇹)
    ("<-||-" . ?⇺)
    ("<-||->" . ?⇼)
    ("<<" . ?≪)
    ("<<-" . ?↞)
    ("<<-<" . ?⬻)
    ("<<<" . ?⋘)

    ("<=" . ?≤)

    ("<==" . ?⟸)

    ("<==>" . ?⟺)
    ("<==|" . ?⟽)

    ("<=>" . ?⇔)
    ("<=|" . ?⤆)
    ("<|" . ?⊲)
    ("<|-" . ?⇽)
    ("<|-|>" . ?⇿)
    ("<~" . ?⇜)

    ("=:" . ?≕)

    ("==" . ?≡)

    ("==>" . ?⟹)

    ("=>" . ?⇒)
    ("=?" . ?≟)
    ("=def" . ?≝)
    (">->" . ?↣)
    (">->>" . ?⤖)
    ("><" . ?⋈)
    (">=" . 8805)
    (">=" . ?≥)

    (">>" . ?≫)
    (">>=" . 10524)
    (">>>" . ?⋙)
    ("Bool" . 120121)
    ("Int" . 8484)
    ("Integer" . 8484)
    ("\\" . ?λ)
    ("assert" . 8870)
    ("diff" . 8783)
    ("elem" . ?∈)
    ("empty" . 8709)
    ("empty" . ?∅)
    ("exists" . ?∃)
    ("false" . 8869)
    ("forall" . ?∀)
    ("intersection" . ?∩)
    ("isProperSubsetOf" . ?⊂)
    ("isSubsetOf" . ?⊆)
    ("mappend" . ?⊕)
    ("member" . ?∈)
    ("mempty" . ?∅)
    ("not" . ?¬)
    ("notElem" . ?∉)
    ("notMember" . ?∉)
    ("product" . 8719)
    ("subsetOf" . 8838)
    ("sum" . 8721)
    ("true" . 8868)
    ("undefined" . ?⊥)
    ("union" . ?∪)
    ("|-->" . ?⟼)
    ("|->" . ?↦)
    ("|==>" . ?⟾)
    ("|=>" . ?⤇)
    ("|>" . ?⊳)
    ("|A|" . ?𝔸)
    ("|B|" . ?𝔹)
    ("|C|" . ?ℂ)
    ("|D|" . ?𝔻)
    ("|E|" . ?𝔼)
    ("|F|" . ?𝔽)
    ("|Gamma|" . ?ℾ)
    ("|G|" . ?𝔾)
    ("|H|" . ?ℍ)
    ("|I|" . ?𝕀)
    ("|J|" . ?𝕁)
    ("|K|" . ?𝕂)
    ("|L|" . ?𝕃)
    ("|M|" . ?𝕄)
    ("|N|" . ?ℕ)
    ("|O|" . ?𝕆)
    ("|Pi|" . ?ℿ)
    ("|P|" . ?ℙ)
    ("|Q|" . ?ℚ)
    ("|R|" . ?ℝ)
    ("|S|" . ?𝕊)
    ("|T|" . ?𝕋)
    ("|U|" . ?𝕌)
    ("|V|" . ?𝕍)
    ("|W|" . ?𝕎)
    ("|X|" . ?𝕏)
    ("|Y|" . ?𝕐)
    ("|Z|" . ?ℤ)
    ("|gamma|" . ?ℽ)
    ("|pi|" . ?ℼ)
    ("||" . 8744)
    ("||" . ?∨)
    ("|||" . ?⫴)
    ("~>" . 8669)
    ("~>" . ?⇝)
    ("!" . 172)

                                               ))
                               (prettify-symbols-mode)))

  :bind
  ("C-c h h" . switch-to-haskell))

(use-package hi2
  :commands (turn-on-hi2)
  :init
    (add-hook 'haskell-mode-hook 'turn-on-hi2))

(use-package intero
  :commands (intero-mode intero-mode-blacklist)
  :diminish " ⁉"
  :init
  (setq intero-blacklist (list (expand-file-name "~/.xmonad")))
  (add-hook 'haskell-mode-hook 'intero-mode)
  (add-hook 'haskell-mode-hook 'intero-mode-blacklist)
  :bind (:map intero-mode-map
              ("C-c C-j" . intero-repl)))

(use-package flymake-hlint)

(provide 'edd-haskell)
