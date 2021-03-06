(use-package hi2
  :delight
  (hi2-mode))

(use-package haskell-mode
  :delight
  (haskell-doc-mode " 📜")
  :mode "\\.hs\\'"
  :hook
  (haskell-mode . haskell-doc-mode)
  (haskell-mode . turn-on-hi2)
  (haskell-mode . haskell-indentation-mode)
  (haskell-mode . haskell-decl-scan-mode)
  (haskell-mode . edd-haskell/prettify)
  :init
  (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
    (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
    (add-to-list 'exec-path my-cabal-path))
  :config
  (add-to-list 'company-backends 'company-ghc)
  (custom-set-variables '(company-ghc-show-info t))
  (setq
   haskell-tags-on-save t
   haskell-process-suggest-remove-import-lines t
   haskell-process-auto-import-loaded-modules t
   haskell-process-type 'cabal-repl
   haskell-process-log t)
  (when (not (string-match-p (regexp-quote (expand-file-name "~/.local/bin")) (getenv "PATH")))
    (setenv "PATH"
            (concat
             (getenv "PATH") ":" (expand-file-name "~/.local/bin") )))

  (defun edd-haskell/prettify ()
    (setq-local
               prettify-symbols-alist
               '(("&&" . ?∧)
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
                 ("empty" . 8709)
                 ("empty" . ?∅)
                 ("exists" . ?∃)
                 ("false" . 8869)
                 ("forall" . ?∀)
                 ("intersection" . ?∩)
                 ("isProperSubsetOf" . ?⊂)
                 ("isSubsetOf" . ?⊆)
                 ("mappend" . ?⊕)
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
                 ("~>" . ?⇝)
                 ("!" . 172)))
    (prettify-symbols-mode)
    )

  :bind
  (
   ("C-c h h" . switch-to-haskell)
   :map haskell-mode-map
   ("C-c C-l" . haskell-process-load-or-reload)
   ("C-c C-z" . haskell-interactive-switch)
   ("C-c C-n C-t" . haskell-process-do-type)
   ("C-c C-n C-i" . haskell-process-do-info)
   ("C-c C-n C-c" . haskell-process-cabal-build)
   ("C-c C-n c" . haskell-process-cabal)
   ("C-c DEL" . haskell-hoogle)))


(use-package intero
  :commands (intero-mode intero-mode-blacklist)
  :delight " ‽"
  :hook
  (haskell-mode . intero-mode)
  (haskell-mode . intero-mode-blacklist)
  :init
  (setq intero-blacklist (list (expand-file-name "~/.xmonad")))
  :bind
  (:map intero-mode-map
   ("C-c C-j" . intero-repl)))

(use-package flymake-hlint)

(provide 'edd-haskell)
