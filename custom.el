(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(company-preview ((t (:background "#333" :foreground "light sky blue"))))
 '(company-preview-search ((t (:inherit company-preview))))
 '(company-scrollbar-bg ((t (:inherit company-tooltip))))
 '(company-scrollbar-fg ((t nil)))
 '(company-tooltip ((t (:background "#363A3F" :foreground "#66D9EF"))))
 '(company-tooltip-annotation ((t (:inherit company-tooltip :foreground "#666"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :foreground "#555"))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection))))
 '(company-tooltip-selection ((t (:inherit company-tooltip :background "#666666"))))
 '(flymake-errline ((t (:background "firebrick3" :foreground "gray100"))))
 '(helm-M-x-key ((t (:foreground "#fd971f" :underline t))))
 '(helm-candidate-number ((t (:background "#EEDC82" :foreground "#161A1F"))))
 '(helm-grep-file ((t (:foreground "#ff80f4" :underline t))))
 '(helm-grep-finish ((t (:foreground "#a6e22e"))))
 '(helm-grep-lineno ((t (:foreground "#fd971f"))))
 '(helm-grep-match ((t (:foreground "#eedc82"))))
 '(helm-grep-running ((t (:foreground "#FC580C"))))
 '(helm-prefarg ((t (:foreground "#A6E22E"))))
 '(helm-selection ((t (:background "#333" :foreground "#F92672" :underline t))))
 '(helm-source-header ((t (:background "#161a1f" :foreground "#555" :weight bold :height 1.3 :family "Sans Serif"))))
 '(helm-visible-mark ((t (:background "#555"))))
 '(hl-line ((t (:background "#222222" :underline nil))))
 '(nm-authors-face ((t (:inherit font-lock-function-name-face :slant italic))))
 '(nm-header-face ((t (:inherit font-lock-function-name-face :underline t :weight bold))))
 '(nm-read-face ((t (:inherit font-lock-function-name-face))))
 '(nm-unread-face ((t (:inherit font-lock-keyword-face :weight bold))))
 '(whitespace-line ((t (:background "disabledControlTextColor" :foreground "controlBackgroundColor")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-ghc-show-info t)
 '(custom-safe-themes
   (quote
    ("3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "bf648fd77561aae6722f3d53965a9eb29b08658ed045207fe32ffed90433eb52" "33c5a452a4095f7e4f6746b66f322ef6da0e770b76c0ed98a438e76c497040bb" "91b5a381aa9b691429597c97ac56a300db02ca6c7285f24f6fe4ec1aa44a98c3" "a774c5551bc56d7a9c362dca4d73a374582caedb110c201a09b410c0ebbb5e70" "29a4267a4ae1e8b06934fec2ee49472daebd45e1ee6a10d8ff747853f9a3e622" "dc46381844ec8fcf9607a319aa6b442244d8c7a734a2625dac6a1f63e34bc4a6" "e26780280b5248eb9b2d02a237d9941956fc94972443b0f7aeec12b5c15db9f3" default)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox")
     (:name "unread" :query "tag:unread")
     (:name "gmail-in" :query "folder:gmail/INBOX and tag:inbox")
     (:name "hootsuite-in" :query "folder:hootsuite/INBOX and tag:inbox"))))
 '(org-export-backends (quote (ascii html icalendar latex md confluence)))
 '(send-mail-function (quote sendmail-send-it))
 '(vc-display-status nil))
