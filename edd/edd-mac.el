;; certain builds do otherwise
;;
(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

;; Need these on mac
;;
(dolist (dir '("/usr/local/bin" "~/bin" "/usr/local/MacGPG2/bin"))
  (setenv "PATH" (concat dir ":" (getenv "PATH")))
  (add-to-list 'exec-path dir))

;; unbind some annoying defaults
;;
(dolist (troublesome '("<f11>" "s-h" "s-z" "C-z" "C-x C-z"))
  (global-unset-key (kbd troublesome)))

(global-set-key (kbd "<f10>") 'toggle-frame-fullscreen)


(provide 'edd-mac)
