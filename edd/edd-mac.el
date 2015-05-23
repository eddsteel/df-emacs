;; certain builds do otherwise
;;
(setq mac-option-modifier 'meta
      mac-command-modifier 'super)

;; Mac OS requires messing with the path
;;
(dolist (dir '("/usr/local/bin" "~/bin" "/usr/local/MacGPG2/bin" "~/.nix-profile/bin"))
  (when (file-directory-p dir)
    (setenv "PATH" (concat (expand-file-name dir) ":" (getenv "PATH")))
    (add-to-list 'exec-path (expand-file-name dir))))


;; Use the mac gpg2 equiv
;;
(when (executable-find "gpg2")
  (setq epg-gpg-program "gpg2"))


;; unbind some annoying defaults
;;
(dolist (troublesome '("<f11>" "s-h" "s-z" "C-z" "C-x C-z"))
  (global-unset-key (kbd troublesome)))

(global-set-key (kbd "<f10>") 'toggle-frame-fullscreen)

(use-package reveal-in-finder :ensure t)


(provide 'edd-mac)
