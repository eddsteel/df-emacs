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


;; Use homebrew's scalastyle
;;
(eval-after-load "flycheck"
  (setq flycheck-scalastyle-jar "/usr/local/Cellar/scalastyle/0.8.0/libexec/scalastyle_2.11-0.8.0-batch.jar"))



;; unbind some annoying defaults
;;
(dolist (troublesome '("<f11>" "s-h" "s-z" "C-z" "C-x C-z"))
  (global-unset-key (kbd troublesome)))

(global-set-key (kbd "<f10>") 'toggle-frame-fullscreen)

(use-package reveal-in-finder :ensure t)

(setenv "DOCKER_TLS_VERIFY" "1")
(setenv "DOCKER_HOST" "tcp://192.168.99.101:2376")
(setenv "DOCKER_CERT_PATH" "~/.docker/machine/machines/dev")
(setenv "DOCKER_MACHINE_NAME" "dev")

(provide 'edd-mac)
