;; Bootstrap use-package.
;;

;; https://github.com/nilcons/emacs-use-package-fast/blob/master/README.md#a-trick-less-gc-during-startup
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook
          #'(lambda () ; restore gc after startup
              (setq gc-cons-threshold 800000)))


(setq inhibit-startup-screen t
      package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
                         ("org" . "http://orgmode.org/elpa/")))

(eval-when-compile
  (require 'package)
  (package-initialize)
  (progn
    (unless (package-installed-p 'use-package)
      (message "Whoa! Bootstrapping.")
      (package-refresh-contents)
      (package-install 'use-package))
    (require 'use-package)
    (setq package-enable-at-startup nil
          use-package-always-ensure 't
          use-package-always-pin "melpa")))
(use-package diminish)
(use-package bind-key)

(defun edd/maybe-load-config (name)
  (let ((conf (locate-user-emacs-file name)))
    (when (file-readable-p conf) (load-file conf))))

(use-package exec-path-from-shell
  :init
  (exec-path-from-shell-initialize))

(provide 'edd-bootstrap)
