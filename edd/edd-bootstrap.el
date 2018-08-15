;; Bootstrap use-package.
;;

;; https://github.com/nilcons/emacs-use-package-fast/blob/master/README.md#a-trick-less-gc-during-startup
(package-initialize)
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
(use-package delight :pin gnu)
(use-package bind-key)

(defun edd/maybe-load-config (name)
  (let ((conf (locate-user-emacs-file name)))
    (when (file-readable-p conf) (load-file conf))))

(defun edd/is-local-config (name)
  (file-exists-p (locate-user-emacs-file (format "edd/%s.el" name))))

(defun edd/ensure-local-or-elpa (pkg args state)
  (or
   (when pkg (edd/is-local-config pkg))
   (use-package-ensure-elpa pkg args state)))

(setq use-package-ensure-function #'edd/ensure-local-or-elpa)

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(provide 'edd-bootstrap)
