;; Bootstrap use-package.
;;
(unless (package-installed-p 'use-package)
  (message "whoa! Bootstrapping.")
  (package-refresh-contents)
  (package-install 'use-package))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://melpa-stable.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/") t)
(setq package-enable-at-startup nil)
(setq use-package-always-ensure t)

(defun edd/maybe-load-config (name)
  (let ((conf (locate-user-emacs-file name)))
    (when (file-readable-p conf) (load-file conf))))

(provide 'edd-bootstrap)
