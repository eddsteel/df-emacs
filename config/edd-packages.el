(require 'package)
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(setq package-archives
      '(("gnu" . "http://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("melpa" . "http://melpa.org/packages/")))


;; from https://github.com/otfrom/otfrom-org-emacs/blob/master/init.el#L22
(defun require (p)
  (when (not (package-installed-p p))
    (package-install p))
  (require p))


(provide 'edd-packages)
