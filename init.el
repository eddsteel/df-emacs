(setq dotfile-dir (file-name-directory
		   (or (buffer-file-name) load-file-name)))
(defun emacsd (f)
  (concat dotfile-dir f))

     
; cheers Bodil
(let ((paths (list "/usr/bin" "/bin" "/sbin" "/usr/local/bin"
	       (concat (getenv "HOME") "/bin"))))
  (setenv "PATH" (apply 'concat
			(mapcar (lambda (i) (concat i ":")) paths)))
  (dolist (path paths) (when (file-directory-p path)
			 (add-to-list 'exec-path path))))

(dolist (dir '("oss" "config"))
  (add-to-list 'load-path (emacsd dir)))

(dolist
     (project (directory-files (concat dotfile-dir "site-lisp") t "\\w+"))
   (when (file-directory-p project)
         (add-to-list 'load-path project)))


(let ((edd-config
       '(edd-bindings
         edd-packages
         edd-editor
         edd-secrets
         edd-go
         edd-pdf
         edd-org ; autoload?
         edd-erc ; autoload?
         edd-scala ; autoload?
         edd-mail))) ; autoload?
  (dolist (file edd-config)
    (require file)))

; edd-git
;(maybe-install-and-require 'egg)

;edd-lisp
(maybe-install-and-require 'pretty-lambdada)
(pretty-lambda-for-modes)
(add-hook 'geiser-hook 'pretty-lambda)

;; edd-ess
(add-to-list 'load-path (emacsd "site-lisp/ESS/lisp"))
(load "ess-site")

; TODO load by hostname
(let ((local-el (emacsd "local.el")))
  (if (file-exists-p local-el)
      (load-file local-el)))



(put 'set-goal-column 'disabled nil)
(put 'narrow-to-region 'disabled nil)
