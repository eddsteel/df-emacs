(defmacro edd-with-secrets (stub body)
  `(let*
      ((file (expand-file-name (concat "secrets/" ,stub ".el.gpg")
                               user-emacs-directory))
       (exists (file-readable-p file)))

       (if exists
         (progn (load-file file)
                ,body)
         (message (format "Couldn't load %s in secrets dir" ,stub)))))

(provide 'edd-secrets)
