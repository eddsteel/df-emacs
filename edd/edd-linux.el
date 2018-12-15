(use-package emacs
  :config
  (defun noop ()
    (interactive))
  :bind
  ;; this is what my Enter key is called in X
  ("<key-4660>" . #'noop))

(provide 'edd-linux)
