(use-package emacs
  :config
  (defun noop ()
    (interactive))
    (defun edd-wibble-font ()
      "Switch GUI font between different sizes (switching between laptop and monitor)"
    (interactive)
    (if (eq (face-attribute 'default :height) 132)
        (set-face-attribute 'default (selected-frame) :height 90)
      (set-face-attribute 'default (selected-frame) :height 132)))
  :bind
  ;; this is what my Enter key is called in X
  ("<key-4660>" . #'noop)




  )

(provide 'edd-linux)
