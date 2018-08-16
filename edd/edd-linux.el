(defun noop ()
  (interactive))

(global-set-key (kbd "<key-4660>") #'noop) ; this is what my Enter key is called in X

;; Tweak theme to fit in
(let
    ((bg  "#222222")
     (fbg "#121212"))
  (dolist (fce '(default fringe))
    (set-face-background fce bg))

  (eval-after-load 'magit
    '(dolist (fce '(magit-diff-added-highlight
                   magit-diff-removed-highlight magit-diff-context-highlight
                   magit-blame-heading magit-blame-date magit-blame-summary))
      (set-face-background fce "#121212")))
  (eval-after-load 'org
    '(progn
       (set-face-background 'org-code "#121212")
       (set-face-background 'org-table "#121212"))
    ;;(set-face-background 'org-block bg)))
    ))


;; let's use firefox
(setq browse-url-browser-function 'browse-url-firefox)

(provide 'edd-linux)
