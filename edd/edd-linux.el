;; Nix requires messing with the path too
;;
(if (and (file-exists-p "~/.nix-profile/bin")
         (file-exists-p "/run/current-system/bin"))
    (dolist (dir '("/run/current-system/bin" "~/.nix-profile/bin"))
      (when (file-directory-p dir)
        (setenv "PATH" (concat (expand-file-name dir) ":" (getenv "PATH")))
        (add-to-list 'exec-path (expand-file-name dir)))))

(defun noop ()
  (interactive))

(global-set-key (kbd "<key-4660>") #'noop) ; this is what my Enter key is called in X

;; Tweak theme to fit in
(let
    ((bg  "#222222")
     (fbg "#121212"))
  (dolist (fce '(default fringe))
    (set-face-background fce bg))
  (dolist (fce '(mode-line mode-line-inactive))
    (set-face-background fce fbg))

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

(provide 'edd-linux)
