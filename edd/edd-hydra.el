(use-package hydra
  :init

  ;; Thanks http://kitchingroup.cheme.cmu.edu/blog/2015/09/28/A-cursor-goto-hydra-for-emacs/
  (defhydra goto (:color blue :hint nil)
    "
Goto:
^Char^              ^Word^                ^org^                    ^search^
^^^^^^^^---------------------------------------------------------------------------
_c_: 2 chars        _w_: word by char     _h_: headline in buffer  _o_: helm-occur
_C_: char           _W_: some word        _a_: heading in agenda   _p_: helm-swiper
_L_: char in line   _s_: subword by char  _q_: swoop org buffers   _f_: search forward
^  ^                _S_: some subword     ^ ^                      _b_: search backward
-----------------------------------------------------------------------------------
_B_: helm-buffers       _l_: avy-goto-line
_m_: helm-mini          _i_: ace-window
_R_: helm-recentf

_n_: Navigate           _._: mark position _/_: jump to mark
"
    ("c" avy-goto-char-2)
    ("C" avy-goto-char)
    ("L" avy-goto-char-in-line)
    ("w" avy-goto-word-1)
    ;; jump to beginning of some word
    ("W" avy-goto-word-0)
    ;; jump to subword starting with a char
    ("s" avy-goto-subword-1)
    ;; jump to some subword
    ("S" avy-goto-subword-0)

    ("l" avy-goto-line)
    ("i" ace-window)

    ("h" helm-org-headlines)
    ("a" helm-org-agenda-files-headings)
    ("q" helm-multi-swoop-org)

    ("o" helm-occur)
    ("p" swiper-helm)

    ("f" isearch-forward)
    ("b" isearch-backward)

    ("." org-mark-ring-push :color red)
    ("/" org-mark-ring-goto :color blue)
    ("B" helm-buffers-list)
    ("m" helm-mini)
    ("R" helm-recentf)
    ("n" hydra-navigate/body))

  (global-set-key (kbd "C-c .") 'goto/body))

(provide 'edd-hydra)
