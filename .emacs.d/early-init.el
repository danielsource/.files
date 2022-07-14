;;; Performance tweak
(setq gc-cons-threshold 100000000)      ; 100 MB
(setq read-process-output-max 1000000)  ; 1 MB

(when (featurep 'gtk)
  (add-to-list 'default-frame-alist
               '(background-color . "#1e1e1e"))
  (add-to-list 'default-frame-alist
               '(foreground-color . "#d4d4d4"))
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode 0)
  (fringe-mode '(nil . 0)))
