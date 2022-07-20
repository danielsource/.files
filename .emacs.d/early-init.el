;;; Performance tweak
(setq gc-cons-threshold 100000000)      ; 100 MB
(setq read-process-output-max 1000000)  ; 1 MB

;;; Appearance and Layout
(menu-bar-mode -1)
(when (or
       (eq system-type 'windows-nt)
       (featurep 'gtk)
       (featurep 'x))
  (add-to-list 'default-frame-alist
               '(background-color . "#1e1e1e"))
  (add-to-list 'default-frame-alist
               '(foreground-color . "#d4d4d4"))
  (tool-bar-mode -1)
  (scroll-bar-mode 0)
  (fringe-mode '(nil . 0)))
