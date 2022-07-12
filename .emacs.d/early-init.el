;;; Performance tweak
(setq gc-cons-threshold 100000000)      ; 100 MB
(setq read-process-output-max 1000000)  ; 1 MB

(when (featurep 'gtk)
  (add-to-list 'default-frame-alist
               '(background-color . "#282a36"))
  (add-to-list 'default-frame-alist
               '(foreground-color . "#ffffff"))
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode 0)
  (fringe-mode '(nil . 0)))
