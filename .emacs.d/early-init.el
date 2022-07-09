;;; Performance tweak
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ; 1mb

(add-to-list 'default-frame-alist
             '(background-color . "#282a36"))

(when (featurep 'gtk)
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode 0)
  (fringe-mode '(4 . 0)))
