;;; Performance tweak
(setq gc-cons-threshold 100000000)      ; 100 MB
(setq read-process-output-max 1000000)  ; 1 MB

(defvar minimal/lib t)
(load-file (expand-file-name "minimal.el" user-emacs-directory))

;;; Appearance and Layout
(minimal/sane-config)
(when (or
       (eq system-type 'windows-nt)
       (featurep 'gtk)
       (featurep 'x))
  (add-to-list 'default-frame-alist
               '(background-color . "#1e1e1e"))
  (add-to-list 'default-frame-alist
               '(foreground-color . "#d4d4d4"))
  (fringe-mode '(nil . 0)))
