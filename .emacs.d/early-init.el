;;; Performance tweak
(setq gc-cons-threshold 100000000)      ; 100 MB
(setq read-process-output-max 1000000)  ; 1 MB

(defvar minimal/lib t)
(load-file (expand-file-name "minimal.el" user-emacs-directory))
(minimal/sane-config)
(minimal/bindings)
(minimal/theme)
