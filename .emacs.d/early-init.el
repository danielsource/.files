;;; Performance tweak
(setq gc-cons-threshold 100000000)      ; 100 MB
(setq read-process-output-max 1000000)  ; 1 MB

(defvar minimal/lib t)
(load (expand-file-name "minimal.el" user-emacs-directory) nil t t)
(minimal/sane-config)
(minimal/theme)
