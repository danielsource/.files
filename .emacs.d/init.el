(when (version<= emacs-version "27.1")
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))
(minimal/font)
