(when (version<= emacs-version "27.1")
  (load-file (expand-file-name "early-init.el" user-emacs-directory)))

(add-hook 'minimal/after-reload-hook 'minimal/theme)
(minimal/bindings)
(minimal/font)

(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))

(setenv "BASH_ENV" "~/.bash_aliases")

(setq-default ispell-program-name "aspell"
              ispell-dictionary "brasileiro")
