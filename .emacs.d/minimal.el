(defun minimal/get-above-makefile (&optional getdir)
  (let ((dir (locate-dominating-file "." "Makefile")))
    (if dir
        (concat dir (unless getdir "Makefile"))
      (error "Makefile not found"))))

(defun minimal/make (arguments)
  (compile (concat
            (format "make -C '%s' " (minimal/get-above-makefile t)) arguments)
           t))

(defun minimal/sane-config ()
  (interactive)
  (column-number-mode t)
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (fset 'yes-or-no-p 'y-or-n-p)
  (add-hook 'before-save-hook
            'delete-trailing-whitespace)
  (setq-default tab-width 2
                tab-always-indent 'complete
                indent-tabs-mode nil
                c-basic-offset 2
                c-tab-always-indent 'complete
                c-default-style "k&r"
                cursor-type 'bar
                disabled-command-function nil
                scroll-step 1
                mouse-wheel-progressive-speed nil
                mouse-wheel-scroll-amount '(1 ((shift) . 1))))

(defun minimal/font ()
  (interactive)
  (if (eq system-type 'windows-nt)
      (set-frame-font "Consolas 9" nil t)
    (when (member "Inconsolata" (font-family-list))
      (set-face-attribute 'default nil :font "Inconsolata" :height 105)))
  (set-face-font 'fixed-pitch-serif "Courier New Bold"))

(defun minimal/theme ()
  "Set the theme based on the hour of day."
  (interactive)
  (if (< 6 (decoded-time-hour (decode-time)) 18)
      (load-theme 'modus-operandi)
    (load-theme 'modus-vivendi)))

(defun minimal/corresponding-file ()
  (interactive)
  (let ((filename nil)
        (basename (file-name-sans-extension buffer-file-name)))
    (if (setq filename
              (cond ((string-match "\\.c" buffer-file-name)
                     (concat basename ".h"))
                    ((string-match "\\.h" buffer-file-name)
                     (concat basename ".c"))))
        (find-file filename)
      (error "Corresponding file not found"))))

(defun minimal/corresponding-file-other-window ()
  (interactive)
  (find-file-other-window buffer-file-name)
  (minimal/corresponding-file)
  (other-window -1))

(defun minimal/make-run ()
  (interactive)
  (minimal/make "run"))

(defun minimal/make-clean-all ()
  (interactive)
  (minimal/make "clean all"))

(defun minimal/format (&optional beg end)
  "If mark is active then sort lines, otherwise indent the buffer."
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if (and transient-mark-mode mark-active)
      (sort-lines current-prefix-arg beg end)
    (save-excursion
      (indent-region (point-min) (point-max) nil))))

(defun minimal/reload ()
  (interactive)
  (load-file (expand-file-name
              (if (boundp 'minimal/lib)
                  "init.el"
                "minimal.el")
              user-emacs-directory)))

(unless (boundp 'minimal/lib)
  (minimal/sane-config)
  (minimal/font)
  (minimal/theme)
  (ffap-bindings)
  (windmove-default-keybindings)
  (global-set-key (kbd "<f1>") 'save-buffer)
  (global-set-key (kbd "<f5>") 'minimal/make-run)
  (global-set-key (kbd "<f6>") 'minimal/make-clean-all)
  (global-set-key (kbd "<f7>") 'minimal/format)
  (global-set-key (kbd "<f8>") 'minimal/corresponding-file)
  (global-set-key (kbd "<f9>") 'minimal/corresponding-file-other-window)
  (global-set-key (kbd "<f12>") 'minimal/reload)
  (global-set-key (kbd "M--") 'kill-buffer-and-window)
  (global-set-key (kbd "M-0") 'delete-window)
  (global-set-key (kbd "M-1") 'delete-other-windows)
  (global-set-key (kbd "M-2") 'split-window-below)
  (global-set-key (kbd "M-3") 'split-window-right)
  (global-set-key (kbd "M-4") (lookup-key global-map (kbd "C-x 4")))
  (global-set-key (kbd "M-5") (lookup-key global-map (kbd "C-x 5")))
  (global-set-key (kbd "M-6") (lookup-key global-map (kbd "C-x 6")))
  (global-set-key (kbd "M-7") (lookup-key global-map (kbd "C-x 7")))
  (global-set-key (kbd "M-8") (lookup-key global-map (kbd "C-x 8")))
  (global-set-key (kbd "M-]") 'kill-this-buffer)
  (global-set-key (kbd "M-o") 'other-window)
  (global-set-key (kbd "M-t") (lookup-key global-map (kbd "C-x t")))
  (when (display-graphic-p)
    (global-set-key (kbd "M-[") 'dabbrev-expand)))
