(defun minimal/sane-config ()
  (menu-bar-mode -1)
  (scroll-bar-mode -1)
  (tool-bar-mode -1)
  (column-number-mode t)
  (setq-default cursor-type 'bar))

(defun minimal/set-font ()
  (set-face-attribute 'default nil
		      :font "Inconsolata"
		      :height 105))

(defun minimal/set-theme ()
  "Set the theme based on the hour of day."
  (if (< 6 (decoded-time-hour (decode-time)) 18)
      (load-theme 'modus-operandi)
    (load-theme 'modus-vivendi)))

(minimal/sane-config)
(minimal/set-font)
(minimal/set-theme)
