;;; Setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(unless package--initialized (package-initialize))

;;; Setup use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(setq use-package-always-ensure t)

;; Offload the custom-set-variables to a separate file
;; This keeps your init.el neater and you have the option
;; to gitignore your custom.el if you see fit.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(unless (file-exists-p custom-file)
  (write-region "" nil custom-file))
(load custom-file nil t) ; Load custom file. Don't hide errors. Hide success message

;;; Appearance
(set-frame-font "Noto Sans Mono 9" nil t)
(global-hl-line-mode 1)
(setq-default cursor-type 'bar ; Line-style cursor similar to other text editors
              frame-title-format '("%b")) ; Make window title the buffer name
;; (use-package gruvbox-theme
;;   :config
;;   (if (not custom-enabled-themes)
;;       (load-theme 'gruvbox-dark-hard t)))
(use-package dracula-theme
  :config
  (if (not custom-enabled-themes)
      (load-theme 'dracula t)))

;;; Layout
(setq column-number-mode t
      ring-bell-function 'ignore        ; Disable bell sound
      linum-format "%3d ")    ; Line number format
(tab-bar-mode 1)
(fset 'yes-or-no-p 'y-or-n-p) ; y-or-n-p makes answering questions faster
(show-paren-mode 1)           ; Show closing parens by default
;; (setq inhibit-startup-screen t)           ; Disable startup screen
;; (setq initial-scratch-message "")         ; Make *scratch* buffer blank

;;; External
(setenv "BASH_ENV" "~/.bash_aliases")   ; Get access to bash aliases

;;; Functionality
(setq disabled-command-function nil ; Re-enable all disabled commands
	    shift-select-mode nil
      delete-by-moving-to-trash t)
(xterm-mouse-mode 1)              ; Enable mouse in terminal interface
(savehist-mode)                   ; Save mini-buffer history
(delete-selection-mode 1) ; Selected text will be overwritten when you start typing
(global-auto-revert-mode t) ; Auto-update buffer if file has changed on disk
(add-hook 'before-save-hook
          'delete-trailing-whitespace) ; Delete trailing whitespace on save
(winner-mode 1)                        ; Enable undo/redo window layout
(desktop-save-mode 1)
;; (add-hook 'prog-mode-hook                 ; Show line numbers in programming modes
;;           (if (and (fboundp 'display-line-numbers-mode) (display-graphic-p))
;;               #'display-line-numbers-mode
;;             #'linum-mode))

(defun sanemacs/backward-kill-word ()
  (interactive "*")
  (push-mark)
  (backward-word)
  (delete-region (point) (mark)))

(defun find-corresponding-file ()
  "Find the file that corresponds to this one."
  (interactive)
  (setq filename nil
        basename (file-name-sans-extension buffer-file-name))
  (if (string-match "\\.c" buffer-file-name)
      (setq filename (concat basename ".h")))
  (if (string-match "\\.h" buffer-file-name)
      (if (file-exists-p (concat basename ".c")) (setq filename (concat basename ".c"))
	      (setq filename (concat basename ".cpp"))))
  (if (string-match "\\.hin" buffer-file-name)
      (setq filename (concat basename ".cin")))
  (if (string-match "\\.cin" buffer-file-name)
      (setq filename (concat basename ".hin")))
  (if (string-match "\\.cpp" buffer-file-name)
      (setq filename (concat basename ".h")))
  (if filename (find-file filename)
    (error "Unable to find a corresponding file")))

(setq makefile-names '("Makefile" "makefile"))
(defun get-nearest-makefile ()
  "Search for the Makefile traversing up the directory tree."
  (let ((dir default-directory)
	      (parent-dir (file-name-directory (directory-file-name default-directory)))
	      (nearest-makefile 'nil))
    (while (and (not (string= dir parent-dir))
		            (not nearest-makefile))
      (dolist (filename makefile-names)
	      (setq file-path (concat dir filename))
	      (when (file-readable-p file-path)
	        (setq nearest-makefile file-path)))
      (setq dir parent-dir
	          parent-dir (file-name-directory (directory-file-name parent-dir))))
    nearest-makefile))

(defun reload-config ()
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

;;;; Spell checker
(defun flyspell-brasileiro ()
  (interactive)
  (ispell-change-dictionary "brasileiro")
  (flyspell-buffer))
(defun flyspell-default ()
  (interactive)
  (ispell-change-dictionary "default")
  (flyspell-buffer))

;;;; Sane undo
(use-package undo-tree     ; Enable undo-tree, sane undo/redo behavior
  :init (global-undo-tree-mode)
  :config (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo-tree-history")))
  (global-set-key (kbd "C-z") 'undo-tree-undo)
  (global-set-key (kbd "C-S-z") 'undo-tree-redo))

;;;; Auto parenthesis
(electric-pair-mode 1)
(setq electric-pair-preserve-balance nil)

;;;; Recent files
(recentf-mode 1)
(setq recentf-max-menu-items 32)

;;;; Scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)) ; One line at a time
      mouse-wheel-progressive-speed nil ; Don't accelerate scrolling
      mouse-wheel-follow-mouse 't       ; Scroll window under mouse
      scroll-step 1)              ; Keyboard scroll one line at a time

;;;; Indentation
(setq-default tab-width 2
              c-basic-offset 2
              indent-tabs-mode nil         ; No tab indentation
              tab-always-indent 'complete) ; Tab indent first then complete

;;;; Code style
(setq-default c-default-style "k&r")

;;;; Sane auto-save and backup (put files in /tmp/ or C:/Temp/)
(defconst emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq
 backup-by-copying t                  ; Avoid symlinks
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 auto-save-list-file-prefix emacs-tmp-dir
 auto-save-file-name-transforms `((".*" ,emacs-tmp-dir t)) ; Change autosave dir to tmp
 backup-directory-alist `((".*" . ,emacs-tmp-dir)))
(setq create-lockfiles nil) ; Lockfiles unfortunately cause more pain than benefit

;;; Keybindings
(global-set-key (kbd "<f12>") 'reload-config)
(global-set-key (kbd "<f1>") 'save-buffer)
(global-set-key (kbd "<f5>") '(lambda () (interactive) (compile (format "make -C \"$(dirname '%s')\" run" (get-nearest-makefile)) t)))
(global-set-key (kbd "<f6>") '(lambda () (interactive) (compile (format "make -C \"$(dirname '%s')\" clean all" (get-nearest-makefile)))))
(global-set-key (kbd "<f7>") 'gdb)
(global-set-key (kbd "C-,") 'ffap)
(global-set-key (kbd "C-.") 'calc-dispatch)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop) ; De-indent selection by one tab length
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop) ; Indent selection by one tab length
(global-set-key (kbd "C-C A") '(lambda () (interactive) (find-file-other-window buffer-file-name) (find-corresponding-file) (other-window -1)))
(global-set-key (kbd "C-C a") 'find-corresponding-file)
(global-set-key (kbd "C-\\") 'switch-to-buffer)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x M-o") 'window-swap-states)
(global-set-key (kbd "C-x w f") 'fill-region)
(global-set-key (kbd "C-x w j") 'join-line)
(global-set-key (kbd "C-x w s") 'sort-lines)
(global-set-key (kbd "M--") 'kill-buffer-and-window)
(global-set-key (kbd "M-0") 'delete-window)
(global-set-key (kbd "M-1") 'delete-other-windows)
(global-set-key (kbd "M-2") 'split-window-below)
(global-set-key (kbd "M-3") 'split-window-right)
(global-set-key (kbd "M-4") (lookup-key global-map (kbd "C-x 4")))
(global-set-key (kbd "M-5") (lookup-key global-map (kbd "C-x 5")))
(global-set-key (kbd "M-6") (lookup-key global-map (kbd "C-x 6")))
(global-set-key (kbd "M-8") (lookup-key global-map (kbd "C-x 8")))
(global-set-key (kbd "M-<left>") 'previous-buffer)
(global-set-key (kbd "M-<right>") 'next-buffer)
(global-set-key (kbd "M-]") 'kill-buffer)
(global-set-key (kbd "M-i") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "M-o") '(lambda () (interactive) (other-window 1)))
(global-set-key (kbd "M-t") (lookup-key global-map (kbd "C-x t")))
(global-set-key [mouse-3] 'mouse-popup-menubar-stuff) ; Gives right-click a context menu
(when window-system (global-set-key (kbd "M-[") 'dabbrev-expand))
(windmove-default-keybindings)

;; (use-package helm
;;   :config
;;   (require 'helm-config)
;;   :init
;;   (helm-mode 1)
;;   :bind
;;   (("M-x"     . helm-M-x) ;; Evaluate functions
;;    ("C-x C-f" . helm-find-files) ;; Open or create files
;;    ("C-x b"   . helm-mini) ;; Select buffers
;;    ("C-x C-r" . helm-recentf) ;; Select recently saved files
;;    ("C-c i"   . helm-imenu) ;; Select document heading
;;    ("M-y"     . helm-show-kill-ring) ;; Show the kill ring
;;    :map helm-map
;;    ("C-z" . helm-select-action)
;;    ("<tab>" . helm-execute-persistent-action)))

;;; Programming Languages Support
;;;; PHP
(use-package php-mode :ensure t)

;;; Learning Emacs Lisp =======================================================