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
(setq-default cursor-type 'bar ; Line-style cursor similar to other text editors
              frame-title-format '("%b")) ; Make window title the buffer name
(set-frame-font "Noto Sans Mono 9" nil t)
(set-face-font 'fixed-pitch-serif "Courier New Bold")
(add-hook 'prog-mode-hook 'hl-line-mode)
(use-package dracula-theme
  :config
  (if (not custom-enabled-themes)
      (load-theme 'dracula t)))
(unless (display-graphic-p)
  (add-to-list 'default-frame-alist
               '(background-color . nil)))

;;; Layout
(setq column-number-mode t
      ring-bell-function 'ignore        ; Disable bell sound
      linum-format "%3d "               ; Line number format
      frame-resize-pixelwise t)         ; Make frame use all available space on screen
(tab-bar-mode 1)
(fset 'yes-or-no-p 'y-or-n-p) ; y-or-n-p makes answering questions faster
(show-paren-mode 1)           ; Show closing parens by default
(setq inhibit-startup-screen t)         ; Disable startup screen
(setq initial-scratch-message "")       ; Make *scratch* buffer blank

;;; External
(setenv "BASH_ENV" "~/.bash_aliases")   ; Get access to bash aliases

;;; Functionality
(setq disabled-command-function nil  ; Re-enable all disabled commands
      shift-select-mode nil
      delete-by-moving-to-trash t
      history-length 1000
      global-auto-revert-non-file-buffers t
      visible-bell t
      mark-ring-max 32)
(xterm-mouse-mode 1)              ; Enable mouse in terminal interface
(savehist-mode 1)                   ; Save mini-buffer history
(save-place-mode 1)
(delete-selection-mode 1) ; Selected text will be overwritten when you start typing
(global-auto-revert-mode t) ; Auto-update buffer if file has changed on disk
(add-hook 'before-save-hook
          'delete-trailing-whitespace) ; Delete trailing whitespace on save
(add-hook 'dired-before-readin-hook
          'dired-hide-details-mode)
(winner-mode 1)                        ; Enable undo/redo window layout

(defun d/find-corresponding-file ()
  "Find the file that corresponds to this one."
  (interactive)
  (let ((filename nil)
        (basename (file-name-sans-extension buffer-file-name)))
  (if (string-match "\\.c" buffer-file-name)
      (setq filename (concat basename ".h")))
  (if (string-match "\\.h" buffer-file-name)
      (setq filename (concat basename ".c")))
  (if (string-match "\\.cpp" buffer-file-name)
      (setq filename (concat basename ".hpp")))
  (if (string-match "\\.hpp" buffer-file-name)
      (setq filename (concat basename ".cpp")))
  (if filename (find-file filename)
    (error "Unable to find a corresponding file"))))

(defun d/find-corresponding-file-other-window ()
  (interactive)
  (find-file-other-window buffer-file-name)
  (d/find-corresponding-file)
  (other-window -1))

(setq d/makefile-names '("Makefile" "makefile"))
(defun d/get-nearest-makefile ()
  "Search for the Makefile traversing up the directory tree."
  (let ((dir default-directory)
        (parent-dir (file-name-directory (directory-file-name default-directory)))
        (nearest-makefile 'nil))
    (while (and (not (string= dir parent-dir))
                (not nearest-makefile))
      (dolist (filename d/makefile-names)
        (setq file-path (concat dir filename))
        (when (file-readable-p file-path)
          (setq nearest-makefile file-path)))
      (setq dir parent-dir
            parent-dir (file-name-directory (directory-file-name parent-dir))))
    nearest-makefile))

(defun d/compile-make (arguments)
  (compile (concat
            (format "make -C \"$(dirname '%s')\" " (d/get-nearest-makefile)) arguments)
           t))
(defun d/compile-make-run () (interactive) (d/compile-make "run"))
(defun d/compile-make-clean-all () (interactive) (d/compile-make "clean all"))

(defun d/reload-config ()
  (interactive)
  (load-file (expand-file-name "init.el" user-emacs-directory)))

;;;; Spell checker
(defun d/flyspell-brasileiro ()
  (interactive)
  (ispell-change-dictionary "brasileiro")
  (flyspell-buffer))
(defun d/flyspell-default ()
  (interactive)
  (ispell-change-dictionary "default")
  (flyspell-buffer))

;;;; Completion UI
(use-package vertico
  :init (vertico-mode 1)
  :custom (vertico-cycle t))

;;;; Sane undo
(use-package undo-tree     ; Enable undo-tree, sane undo/redo behavior
  :init
  (global-undo-tree-mode)
  (global-set-key (kbd "C-M-z") 'revert-buffer)
  (global-set-key (kbd "C-S-z") 'undo-tree-redo)
  (global-set-key (kbd "C-z") 'undo-tree-undo)
  :custom
  (undo-tree-history-directory-alist
   '(("." . "~/.emacs.d/undo-tree-history"))))

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
              tab-always-indent 'complete  ; Tab indent first then complete
              c-tab-always-indent 'complete)

;;;; Code style
(setq-default c-default-style "k&r")

;;;; Sane auto-save and backup (put files in /tmp/ or C:/Temp/)
(defconst d/emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq
 backup-by-copying t                  ; Avoid symlinks
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t
 auto-save-list-file-prefix d/emacs-tmp-dir
 auto-save-file-name-transforms `((".*" ,d/emacs-tmp-dir t)) ; Change autosave dir to tmp
 backup-directory-alist `((".*" . ,d/emacs-tmp-dir)))
(setq create-lockfiles nil) ; Lockfiles unfortunately cause more pain than benefit

;;; Keybindings
(add-hook 'c-initialization-hook
          '(lambda ()
             (define-key c-mode-map (kbd "<f5>") 'd/compile-make-run)
             (define-key c-mode-map (kbd "<f6>") 'd/compile-make-clean-all)))
(add-hook 'makefile-mode-hook
          '(lambda ()
             (define-key makefile-mode-map (kbd "<f5>") 'd/compile-make-run)
             (define-key makefile-mode-map (kbd "<f6>") 'd/compile-make-clean-all)))
(add-hook 'compilation-shell-minor-mode-hook
          '(lambda ()
             (define-key compilation-shell-minor-mode-map (kbd "<f5>") 'd/compile-make-run)
             (define-key compilation-shell-minor-mode-map (kbd "<f6>") 'd/compile-make-clean-all)))
(add-hook 'web-mode-hook
          '(lambda ()
             (define-key web-mode-map (kbd "<f5>") 'browse-url-of-buffer)))

(global-set-key (kbd "<f12>") 'd/reload-config)
(global-set-key (kbd "<f1>") 'save-buffer)
(global-set-key (kbd "<f9>") 'repeat)
(global-set-key (kbd "C-,") 'ffap)
(global-set-key (kbd "C-.") 'calc-dispatch)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop) ; De-indent selection by one tab length
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop) ; Indent selection by one tab length
(global-set-key (kbd "C-C A") 'd/find-corresponding-file-other-window)
(global-set-key (kbd "C-C a") 'd/find-corresponding-file)
(global-set-key (kbd "C-M-]") 'erase-buffer)
(global-set-key (kbd "C-S-SPC") 'rectangle-mark-mode)
(global-set-key (kbd "C-S-c") 'clipboard-kill-ring-save)
(global-set-key (kbd "C-S-p") 'execute-extended-command)
(global-set-key (kbd "C-S-v") 'clipboard-yank)
(global-set-key (kbd "C-S-x") 'clipboard-kill-region)
(global-set-key (kbd "C-\\") 'switch-to-buffer)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x M-]") 'kill-some-buffers)
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
(global-set-key (kbd "M-<f9>") 'repeat-complex-command)
(global-set-key (kbd "M-<left>") 'previous-buffer)
(global-set-key (kbd "M-<right>") 'next-buffer)
(global-set-key (kbd "M-]") 'kill-this-buffer)
(global-set-key (kbd "M-i") '(lambda () (interactive) (other-window -1)))
(global-set-key (kbd "M-o") '(lambda () (interactive) (other-window 1)))
(global-set-key (kbd "M-s M-s") 'shell)
(global-set-key (kbd "M-t") (lookup-key global-map (kbd "C-x t")))
(global-set-key (kbd "M-º") 'global-display-line-numbers-mode)
(global-set-key [mouse-3] 'mouse-popup-menubar-stuff) ; Gives right-click a context menu
(when (display-graphic-p) (global-set-key (kbd "M-[") 'dabbrev-expand))
(windmove-default-keybindings)

;;;; Show keyboard key sequences
(use-package which-key
  :init (which-key-mode))

;;;; Complete text while typing
(use-package company
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (global-set-key (kbd "C-c /") 'company-files)
  (when (display-graphic-p)
    (global-set-key (kbd "M-[") 'company-complete-common))
  :custom
  (company-idle-delay 0))
(use-package company-web
  :init
  (add-hook 'web-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends) '(company-web-html))
              (company-mode t))))

;;; Programming Languages Support

;;;; GDB (debugger)
(setq gdb-many-windows t)

;;;; LSP
;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :init
;;   (add-hook 'c-mode-hook 'lsp)
;;   (add-hook 'c++-mode-hook 'lsp)
;;   (setq lsp-keymap-prefix "C-c l")
;;   :config
;;   (lsp-enable-which-key-integration t))

(use-package nasm-mode)
(use-package php-mode)
(use-package web-mode
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2)
  :config
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))
(org-babel-do-load-languages            ; Evaluate C in Org Mode
 'org-babel-load-languages '((C . t)))

;;; Learning Emacs Lisp =======================================================
