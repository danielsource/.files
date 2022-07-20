;;; Setup package.el
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
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
(minimal/font)
(add-hook 'prog-mode-hook 'hl-line-mode)
(use-package vscode-dark-plus-theme
	           :config
	           (if (not custom-enabled-themes)
		             (load-theme 'vscode-dark-plus t)))
(unless (display-graphic-p)
  (add-to-list 'default-frame-alist
               '(background-color . nil)))

;;; Layout
(setq frame-resize-pixelwise t ; Make frame use all available space on screen
      inhibit-startup-screen t ; Disable startup screen
      initial-scratch-message ""        ; Make *scratch* buffer blank
      linum-format "%3d "               ; Line number format
      ring-bell-function 'ignore)       ; Disable bell sound
(if (featurep 'tab-bar-mode)
    (tab-bar-mode t))
(show-paren-mode t)                   ; Show closing parens by default

;;; External
(setenv "BASH_ENV" "~/.bash_aliases")   ; Get access to bash aliases

;;; Functionality
(setq delete-by-moving-to-trash t
      history-length 1000
      mark-ring-max 32
      shift-select-mode nil
      visible-bell t
      global-auto-revert-non-file-buffers t)
(add-hook 'before-save-hook
          'delete-trailing-whitespace) ; Delete trailing whitespace on save
(add-hook 'dired-before-readin-hook
          'dired-hide-details-mode)
(add-to-list
 'display-buffer-alist
 '("*Async Shell Command*" ; Run command without displaying the output
   display-buffer-no-window (nil)))
(delete-selection-mode t) ; Selected text will be overwritten when you start typing
(global-auto-revert-mode t) ; Auto-update buffer if file has changed on disk
(save-place-mode t)
(savehist-mode t)                 ; Save mini-buffer history
(winner-mode t)                   ; Enable undo/redo window layout
(xterm-mouse-mode t)              ; Enable mouse in terminal interface

;;;; Spell checker
(defun d/flyspell-brasileiro ()
  (interactive)
  (ispell-change-dictionary "brasileiro")
  (flyspell-buffer))
(defun d/flyspell-default ()
  (interactive)
  (ispell-change-dictionary "default")
  (flyspell-buffer))

(defun d/copy-region-or-line (&optional beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if mark-active
      (clipboard-kill-ring-save beg end)
    (let ((select-enable-clipboard t))
      (kill-ring-save
       (line-beginning-position)
       (if (eobp) (point-at-eol) (+ 1 (line-end-position)))))))
(defun d/cut-region-or-line (&optional beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if mark-active
      (clipboard-kill-region beg end)
    (let ((select-enable-clipboard t))
      (kill-whole-line))))
(defun d/delete-region-or-line (&optional beg end)
  (interactive (if (use-region-p)
                   (list (region-beginning) (region-end))
                 (list nil nil)))
  (if mark-active
      (delete-region beg end)
    (delete-region
     (line-beginning-position)
     (if (eobp) (point-at-eol) (+ 1 (line-end-position))))))

(if (version<= emacs-version "27.1")
    (load-file (expand-file-name "early-init.el" user-emacs-directory))
;;;; Completion UI
  (use-package vertico
	             :init (vertico-mode t)
	             :custom (vertico-cycle t)))
(setq read-file-name-completion-ignore-case t
      read-buffer-completion-ignore-case t)

;;;; Sane undo
(use-package undo-tree     ; Enable undo-tree, sane undo/redo behavior
	           :init
	           (global-undo-tree-mode)
	           (global-set-key (kbd "C-M-z") 'revert-buffer)
	           (global-set-key (kbd "C-S-z") 'undo-tree-redo)
	           (global-set-key (kbd "C-z") 'undo-tree-undo)
	           :custom
	           (undo-tree-history-directory-alist
	            (list (cons "." (expand-file-name "undo-tree-history" user-emacs-directory)))))

;;;; Auto parenthesis
(electric-pair-mode t)
(setq electric-pair-preserve-balance nil)

;;;; Recent files
(recentf-mode t)
(setq recentf-max-menu-items 32)

;;;; Scrolling
(setq mouse-wheel-follow-mouse 't       ; Scroll window under mouse
      mouse-wheel-progressive-speed nil ; Don't accelerate scrolling
      scroll-step 1               ; Keyboard scroll one line at a time
      mouse-wheel-scroll-amount '(1 ((shift) . 1))) ; One line at a time

;;;; Sane auto-save and backup (put files in /tmp/ or C:/Temp/)
(defconst d/emacs-tmp-dir (expand-file-name (format "emacs%d" (user-uid)) temporary-file-directory))
(setq
 auto-save-file-name-transforms `((".*" ,d/emacs-tmp-dir t)) ; Change autosave dir to tmp
 auto-save-list-file-prefix d/emacs-tmp-dir
 backup-by-copying t                    ; Avoid symlinks
 backup-directory-alist `((".*" . ,d/emacs-tmp-dir))
 create-lockfiles nil ; Lockfiles unfortunately cause more pain than benefit version-control t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2)

;;; Keybindings
(add-hook 'web-mode-hook
          (lambda ()
            (define-key web-mode-map (kbd "<f5>") 'browse-url-of-buffer)))

(global-set-key (kbd "<f12>") 'minimal/reload)
(global-set-key (kbd "<f1>") 'save-buffer)
(global-set-key (kbd "<f5>") 'minimal/make-run)
(global-set-key (kbd "<f6>") 'minimal/make-clean-all)
(global-set-key (kbd "<mode-line> C-<down-mouse-1>") 'mouse-buffer-menu)
(global-set-key (kbd "C-,") 'ffap)
(global-set-key (kbd "C-<") 'indent-rigidly-left-to-tab-stop)
(global-set-key (kbd "C-<down-mouse-1>") 'ffap-at-mouse)
(global-set-key (kbd "C->") 'indent-rigidly-right-to-tab-stop)
(global-set-key (kbd "C-C a") 'minimal/corresponding-file)
(global-set-key (kbd "C-M-,") 'ffap-alternate-file)
(global-set-key (kbd "C-M-]") 'erase-buffer)
(global-set-key (kbd "C-M-g") 'term)
(global-set-key (kbd "C-S-<down-mouse-1>") 'ffap-menu)
(global-set-key (kbd "C-S-SPC") 'rectangle-mark-mode)
(global-set-key (kbd "C-S-a") 'mark-whole-buffer)
(global-set-key (kbd "C-S-c") 'd/copy-region-or-line)
(global-set-key (kbd "C-S-d") 'd/delete-region-or-line)
(global-set-key (kbd "C-S-p") 'execute-extended-command)
(global-set-key (kbd "C-S-v") 'clipboard-yank)
(global-set-key (kbd "C-S-x") 'd/cut-region-or-line)
(global-set-key (kbd "C-c D") 'ffap-list-directory)
(global-set-key (kbd "C-c d") 'dired-at-point)
(global-set-key (kbd "C-c r") 'ffap-read-only)
(global-set-key (kbd "C-x 4 C-,") 'ffap-other-window)
(global-set-key (kbd "C-x 4 C-C a") 'minimal/corresponding-file-other-window)
(global-set-key (kbd "C-x 4 C-M-,") 'ffap-alternate-file-other-window)
(global-set-key (kbd "C-x 4 C-c d") 'ffap-dired-other-window)
(global-set-key (kbd "C-x 4 C-c r") 'ffap-read-only-other-window)
(global-set-key (kbd "C-x 5 C-,") 'ffap-other-frame)
(global-set-key (kbd "C-x 5 C-c d") 'ffap-dired-other-frame)
(global-set-key (kbd "C-x 5 C-c r") 'ffap-read-only-other-frame)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "C-x M-]") 'kill-some-buffers)
(global-set-key (kbd "C-x M-o") 'window-swap-states)
(global-set-key (kbd "C-x t C-,") 'ffap-other-tab)
(global-set-key (kbd "C-x t C-c r") 'ffap-read-only-other-tab)
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
(global-set-key (kbd "M-]") 'kill-this-buffer)
(global-set-key (kbd "M-i") (lambda () (interactive) (other-window -1)))
(global-set-key (kbd "M-o") (lambda () (interactive) (other-window 1)))
(global-set-key (kbd "M-s M-e") 'eshell)
(global-set-key (kbd "M-s M-i") 'ielm)
(global-set-key (kbd "M-s M-s") 'shell)
(global-set-key (kbd "M-t") (lookup-key global-map (kbd "C-x t")))
(global-set-key (kbd "M-º") 'global-display-line-numbers-mode)
(global-set-key (kbd "M-ç") 'ffap-next)
(windmove-default-keybindings)

(when (display-graphic-p)
  (global-set-key (kbd "M-[") 'dabbrev-expand))

;;;; Show keyboard key sequences
(use-package which-key
	           :init (which-key-mode))

;;;; Complete text while typing
(use-package company
	           :init (add-hook 'after-init-hook 'global-company-mode)
	           :config
	           (global-set-key (kbd "C-c /") 'company-files)
	           (when (display-graphic-p)
	             (global-set-key (kbd "M-[") 'company-complete))
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

;;;; Emacs C source code
(setq find-function-C-source-directory "~/Downloads/emacs/src/")

(add-to-list 'auto-mode-alist '("\\.inc\\'" . c-mode))

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
	           (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
	           (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
	           (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
	           (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
	           (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
	           (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
	           (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
	           (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode)))
(org-babel-do-load-languages            ; Evaluate C in Org Mode
 'org-babel-load-languages '((C . t)))
