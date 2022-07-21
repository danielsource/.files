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

;;; External
(setenv "BASH_ENV" "~/.bash_aliases")   ; Get access to bash aliases

;;; Functionality
(setq delete-by-moving-to-trash t
      history-length 1000
      mark-ring-max 32
      shift-select-mode nil)

(if (version<= emacs-version "27.1")
    (load-file (expand-file-name "early-init.el" user-emacs-directory))
;;;; Completion UI
  (use-package vertico
               :init (vertico-mode t)
               :custom (vertico-cycle t)))

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

;;;; Emacs C source code
(setq find-function-C-source-directory "~/Downloads/emacs/src/")

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
