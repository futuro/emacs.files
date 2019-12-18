;;;
;;  Some hand-coded preferred options
;;;

;; delete excess backup versions silently
(setq delete-old-versions -1)
;; use version control
(setq version-control t)
;; Which directory to put backups file
;; This is like a very crude version control for any file
;; that isn't otherwise version controlled
(setq backup-directory-alist
      `(("." . "~/.emacs.d/backups")))

;; don't ask for confirmation when opening symlinked file
(setq vc-follow-symlinks t)

;;transform backups file name
(setq auto-save-file-name-transforms
      '((".*" "~/.emacs.d/auto-save-list/" t)))

;; inhibit useless and old-school startup screen
(setq inhibit-startup-screen t)
;; silent bell when you make a mistake
(setq ring-bell-function 'ignore)

;; use utf-8 by default
(setq coding-system-for-read 'utf-8)
(setq coding-system-for-write 'utf-8)

;; sentence SHOULD end with only a point.
(setq sentence-end-double-space nil)

;; toggle wrapping text at the 80th character
(setq default-fill-column 80)

;; print a default message in the empty scratch buffer opened at startup
(setq initial-scratch-message "Welcome to Emacs")

;;;
;;  Fonts
;;;

(if (eq system-type 'darwin)
    (set-face-attribute 'default nil
			:background "#3F3F3F"
			:foreground "#DCDCCC"
			:slant 'normal
			:weight 'normal
			:height 120
			:width 'normal
			:foundry "nil"
			:family "Source Code Pro for Powerline")
  (set-face-attribute 'default nil
		      :background "#3F3F3F"
		      :foreground "#DCDCCC"
		      :slant 'normal
		      :weight 'normal
		      :height 120
		      :width 'normal
		      :foundry "xos4"
		      :family "Terminess Powerline"))


;;;
;;  And now some packages
;;;

;; Package meta-setup

(require 'package)
;; tells emacs not to load any packages before starting up
(setq package-enable-at-startup nil)
;; the following lines tell emacs where on the internet to look up
;; for new packages.
(setq package-archives '(("org"       . "https://orgmode.org/elpa/")
                         ("gnu"       . "https://elpa.gnu.org/packages/")
                         ("melpa"     . "https://melpa.org/packages/")))
(package-initialize)

;; `use-package' is the way we're going to load packages and handle
;; their configuration and dependencies
;;
;; Bootstrap `use-package'
(unless (package-installed-p 'use-package) ; unless it is already installed
  (package-refresh-contents) ; update packages archive
  (package-install 'use-package)) ; and install the most recent version of use-package

(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t))

(use-package which-key
  :init (which-key-mode))

;; A nicer way to define key bindings.
;;
;; TODO: read about the use-package keywords to see if this is helpful
;; or not.
;; https://github.com/noctuid/general.el#use-package-keywords
;;
;; https://github.com/noctuid/general.el#about
;;(use-package general)

;; "avy is a GNU Emacs package for jumping to visible text
;;  using a char-based decision tree."
;; https://github.com/abo-abo/avy#introduction
(use-package avy
  :bind ("C-'" . avy-goto-word-1))

;; Install `ivy', `swiper', and `counsel' in one go
(use-package counsel
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1)
  :bind
  ("C-s" . swiper)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-c C-r" . ivy-resume))

(use-package ivy-hydra)

;; This provides flexible matching that more accurately aligns with
;; how we might think about string matches. `ivy' uses it if it's
;; available, thus why I'm requiring it.
;;
;; https://github.com/lewang/flx
(use-package flx)

;;;
;;; UI and Theming
;;;

;; Use my favorite theme
(use-package zenburn-theme)

;; Disable the menu GUI elements
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

;;;
;;; Clojure/Lisps
;;;

(use-package smartparens
  :init (smartparens-global-strict-mode)
  :bind
  ("M-(" . sp-wrap-round)
  ;; All the ctrl/meta-bracket/brace keys are used or map to ESC, so
  ;; instead we'll use the key right next to them. Navigating through
  ;; completions still works with this setup.
  ("M-p" . sp-wrap-square)
  ("M-P" . sp-wrap-curly)
  ("C-<left>" . sp-forward-slurp-sexp)
  :config
  (require 'smartparens-config))

(use-package clojure-mode
  :mode "\\.repl\\'"
  :init
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'hs-minor-mode))

(use-package cider
  :hook (clojure-mode . cider-mode)
  :config
  (setq cider-prompt-for-symbol nil)
  (setq cider-repl-history-file "~/.emacs.d/repl-history.clj"))

(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode))

(use-package sayid
  :after clojure-mode
  :config
  (sayid-setup-package))

(use-package company
  :config
  (global-company-mode))

(use-package slime
  :config
  (setq inferior-lisp-program "/bin/sbcl")
  (setq slime-contribs '(slime-fancy)))

;;;
;;  Web Development Settings
;;;

(use-package js2-mode
  :config
  (setq js-indent-level 2))

(use-package rjsx-mode
  :after (js2-mode))

;;;
;;  Ruby Shenanigans
;;;

(use-package enh-ruby-mode
  :config
  (add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)))

(use-package robe
  :after (company)
  :hook
  (ruby-mode . robe-mode)
  (enh-ruby-mode . robe-mode)
  :config
  (push 'company-robe company-backends))

;;;
;;; Misc
;;;

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-global-modes '(not org-mode enh-ruby-mode)))

(use-package rainbow-delimiters
  :hook (clojure-mode . rainbow-delimiters-mode))

(use-package magit)

(use-package forge
  :after magit
  :config
  (setq auth-sources (quote ("~/.authinfo.gpg"))))

(use-package git-link
  :config
  (setq git-link-use-commit t))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (setq projectile-project-search-path '("~/devel/"))
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching nil))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

(use-package htmlize)

;; Jacked from https://emacs.stackexchange.com/q/3374
(defun my/org-inline-css-hook (exporter)
  "Set code block backgrounds to current theme background.

For the HTML EXPORTER, insert custom inline css to
automatically set the background of code blocks to the current
theme's background."
  (when (eq exporter 'html)
    (let* ((my-pre-bg (face-background 'default))
           (my-pre-fg (face-foreground 'default)))
      (setq
       org-html-head-extra
       (concat
        org-html-head-extra
        (format "<style type=\"text/css\">\n pre.src {background-color: %s; color: %s;}</style>\n"
                my-pre-bg my-pre-fg))))))

(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org$" . org-mode))
  :commands (org-store-link org-capture)
  :hook ((org-mode . auto-fill-mode)
	 (org-export-before-processing-hook . 'my/org-inline-css-hook))
  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture))
  :config
  (require 'ox-md)
  (require 'ox-html)
  (require 'ox-org)
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-capture-templates
	'(("r" "Recording research notes"
	   ;; Make an entry when we capture. Another possibility is to
	   ;; have a function that picks where to put the entry
	   entry
	   ;; put it in the `org-default-notes-file' until I think of
	   ;; something better
	   (file "")
	   ;; The template
	   "* Test header
  Current user: %n
  Current File: %f
  Full Path: %F

  Annotation: %a
  Prompted Annotation: %A
  Bare link: %l

  Initial Content:
  "
	   )))
  )

(use-package csv-mode)

;; Better window selection
(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;; Undo/redo window changes
(winner-mode t)

;; SHIFT-arrowkey to move between buffers
(windmove-default-keybindings)

;; Put window splits side-by-side instead of stacked
(setq ediff-split-window-function 'split-window-horizontally)
;; Don't put the help window in its own frame when using GUI emacs
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Enable line numbers everywhere for easier pairing
(global-display-line-numbers-mode)
;; Don't shrink line number width
(setq display-line-numbers-grow-only t)
;; Use absolute line numbers, instead of relative
(setq display-line-numbers-type t)

;(setq org-directory (concat ))
;(setq org-default-notes-file
;      (concat org-directory "/notes.org"))
;;;
;;; Customization Loading
;;;

;; Put our custom file inside of version control
;; (and make sure it won't mess with loading our init.el)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
