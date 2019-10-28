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
  (general-define-key "C-s" 'swiper
		      "M-x" 'counsel-M-x
		      "C-x C-f" 'counsel-find-file
		      "C-c C-r" 'ivy-resume))

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
;;; Misc
;;;

(use-package rainbow-delimiters
  :hook (clojure-mode . rainbow-delimiters-mode))

(use-package magit)

(use-package git-link
  :config
  (setq git-link-use-commit t))

(use-package exec-path-from-shell
  :config
  (exec-path-from-shell-initialize))

(global-display-line-numbers-mode)

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

(use-package org-plus-contrib
  :hook (org-mode . auto-fill-mode))

(use-package csv-mode)

(use-package enh-ruby-mode
  :config
  (add-to-list 'auto-mode-alist
             '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)))

(use-package robe
  :after (company)
  :hook
  (ruby-mode-hook . robe-mode)
  (enh-ruby-mode-hook . robe-mode)
  :config
  (push 'company-robe company-backends))

(winner-mode t)
(windmove-default-keybindings)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

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
