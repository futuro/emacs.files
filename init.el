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
(setq coding-system-for-write 'utf-8 )

;; sentence SHOULD end with only a point.
(setq sentence-end-double-space nil)

;; toggle wrapping text at the 80th character
(setq default-fill-column 80)

;; print a default message in the empty scratch buffer opened at startup
(setq initial-scratch-message "Welcome to Emacs")

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

;; A nicer way to define key bindings
;; https://github.com/noctuid/general.el#about
(use-package general
  :config
  (general-define-key "C-'" 'avy-goto-word-1))

;; "avy is a GNU Emacs package for jumping to visible text
;;  using a char-based decision tree."
;; https://github.com/abo-abo/avy#introduction
(use-package avy
  :ensure t
  :commands (avy-goto-word-1))

;; Install `ivy', `swiper', and `counsel' in one go
(use-package counsel
  :ensure t
  :config
  (setq ivy-use-virtual-buffers t)
  (ivy-mode 1)
  (general-define-key "C-s" 'swiper
		      "M-x" 'counsel-M-x
		      "C-x C-f" 'counsel-find-file
		      "C-c C-r" 'ivy-resume))

;; (use-package hydra)

;; (use-package ivy-hydra
;;   :after (ivy hydra))

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

;(use-package paredit)
(use-package smartparens
  :hook ((clojure-mode . smartparens-strict-mode)
	 (elisp-mode . smartparens-strict-mode))
;(require 'smartparens-config)

(use-package clojure-mode
  :init
  (add-hook 'clojure-mode-hook #'subword-mode))

(use-package cider
  :hook (clojure-mode . cider-mode))
(use-package clj-refactor
  :hook (clojure-mode . clj-refactor-mode))

;;;
;;; Misc
;;;

(use-package rainbow-delimiters
  :hook (clojure-mode . rainbow-delimiters-mode))
(use-package magit)
(use-package exec-path-from-shell)

(global-display-line-numbers-mode)

;;;
;;; Custom Loading
;;;

;; Put our custom file inside of version control
;; (and make sure it won't mess with loading our init.el)
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
