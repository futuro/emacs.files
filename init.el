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
(setq initial-scratch-message ";; Welcome to Emacs")

;; I often want pdfs and friends to auto-revert when their contents
;; change, since I'm likely editing the source tex doc while viewing
;; it
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; I run the emacs server, so when I want to really truly kill emacs
;; it's nice to have this bound.
(global-set-key (kbd "C-x C-M-c")
		'save-buffers-kill-emacs)

;; Set up a global key for hs-toggle-hiding
(global-set-key (kbd "M-/") 'hs-toggle-hiding)

(eval-after-load "hideshow"
  '(add-to-list 'hs-special-modes-alist
              `(enh-ruby-mode
                ,(rx (or "def" "class" "module" "do" "{" "[" "if" "else" "unless")) ; Block start
                ,(rx (or "}" "]" "end"))                       ; Block end
                ,(rx (or "#" "=begin"))                        ; Comment start
                enh-ruby-forward-sexp nil)))

;;;
;;  Fonts
;;;

;; (set-face-attribute 'default nil
;; 		    :background "#3F3F3F"
;; 		    :foreground "#DCDCCC"
;; 		    :slant 'normal
;; 		    :weight 'normal
;; 		    :height 220
;; 		    :width 'normal
;; 		    :family "Terminus")

;; HiDPI monitors and emacs act funny, but this gets me the right size
;; on my second monitor
(set-frame-font "-Xos4-Terminus-normal-normal-normal-*-26-*-*-*-m-*-iso10646-1")

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

;; mu4e is installed locally
(if (file-directory-p "/usr/share/emacs/site-lisp/mu/mu4e")
    (use-package mu4e
      :load-path "/usr/share/emacs/site-lisp/mu/mu4e"
      :config
      (setq
       ;; top-level Maildir
       mu4e-maildir       "~/mail/eniessenderry"
       ;; folder for sent messages
       mu4e-sent-folder   "/Sent_Mail"
       ;; unfinished messages
       mu4e-drafts-folder "/Drafts"
       ;; trashed  messages
       mu4e-trash-folder  "/Trash"

       mu4e-maildir-shortcuts
       '(("/INBOX" . ?i)
	 ("/Sent_Mail" . ?s)
	 ("/Trash" . ?t)
	 ("/All_Mail" . ?a))

       mu4e-html2text-command
       "pandoc --from=html --to=plain -"
       mu4e-get-mail-command "offlineimap")))

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
  :bind
  ("C-'" . avy-goto-word-1)
  ("C-\"" . avy-goto-char))

(use-package wgrep)

;; Install `ivy', `swiper', and `counsel' in one go
(use-package counsel
  :config
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  (ivy-mode 1)
  :bind
  ("C-s" . swiper)
  ("M-x" . counsel-M-x)
  ("M-y" . counsel-yank-pop)
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
(use-package zenburn-theme
  :config
  (enable-theme 'zenburn))

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
  (("M-(" . sp-wrap-round)
   ;; All the ctrl/meta-bracket/brace keys are used or map to ESC, so
   ;; instead we'll use the key right next to them. Navigating through
   ;; completions still works with this setup.
   ("M-p" . sp-wrap-square)
   ("M-P" . sp-wrap-curly)
   ;; Some movement commands.
   ("C-M-f" . sp-forward-sexp)
   ("C-M-b" . sp-backward-sexp)
   ;; w is 'backward' because it's left of the e key
   ("C-M-e" . sp-up-sexp)
   ("C-M-w" . sp-backward-up-sexp)
   ;; Directions define the movement of the delimiter. Slurping moves
   ;; delimiter over next form, barfing moves delimiter before next
   ;; form (or, said slightly differently, "barfs" the next form past
   ;; the delimiter).
   ("C-S-<right>" . sp-forward-slurp-sexp)
   ("C-S-<left>" . sp-forward-barf-sexp)
   ;; Unwrapping commands. Backspace was chosen for the `back'wards
   ;; motion. I might change this in the future
   ("S-M-<delete>" . sp-unwrap-sexp)
   ("S-M-<backspace>" . sp-backward-unwrap-sexp)
   ;; Splice commands. Here, backspace is used for the `back'ward
   ;; motion as well.
   ("C-M-<delete>" . sp-splice-sexp-killing-forward)
   ("C-M-<backspace>" . sp-splice-sexp-killing-backward)
   ;; Change inside the next/enclosing expression
   ("M-i" . sp-change-inner) ; Change the inside of the next balanced expression
   ("M-e" . sp-change-enclosing) ; Change the inside of the enclosing expression
   )
  :config
  (require 'smartparens-config))

(use-package highlight-parentheses
  :ensure t
  :config
  (highlight-parentheses-mode)
  (global-highlight-parentheses-mode))

(use-package clojure-mode
  :mode "\\.repl\\'"
  :init
  (add-hook 'clojure-mode-hook #'subword-mode)
  (add-hook 'clojure-mode-hook #'hs-minor-mode)
  (setq clojure-use-metadata-for-privacy t))

(use-package cider
  :hook (clojure-mode . cider-mode)
  :config
  (setq cider-repl-buffer-size-limit 200000)
  (setq cider-prompt-for-symbol nil)
  (setq cider-repl-history-file "~/.emacs.d/repl-history.clj")
  (setq cider-repl-pop-to-buffer-on-connect nil))

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

(use-package fennel-mode)

;;;
;;  Whitespace Sensitive Formats
;;;

(defun aj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

(use-package yaml-mode
  :mode "\\.yml\\'"
  :bind
  (("M-C-i" . aj-toggle-fold)))

;;;
;;  Web Development Settings
;;;

(use-package js2-mode
  :config
  (setq js-indent-level 2))

(use-package rjsx-mode
  :after (js2-mode))

(use-package web-mode
  :mode "\\.erb\\'"
  :config
  (setq web-mode-markup-indent-offset 2))

;;;
;;; Statistics
;;;

(use-package ess
  :config
  (setq ess-style 'DEFAULT))

;;;
;;; Misc
;;;

;; Typescript
(use-package tide)

(use-package w3m)

(use-package dockerfile-mode)

(use-package terraform-mode)

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode)
  :config
  (setq flycheck-global-modes '(not org-mode)))

(use-package rainbow-delimiters
  :hook (clojure-mode . rainbow-delimiters-mode))

(use-package magit)

(use-package git-commit
  ;; :config
  ;; (setq git-commit-major-mode 'markdown-mode)
  )

;; The forge package uses this, but since it's applicable no matter
;; the package that's using it, I've pulled it into the top-level.
(setq auth-sources '("~/.authinfo.gpg"))

(use-package forge
  :after magit)

(use-package git-link
  :config
  (setq git-link-use-commit t))

(use-package exec-path-from-shell
  :config
  (setq exec-path-from-shell-shell-name "/bin/zsh")
  (exec-path-from-shell-initialize))

(use-package projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (projectile-mode +1)
  (if (not (eq system-type 'darwin))
      (setq projectile-project-search-path '("~/devel/"))
    (setq projectile-project-search-path '("~/devel/" "~/devel/active-repos/" "~/devel/archived-repos/")))
  (setq projectile-completion-system 'ivy)
  (setq projectile-enable-caching nil)
  (push ".cpcache" projectile-globally-ignored-directories)
  (push ".shadow-cljs" projectile-globally-ignored-directories))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode))

;;;
;;  SQL
;;;

(use-package sql-indent
  :after sql
  :hook ((sql-mode . sqlind-minor-mode)))

;;;
;;  Document Editing
;;;

(use-package tex-site
  :load-path "/usr/share/emacs/site-lisp"
  :mode ("\\.tex\\'" . TeX-latex-mode))

;; Edit regions in separate buffers, like `org-edit-src-code' but for
;; arbitrary regions.
(use-package edit-indirect)

(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

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

;;;
;;  Let's Get ORGanized!
;;;

;;; Supporting functions

;; TODO: is this necessary, or is which-func loaded by default?
(require 'which-func)

;; Borrowed and Inspired by https://gitlab.com/howardabrams/spacemacs.d/-/blob/b356637e49500d0856b7128ce764ca12cb32c2a1/layers/ha-org/funcs.el#L352


;; TODO: ~futuro/org-capture-fileref-snippet~ needs to be called
;; within a ~with-current-buffer~ macro, I think. Is there a way to
;; make that less implicitly necessary?

(defun futuro/org-capture-fileref-snippet (f type headers func-name)
  "Given a file F, a structure template TYPE, any desired
HEADERS, and an optional FUNC-NAME, return a structural template
referencing the file the snippet came from, and with the type
specified."
  (let* ((code-snippet
	  (buffer-substring-no-properties (mark) (point)))
	 (file-name   (buffer-file-name))
	 (file-base   (file-name-nondirectory file-name))
	 (line-number (line-number-at-pos (region-beginning)))
	 (git-link    (git-link-url (git-link--select-remote) line-number))
	 (remote-link (if (string-match "Remote .+ not found" git-link)
			  "/(no remote available)"
			(format "/[[%s][(remote)]]" git-link)))
	 (initial-txt (if (null func-name)
			  (format "From ~%s~ [[file:%s::%s][(local)]]%s:"
				  file-base file-name line-number remote-link)
			(format "From ~%s~ (in ~%s~ [[file:%s::%s][(local)]]%s):"
				func-name file-base file-name line-number
				remote-link))))
    (format "%s

  #+BEGIN_%s %s -n %s -r -l \"%s (ref: %%s)\"
%s
  #+END_%s" initial-txt type headers line-number comment-start code-snippet type)))

(defun futuro/org-capture-code-snippet (f)
  "Capture a code snippet from file F.

Given a file, F, this captures the currently selected text
within an Org SRC block with a language based on the current mode
and a backlink to the function and the file."
  (with-current-buffer (find-buffer-visiting f)
    (let ((org-src-mode (replace-regexp-in-string "-mode" "" (format "%s" major-mode)))
          (func-name (which-function)))
      (futuro/org-capture-fileref-snippet f "SRC" org-src-mode func-name))))

(defun futuro/org-capture-clip-snippet (f)
  "Given a file, F, this captures the currently selected text
within an Org EXAMPLE block and a backlink to the file."
  (with-current-buffer (find-buffer-visiting f)
    (futuro/org-capture-fileref-snippet f "EXAMPLE" "" nil)))

(use-package org
  :ensure org-plus-contrib
  :mode (("\\.org$" . org-mode))
  :commands (org-store-link org-capture)
  :hook ((org-mode . auto-fill-mode)
	 (org-mode . flyspell-mode)
	 (org-export-before-processing . my/org-inline-css-hook))
  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ([remap org-cycle-agenda-files] . avy-goto-word-1))
  :config
  (setq org-fontify-quote-and-verse-blocks t)
  (setq org-clock-persist 'history)
  (org-clock-persistence-insinuate)
  (require 'ox-md)
  (require 'ox-html)
  (require 'ox-org)
  (require 'ob-eshell)
  (setq org-babel-load-languages
	'((emacs-lisp . t)
	  (sql . t)
	  (shell . t)
	  (eshell . t)))
  (org-babel-do-load-languages
   'org-babel-load-languages
   org-babel-load-languages)
  (add-to-list 'org-src-lang-modes '("enh-ruby" . ruby))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-refile-targets '((nil . (:maxlevel . 20)))) ; Let me refile to any heading in the buffer
  (setq org-capture-templates
	'(("c" "Clock experiments"
	   ;; Plain, non-list item
	   plain
	   ;; Put it with the currently clocked item, for ease of
	   ;; selecting capture location
	   (clock)
	   ;; Drop in a captured code snippet with point afterwards
	   "%? %(futuro/org-capture-code-snippet \"%F\")"
	   ;; 1 line of padding around the item
	   :empty-lines 1))))

(use-package csv-mode)

;; Better window selection
(use-package ace-window
  :bind
  ("M-o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package writeroom-mode)

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

;; Allow narrow-to-region from keybindings
(put 'narrow-to-region 'disabled nil)

;; Start an emacs server
(server-start)
