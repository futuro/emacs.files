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
