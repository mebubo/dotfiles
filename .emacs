;; -*- mode: emacs-lisp -*-

;; load plugins from ~/.emacs.d/plugins
(setq load-path (cons "~/.emacs.d/plugins" load-path))

;; make customize use it's own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; for safely loading libraries
;; http://www.emacswiki.org/emacs/LoadingLispFiles
(defmacro with-library (symbol &rest body)
  `(when (require ,symbol nil t)
     ,@body))

;; no-splash-screen
(setq inhibit-startup-message t)

;; don't stop on C-z
(global-set-key "\C-z" nil)

;; select buffer
(global-set-key "\C-x\C-b" 'bs-show)

;; switch to buffer
(iswitchb-mode t)
;(ido-mode t)

;; compile by Esc-m
(global-set-key (kbd "\e\em") 'compile)

;; view-mode
(define-key ctl-x-map "\C-q" 'view-mode)

;; transiet mark
(setq transient-mark-mode t)

;; syntax highlighting
(setq font-lock-maximum-decoration t)

;; russian
(setq default-input-method "russian-computer")

;; kill-whole-line
(setq kill-whole-line t)

;; color-theme
(with-library 'color-theme
	      (color-theme-dark-laptop))

;; desktop
(desktop-save-mode 1)
(setq desktop-buffers-not-to-save
      (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb\\|\\.gpg"
              "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)

;; recentf
(recentf-mode t)

;; savehist
(savehist-mode t)

;; server
(server-start)
(setq server-kill-new-buffers t)

;; winner
(winner-mode)

;; paren
(show-paren-mode)

;; bell
(setq visible-bell 'top-bottom)

;; backups
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs.d/backups"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups

;; reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; AUCTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(global-set-key "\C-c\C-z" 'TeX-insert-dollar)

;; uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-names t
      uniquify-buffer-name-style 'reverse)

;; mail-client
(setq mail-user-agent 'gnus-user-agent)

;; no-tabs
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; indentation
(setq-default c-basic-offset 4)
;; kernel-indentation
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "linux")))

;; cscope
(with-library 'xcscope
              (setq cscope-command-args "-q"))

;; remember
(define-key global-map (kbd "<f9> r") 'remember)
(define-key global-map (kbd "<f9> R") 'remember-region)

;; org
(require 'org-mouse)
;(org-remember-insinuate)
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)

;; dictionary
(with-library 'dictionary
              (setq dictionary-server "localhost")
              (setq dictionary-default-dictionary "mueller7")
              (setq dictionary-create-buttons nil)
              (setq dictionary-use-single-buffer t)
              (global-dictionary-tooltip-mode 1)
              (global-set-key "\C-cs" 'dictionary-search)
              (global-set-key "\C-cm" 'dictionary-match-words)
              (global-set-key "\C-cs" 'dictionary-lookup-definition))

;; mouse-select-buffer
(msb-mode)

;; ropemacs
(with-library 'pymacs
              (pymacs-load "ropemacs" "rope-")
              (setq ropemacs-enable-autoimport 't))

;; yasnippet
(with-library 'yasnippet-bundle)

;; Emacs 23: bundled EasyPG
(require 'epa)
;(epa-file-enable)

;; windmove
(windmove-default-keybindings 'meta)

;; tramp
;(require 'tramp)
(setq tramp-default-method "ssh")

;; trailing whitespace
(setq default-indicate-empty-lines t)
(setq-default show-trailing-whitespace t)

;; hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-file-name-partially
        try-complete-file-name try-expand-all-abbrevs
        try-expand-list try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; rcirc
(setq rcirc-server-alist
      '(("irc.oftc.net" :channels ("#debian" "#awesome" "#suckless" "#debian-eeepc"))
        ("irc.freenode.net" :channels ("#emacs" "#mer" "#couchdb" "#ubuntuone"))))
(rcirc-track-minor-mode 1)

;; copying lines without selecting them
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))
;; killing lines without selecting them
(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
