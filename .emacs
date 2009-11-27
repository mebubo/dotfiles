(setq load-path (cons "~/.emacs.d/plugins" load-path))
(defmacro with-library (symbol &rest body)
  `(when (require ,symbol nil t)
     ,@body))
;; --keys--
(global-set-key "\C-z" nil)
(global-set-key "\C-x\C-b" 'bs-show)
(global-set-key (kbd "\e\em") 'compile)
(define-key ctl-x-map "\C-q" 'view-mode)
;; -vars--
(setq transient-mark-mode t)
;(setq view-read-only t)
(setq font-lock-maximum-decoration t)
(setq default-input-method "russian-computer")
(setq compilation-read-command nil)
(setq preview-scale-function 1.2)
(setq kill-whole-line t)
;; --iswitchb--
(iswitchb-mode t)
;; --color-theme--
(with-library 'color-theme
	      (color-theme-dark-laptop))
;; --desktop--
(desktop-save-mode 1)
(setq desktop-buffers-not-to-save
      (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb\\|\\.gpg"
              "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
;; --recentf--
(recentf-mode t)
;; --savehist--
(savehist-mode t)
;; --server--
(server-start)
;; --winner--
(winner-mode)
;; --paren--
(show-paren-mode)
;; --misc--
(put 'upcase-region 'disabled nil)
;; --bell--
(setq visible-bell 'top-bottom)
;; --backups--
(setq backup-by-copying t      ; don't clobber symlinks
      backup-directory-alist
      '(("." . "~/.emacs.d/backups"))    ; don't litter my fs tree
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)       ; use versioned backups
;; --imaxima--
(autoload 'imaxima "imaxima" "Image support for Maxima" t)
(autoload 'maxima "maxima" "Original Maxima mode" t)
(setq imaxima-use-maxima-mode-flag t)
(autoload 'imath-mode "imath" "Interactive Math minor mode." t)
;; --reftex--
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; --AUCTeX--
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(global-set-key "\C-c\C-z" 'TeX-insert-dollar)
;; --server-new-frame--
(setq server-kill-new-buffers t)
;; --no-splash-screen--
(setq inhibit-startup-message t)
;; --uniquify--
(require 'uniquify)
(setq uniquify-buffer-names t
      uniquify-buffer-name-style 'reverse)
;; --mail-client--
(setq mail-user-agent 'gnus-user-agent)
;; --no-tabs--
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;; --cscope--
(require 'xcscope)
;(setq cscope-command-args "-q")
;; --kernel-indentation--
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "linux")))
;; --indentation--
;(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
;; --remember--
(define-key global-map (kbd "<f9> r") 'remember)
(define-key global-map (kbd "<f9> R") 'remember-region)
;; --org--
(require 'org-mouse)
;(org-remember-insinuate)
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)
;; --ido--
;(ido-mode t)
;; --host-specific-config--
(load-file "~/.emacs.host")
;; --dictionary--
;; (setq dictionary-server "localhost")
;; (setq dictionary-default-dictionary "mueller7")
;; (setq dictionary-create-buttons nil)
;; (setq dictionary-use-single-buffer t)
;; (global-dictionary-tooltip-mode 1)
;; (global-set-key "\C-cs" 'dictionary-search)
;; (global-set-key "\C-cm" 'dictionary-match-words)
;; (global-set-key "\C-cs" 'dictionary-lookup-definition)
;; --mouse-select-buffer--
(msb-mode)
;; --ropemacs--
;;(require 'pymacs)
;;(pymacs-load "ropemacs" "rope-")
;;(setq ropemacs-enable-autoimport 't)
;; --yasnippet--
;; (require 'yasnippet-bundle)
;; Emacs 23: bundled EasyPG
(require 'epa)
;;(epa-file-enable)
;; --windmove--
(windmove-default-keybindings 'meta)
;; --tramp--
;(require 'tramp)
(setq tramp-default-method "ssh")
;; --trailing whitespace--
(setq default-indicate-empty-lines t)
(setq-default show-trailing-whitespace t)
;; --hippie-expand--
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
;; --make customize use it's own file--
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; wanderlust
(autoload 'wl "wl" "Wanderlust" t)
(autoload 'wl-other-frame "wl" "Wanderlust on new frame." t)
(autoload 'wl-draft "wl-draft" "Write draft with Wanderlust." t)

;; IMAP
(setq elmo-imap4-default-server "imap.gmail.com")
(setq elmo-imap4-default-user "dolgovs@gmail.com")
(setq elmo-imap4-default-authenticate-type 'clear)
(setq elmo-imap4-default-port '993)
(setq elmo-imap4-default-stream-type 'ssl)

(setq elmo-imap4-use-modified-utf7 t)

;; SMTP
(setq wl-smtp-connection-type 'starttls)
(setq wl-smtp-posting-port 587)
(setq wl-smtp-authenticate-type "plain")
(setq wl-smtp-posting-user "mattofransen")
(setq wl-smtp-posting-server "smtp.gmail.com")
(setq wl-local-domain "gmail.com")

(setq wl-default-folder "%inbox")
(setq wl-default-spec "%")
(setq wl-draft-folder "%[Gmail]/Drafts") ; Gmail IMAP
(setq wl-trash-folder "%[Gmail]/Trash")

(setq wl-folder-check-async t)

(setq elmo-imap4-use-modified-utf7 t)

(autoload 'wl-user-agent-compose "wl-draft" nil t)
(if (boundp 'mail-user-agent)
    (setq mail-user-agent 'wl-user-agent))
(if (fboundp 'define-mail-user-agent)
    (define-mail-user-agent
      'wl-user-agent
      'wl-user-agent-compose
      'wl-draft-send
      'wl-draft-kill
      'mail-send-hook))

;; rcirc
(setq rcirc-server-alist
      '(("irc.oftc.net" :channels ("#debian" "#awesome" "#suckless" "#debian-eeepc"))
        ("irc.freenode.net" :channels ("#emacs" "#mer" "#couchdb"))))


;; copying lines without selecting them
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))
