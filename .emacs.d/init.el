;; -*- mode: emacs-lisp -*-

;; for safely loading libraries
;; http://www.emacswiki.org/emacs/LoadingLispFiles
(defmacro with-library (symbol &rest body)
  `(when (require ,symbol nil t)
     ,@body))

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(add-to-list 'load-path "~/.emacs.d/my-lisp")
(load-library "my-shell")

;; make customize use it's own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; font
(set-frame-font "monospace-10")

;; packages
(with-library 'package
              (add-to-list 'package-archives
                           '("marmalade" . "http://marmalade-repo.org/packages/") t)
              (package-initialize)

              (when (not package-archive-contents)
                (package-refresh-contents))

              (defvar my-packages '(smex markdown-mode git-commit
                                         expand-region highlight-symbol))

              (dolist (p my-packages)
                (when (not (package-installed-p p))
                  (package-install p))))

;; no-splash-screen
(setq inhibit-startup-message t)

;; don't stop on C-z
(global-set-key "\C-z" nil)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;; switch to buffer
;(iswitchb-mode t)

;; ido
(ido-mode 'buffers)
(add-to-list 'ido-ignore-buffers "^\*")

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

;; recentf
;; http://www.masteringemacs.org/articles/2011/01/27/find-files-faster-recent-files-package/
;; also see recentf-open-files
(recentf-mode t)
(setq recentf-max-saved-items 200)
(defun ido-recentf-open ()
  "Use `ido-completing-read' to \\[find-file] a recent file"
  (interactive)
  (if (find-file (ido-completing-read "Find recent file: " recentf-list))
      (message "Opening file...")
    (message "Aborting")))
(global-set-key (kbd "C-x C-r") 'ido-recentf-open)

;; savehist
(savehist-mode t)

;; server
(server-start)
(setq server-kill-new-buffers t)

;; winner
(require 'winner)
(setq winner-dont-bind-my-keys t) ;; default bindings conflict with org-mode
(global-set-key (kbd "<C-s-left>") 'winner-undo)
(global-set-key (kbd "<C-s-right>") 'winner-redo)
(winner-mode t)

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
(setq org-directory "~/org/")
(setq org-default-notes-file (concat org-directory "/notes.org"))
(define-key global-map "\C-cr" 'org-remember)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-log-done t)
(setq org-export-html-postamble nil
      org-export-html-preamble nil)

;; dictionary
(with-library 'dictionary
              (setq dictionary-server "localhost")
              (setq dictionary-default-dictionary "wn")
              (setq dictionary-create-buttons nil)
              (setq dictionary-use-single-buffer t)
              (global-dictionary-tooltip-mode 1)
              (defun my-dictionary-lookup-definition ()
                (interactive)
                (dictionary-lookup-definition)
                (other-window 1)
                )
              (global-set-key "\C-cs" 'dictionary-search)
              (global-set-key "\C-cm" 'dictionary-match-words)
              (global-set-key "\C-cd" 'my-dictionary-lookup-definition))

;; mouse-select-buffer
(msb-mode)

;; ropemacs
(with-library 'pymacs
              (pymacs-load "ropemacs" "rope-")
              (setq ropemacs-enable-autoimport 't))

;; yasnippet
(with-library 'yasnippet-bundle)

;; Emacs 23: bundled EasyPG
(with-library 'epa)

;; windmove
(windmove-default-keybindings 'meta)

;; tramp
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
      '(("irc.oftc.net" :channels ("#awesome" "#suckless"))
        ("irc.freenode.net" :channels ("#emacs" "#notmuch" "#ac100"))))
(setq rcirc-omit-responses '("JOIN" "PART" "QUIT" "NICK" "AWAY"))
(rcirc-track-minor-mode 1)
(add-hook 'window-configuration-change-hook
          '(lambda ()
             (setq rcirc-fill-column (- (window-width) 2))))

(defun rcirc-sort-name (buf)
  "Return server process and buffer name as a string."
  (with-current-buffer buf
    (downcase (concat (if rcirc-server-buffer
                          (buffer-name rcirc-server-buffer)
                        " ")
                      " "
                      (or rcirc-target "")))))

(defun rcirc-sort-buffers (a b)
  "Sort buffers A and B using `rcirc-sort-name'."
  (string< (rcirc-sort-name a)
           (rcirc-sort-name b)))

(add-hook 'rcirc-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))

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

;; bookmarks
(setq bookmark-default-file "~/.emacs.d/bookmarks"
      bookmark-save-flag 1)

;; dired open marked files
(eval-after-load "dired"
  '(progn
     (define-key dired-mode-map "F" 'my-dired-find-file)
     (defun my-dired-find-file (&optional arg)
       "Open each of the marked files, or the file under the point, or when prefix arg, the next N files "
       (interactive "P")
       (let* ((fn-list (dired-get-marked-files nil arg)))
         (mapc 'find-file fn-list)))))

;; notmuch
(with-library 'notmuch
              (require 'notmuch-address)
              (require 'notmuch-maildir-fcc)
              (setq notmuch-folders '(
                                      ("personal" . "tag:personal AND tag:inbox")
                                      ("vm-sqe-spb" . "tag:inbox AND tag:vm-sqe-spb")
                                      ("vm-sqe" . "tag:inbox AND tag:vm-sqe")
                                      ("cr" . "tag:inbox AND tag:cr")
                                      ("spb-all" . "tag:inbox AND tag:spb-all")
                                      ("phare" . "phare and tag:inbox")
                                      ("imp" . "tag:imp")
                                      ("int" . "tag:int")
                                      ("todo" . "tag:todo")
                                      ("sent" . "tag:sent AND tag:inbox")
                                      ("inbox" . "tag:inbox")
                                      )
                    notmuch-search-oldest-first nil
                    notmuch-address-command "addrlookup"
                    notmuch-fcc-dirs '(("sent"))
                    notmuch-search-line-faces '(("todo" . '(:foreground "brightred"))
                                                ("imp" . '(:foreground "brightmagenta"))
                                                ("personal" . '(:foreground "cyan"))
                                                ("unread" . '(:foreground "green")))
                    notmuch-message-headers '("Subject" "To" "Cc" "Date" "User-Agent")
                    notmuch-mua-hidden-headers nil
                    notmuch-search-result-format '(("date" . "%s ") ("count" . "%-7s ")
                                                   ("authors" . "%-30s ") ("subject" . "%s ") ("tags" . "(%s)"))
                    notmuch-show-all-tags-list t
                    )

              (global-set-key [f8] 'notmuch)
              (add-hook 'notmuch-search-hook
                        (lambda ()
                          (setq show-trailing-whitespace nil)))

)

;; typing-practice
(with-library 'typing-practice
              (setq typing-practice-time-threshold 10))

;; mouse yank
(setq mouse-yank-at-point t)

;; scheme
(with-library 'quack
              (setq quack-default-program "scheme"))

;; dired
(setq dired-auto-revert-buffer t)

;; blink-cursor
(blink-cursor-mode 0)

;; jabber
(with-library 'jabber
              (setq jabber-account-list '(
                                          ("dolgovs@gmail.com"
                                           (:network-server . "talk.google.com")
                                           (:connection-type . ssl))
                                          )))
;; SMTP
(setq send-mail-function 'smtpmail-send-it
      message-send-mail-function 'smtpmail-send-it
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
      smtpmail-auth-credentials '(("smtp.gmail.com" 587 "dolgovs@gmail.com" nil))
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-debug-info t)

;; Mail address
(setq user-mail-address "dolgovs@gmail.com")
(setq user-full-name "Sergei Dolgov")

;; message
(setq message-directory "~/mail/")

;; highlight-symbol
(with-library 'highlight-symbol
              (global-set-key [(control f3)] 'highlight-symbol-at-point)
              (global-set-key [(control f4)] 'highlight-symbol-remove-all)
              (global-set-key (kbd "C-x *") 'highlight-symbol-prev)
              (global-set-key (kbd "C-*") 'highlight-symbol-next)
              (setq highlight-symbol-on-navigation-p t))

;; browse-kill-ring
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))
(global-set-key "\C-cy" '(lambda ()
                           (interactive)
                           (popup-menu 'yank-menu)))
;; other-window
(global-set-key "\M-`" 'other-window)

;; yes-or-no
(defalias 'yes-or-no-p 'y-or-n-p)

;; menu-bar
(menu-bar-mode -1)

;; scroll-bar
(scroll-bar-mode nil)

;; tool-bar
(tool-bar-mode -1)

;; column-number
(column-number-mode t)

;; don't jump around, esp. in shell mode
(setq split-window-keep-point t)

;; https://gist.github.com/r0man/emacs-starter-kit/raw/personalizations/roman.el
(defun duplicate-current-line-or-region (arg)
  "Duplicates the current line or region ARG times.
If there's no region, the current line will be duplicated. However, if
there's a region, all lines that region covers will be duplicated."
  (interactive "p")
  (let (beg end (origin (point)))
    (if (and mark-active (> (point) (mark)))
        (exchange-point-and-mark))
    (setq beg (line-beginning-position))
    (if mark-active
        (exchange-point-and-mark))
    (setq end (line-end-position))
    (let ((region (buffer-substring-no-properties beg end)))
      (dotimes (i arg)
        (goto-char end)
        (newline)
        (insert region)
        (setq end (point)))
      (goto-char (+ origin (* (length region) arg) arg)))))
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Enable cut-and-paste between Emacs and X clipboard.
(setq x-select-enable-clipboard t)

;; Add a final newline when saving.
(setq require-final-newline t)

;; project-root
(with-library 'project-root
              (setq project-roots
                    '(("Generic Git Project" :root-contains-files (".git"))
                      ("Generic Mercurial Project" :root-contains-files (".hg"))))
              (global-set-key (kbd "C-c p f") 'project-root-find-file)
              (global-set-key (kbd "C-c p g") 'project-root-grep)
              (global-set-key (kbd "C-c p a") 'project-root-ack)
              (global-set-key (kbd "C-c p d") 'project-root-goto-root)
              (global-set-key (kbd "C-c p l") 'project-root-browse-seen-projects)
              (global-set-key (kbd "C-c p s")
                              (lambda () (interactive)
                                (with-project-root
                                    (shell (concat (car project-details) "-shell"))))))

;; nxml
(setq nxml-child-indent 4)

;; smex
(with-library 'smex
              (smex-initialize)
              (setq smex-save-file "~/.emacs.d/smex.save")
              (global-set-key (kbd "M-x") 'smex)
              (global-set-key (kbd "M-X") 'smex-major-mode-commands)
              ;; This is your old M-x.
              (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; http://jblevins.org/projects/deft/
(with-library 'deft
              (setq
               deft-extension "org"
               deft-directory "~/txt/deft/"
               deft-text-mode 'org-mode)
              (global-set-key (kbd "<f9>") 'deft))

;; git-commit
(with-library 'git-commit
              (add-hook 'git-commit-mode-hook 'turn-on-flyspell))

;; append and prepend asterisks to dired buffer names
;; this way they are filtered out from ido-switch-buffer
(add-hook 'dired-after-readin-hook
          (lambda ()
            (unless (string-match "^\\*" (buffer-name))
              (rename-buffer
               (generate-new-buffer-name
                (concat "*" (buffer-name) "*"))))))

;; markdown
(with-library 'markdown-mode)

;; minimap
(with-library 'minimap)

(set-register ?i '(file . "~/.emacs.d/init.el"))

;; org-publish
(setq org-publish-project-alist
      '(("mebubo.github.com"
         :base-directory "~/j/mebubo.github.com/org/"
         :base-extension "org"
         :publishing-directory "~/j/mebubo.github.com/_posts/"
         :recursive t
         :publishing-function org-publish-org-to-html
         :headline-levels 4
         :html-extension "html"
         :body-only t)))

;; expand-region
(with-library 'expand-region
              (global-set-key (kbd "C-=") 'er/expand-region))

;; mu4e
(setq
 mu4e-maildir "~/mail"
 mu4e-sent-folder "/sent"
 mu4e-drafts-folder "/drafts"
 mu4e-trash-folder "/trash"
 mu4e-refile-folder "/archive"
 mu4e-get-mail-command "true"
 mu4e-update-interval 300
 mu4e-use-fancy-chars t
)

;; erc
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
