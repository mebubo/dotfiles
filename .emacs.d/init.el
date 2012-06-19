;; -*- mode: emacs-lisp -*-

;; load plugins from ~/.emacs.d/plugins
(setq load-path (cons "~/.emacs.d/plugins" load-path))

;; make customize use it's own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

;; font
(set-frame-font "monospace-10")

;; for safely loading libraries
;; http://www.emacswiki.org/emacs/LoadingLispFiles
(defmacro with-library (symbol &rest body)
  `(when (require ,symbol nil t)
     ,@body))

(with-library 'package
              (add-to-list 'package-archives
                           '("marmalade" . "http://marmalade-repo.org/packages/") t)
              (package-initialize)

              (when (not package-archive-contents)
                (package-refresh-contents))

              ;; Add in your own as you wish:
              (defvar my-packages '(smex markdown-mode git-commit expand-region)
                "A list of packages to ensure are installed at launch.")

              (dolist (p my-packages)
                (when (not (package-installed-p p))
                  (package-install p))))


;; no-splash-screen
(setq inhibit-startup-message t)

;; don't stop on C-z
(global-set-key "\C-z" nil)

;; select buffer
; (require 'bs)
; (global-set-key "\C-x\C-b" 'bs-show)
; (add-to-list 'bs-configurations
;              '("dired" nil nil nil
;                (lambda (buf)
;                  (with-current-buffer buf
;                    (not (eq major-mode 'dired-mode)))) nil))
; (add-to-list 'bs-configurations
;              '("rcirc" nil nil nil
;                (lambda (buf)
;                  (with-current-buffer buf
;                    (not (eq major-mode 'rcirc-mode))))
;                rcirc-sort-buffers))


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
;; Display ido results vertically, rather than horizontally
;(setq ido-decorations (quote ("\n-> " "" "\n   " "\n   ..." "[" "]"
;                              " [No match]" " [Matched]" " [Not readable]"
;                              " [Too big]" " [Confirm]")))
;(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
;(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)
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
(recentf-mode t)
(defun recentf-open-files-compl ()
  (interactive)
  (let* ((tocpl (mapcar (lambda (x) (cons (file-name-nondirectory x) x))
                        recentf-list))
         (fname (completing-read "File name: " tocpl nil nil)))
    (when fname
      (find-file (cdr (assoc-string fname tocpl))))))
(global-set-key "\C-x\C-r" 'recentf-open-files-compl)

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
;(org-remember-insinuate)
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

; (add-hook 'rcirc-mode-hook
;           (lambda ()
;             (define-key rcirc-mode-map (kbd "C-x C-b")
;               (lambda ()
;                 (interactive)
;                 (bs--show-with-configuration "rcirc")))))

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
                                           ;;(:port . 443)
                                           (:connection-type . ssl))
                                          ;;"sd208054@im.sun.com")
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

;; disable arrow keys
;; (global-unset-key [(up)])
;; (global-unset-key [(down)])
;; (global-unset-key [(left)])
;; (global-unset-key [(right)])
;; (global-unset-key [(prior)])
;; (global-unset-key [(next)])
;; (global-unset-key [(home)])
;; (global-unset-key [(end)])

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

;; auto-config
(with-library 'auto-complete-config
              (add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
              (setq ac-comphist-file  "~/.emacs.d/ac-comphist.dat")
              (ac-config-default)
              (ac-define-source buffer-viewport
                                '((candidates . buffer-viewport-to-list)
                                  (symbol . "Q")))

              (add-hook 'shell-mode-hook
                        (lambda ()
                          (setq ac-sources '(ac-source-buffer-viewport))))

              )

;; column-number
(column-number-mode t)

;; my stuff
(defun insert-last-command-line-argument ()
  ""
  (interactive)
  (let ((beg) (end))
    (save-excursion
      (beginning-of-line)
      (comint-previous-prompt 1)
      (end-of-line)
      (setq end (point))
      (backward-sexp)
      (setq beg (point)))
    (insert (filter-buffer-substring beg end)))
)

;; comint
(defvar comint-history-file "~/.emacs.d/comint-history")
(defun save-comint-history (str)
  (with-current-buffer (find-file-noselect comint-history-file)
    (goto-char (point-max))
    (if (not (string-match "\\`\\s *\\'" str))
        (progn (insert str)
               (basic-save-buffer)))))
(add-hook 'comint-input-filter-functions 'save-comint-history)

(defun buffer-to-list (name)
  "Takes buffer NAME, returns the contents of tha buffer as a list of strings"
  (with-current-buffer name
    (save-excursion
      (let ((l '())
            (max-line (line-number-at-pos (point-max))))
        (goto-char (point-min))
        (while (not (eq max-line (line-number-at-pos)))
          (add-to-list 'l (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position)))
          (forward-line))
        l))))

(require 'cl)

(defun remove-duplicates-str (list)
  (delete "" (remove-duplicates list :test 'equal :from-end t)))

(defun ido-complete-comint-history ()
  (interactive)
  (insert
   (ido-completing-read "comint-history: "
                        (remove-duplicates-str
                         (buffer-to-list
                         (find-file-noselect comint-history-file))))))
(add-hook 'shell-mode-hook
          (lambda ()
            (local-set-key (kbd "M-s") 'ido-complete-comint-history)))

(defun buffer-viewport-to-list ()
  (save-excursion
    (let ((beg (progn (move-to-window-line 0)
                      (point)))
          (end (progn (move-to-window-line -1)
                      (move-end-of-line 1)
                      (point))))
      (split-string (buffer-substring-no-properties beg end)))))

(defun ido-complete-buffer-viewport ()
  (interactive)
  (insert
   (ido-completing-read "buffer-viewport: "
                        (remove-duplicates-str
                         (buffer-viewport-to-list)))))

(global-set-key (kbd "C-c i") 'ido-complete-buffer-viewport)

(defun insert-from-viewport ()
  (interactive)
  (insert
   (save-excursion
     (move-to-window-line 0)
     (isearch-forward)
     (let ((beg (save-excursion
                   (search-backward " ")
                   (point)))
            (end (save-excursion
                   (search-forward " ")
                   (point))))
       (buffer-substring-no-properties beg end)))))

(global-set-key (kbd "C-c I") 'insert-from-viewport)

;; shell-mode
(add-hook 'shell-mode-hook (lambda () (setq show-trailing-whitespace nil)))

(defun surround-with-asterisks (string)
  (unless (equal (substring string 0 1) "*")
    (setq string (concat "*" string)))
  (unless (equal (substring string -1) "*")
    (setq string (concat string "*")))
  string)

(defun new-shell ()
  "Start new shell, prompting for buffer name and uniquifying if necessary"
  (interactive)
  (shell
   (generate-new-buffer-name
    (surround-with-asterisks
     (read-from-minibuffer "New shell name: " "shell")))))
(global-set-key [f5] 'new-shell)

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

;; Use IDO fro comint history
;; See: http://paste.lisp.org/display/37129 (modified to remove duplicate)
(defun ido-complete-comint-input-ring ()
  "Fetch a previous element from history using ido-like completion.
This function searches *all* elements in history, not just
previous or next elements like Comint's normal completion.
So you can bind it to both M-r and M-s."
  (interactive)
  (unless (null comint-input-ring)
    (let* ((elt (ido-completing-read "History: " (delete "" (remove-duplicates (cddr (ring-elements comint-input-ring)) :test #'string=)) nil t))
           (pos (comint-previous-matching-input-string-position
                 (regexp-quote elt) 1)))
      (unless (null pos)
        (setq comint-input-ring-index pos)
        (message "History item: %d" (1+ pos))
        (comint-delete-input)
        (insert (ring-ref comint-input-ring pos))))))
(add-hook 'shell-mode-hook (lambda () (local-set-key (kbd "M-r") 'ido-complete-comint-input-ring)))

;; Enable cut-and-paste between Emacs and X clipboard.
(setq x-select-enable-clipboard t)

;; Add a final newline when saving.
(setq require-final-newline t)

;;; COMINT MODE
(custom-set-variables
; '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
; '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
; '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 )

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

;; partial-completion-mode
;(partial-completion-mode t)

;; smex
(with-library 'smex
              (smex-initialize)
              (setq smex-save-file "~/.emacs.d/smex.save")
              (global-set-key (kbd "M-x") 'smex)
              (global-set-key (kbd "M-X") 'smex-major-mode-commands)
              ;; This is your old M-x.
              (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

;; electric in emacs 24
;(electric-pair-mode)
;(electric-indent-mode)
;(electric-layout-mode)

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

;; eshell
(add-hook 'eshell-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(defun kai-eshell-insert-last-word (n)
  (interactive "p")
  (insert (car (reverse
                (split-string
                 (eshell-previous-input-string (- n 1)))))))

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