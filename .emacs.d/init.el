;; -*- mode: emacs-lisp -*-

;; for safely loading libraries
;; http://www.emacswiki.org/emacs/LoadingLispFiles
(defmacro with-library (symbol &rest body)
  `(when (require ,symbol nil t)
     ,@body))

(add-to-list 'load-path "~/.emacs.d/site-lisp")

(add-to-list 'load-path "~/.emacs.d/my-lisp")
(load-library "my-shell")
(load-library "my-comm")

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

              (defvar my-packages '(smex
                                    markdown-mode
                                    git-commit
                                    expand-region
                                    highlight-symbol
                                    browse-kill-ring
                                    back-button
                                    magit
                                    ))

              (dolist (p my-packages)
                (when (not (package-installed-p p))
                  (package-install p))))

;; no-splash-screen
(setq inhibit-startup-message t)

;; don't stop on C-z
(global-set-key (kbd "C-z") nil)

;; ibuffer
(global-set-key (kbd "C-x C-b") 'ibuffer)
;; Switching to ibuffer puts the cursor on the most recent buffer
(defadvice ibuffer (around ibuffer-point-to-most-recent) ()
  "Open ibuffer with cursor pointed to most recent buffer name"
  (let ((recent-buffer-name (buffer-name)))
    ad-do-it
    (ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;; ido
(ido-mode 'buffers)
(add-to-list 'ido-ignore-buffers "^\*")
(setq ido-enable-flex-matching t)

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
(global-set-key (kbd "C-s-<left>") 'winner-undo)
(global-set-key (kbd "C-s-<right>") 'winner-redo)
(winner-mode t)

;; paren
(show-paren-mode)

;; bell
(setq visible-bell 'top-bottom)

;; backups
(setq backup-by-copying t
      backup-directory-alist
      '(("." . "~/.emacs.d/backups"))
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; reftex
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

;; AUCTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(global-set-key (kbd "C-c C-z") 'TeX-insert-dollar)

;; uniquify buffer names
(require 'uniquify)
(setq uniquify-buffer-names t
      uniquify-buffer-name-style 'reverse)

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
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c b") 'org-iswitchb)
(setq org-log-done t)
(setq org-export-html-postamble nil
      org-export-html-preamble nil)

;; mouse-select-buffer
(msb-mode)

;; yasnippet
(with-library 'yasnippet-bundle)

;; EasyPG
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

;; highlight-symbol
(with-library 'highlight-symbol
              (global-set-key (kbd "C-<f3>") 'highlight-symbol-at-point)
              (global-set-key (kbd "C-<f4>") 'highlight-symbol-remove-all)
              (global-set-key (kbd "C-x *") 'highlight-symbol-prev)
              (global-set-key (kbd "C-*") 'highlight-symbol-next)
              (setq highlight-symbol-on-navigation-p t))

;; browse-kill-ring
(when (require 'browse-kill-ring nil 'noerror)
  (browse-kill-ring-default-keybindings))
(global-set-key (kbd "C-c y") '(lambda ()
                                 (interactive)
                                 (popup-menu 'yank-menu)))
;; other-window
(global-set-key (kbd "M-`") 'other-window)

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

(set-register ?i '(file . "~/.emacs.d/init.el"))

;; expand-region
(with-library 'expand-region
              (global-set-key (kbd "C-=") 'er/expand-region))

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "e" 'wdired-change-to-wdired-mode)
             (define-key dired-mode-map "/" 'dired-isearch-filenames)))

(with-library 'back-button
              (back-button-mode 1))

;; zap-up-to-char http://irreal.org/blog/?p=1536
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)
