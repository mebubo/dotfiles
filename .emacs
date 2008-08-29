(setq load-path (cons "~/usr/local/share/emacs/site-lisp" load-path))
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
;; --utf-8--
;(set-language-environment "UTF-8")
;(set-buffer-file-coding-system 'utf-8)
;; --iswitchb--
(iswitchb-mode t)
;; --color-theme--
(require 'color-theme)
(color-theme-dark-laptop)
;(require 'zenburn)
;(color-theme-zenburn)
;; --desktop--
(desktop-save-mode 1)
(setq desktop-buffers-not-to-save
      (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb" 
              "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
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
      '(("." . "~/.backup-emacs"))    ; don't litter my fs tree
      delete-old-versions nil
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
;; --totd--
(defun totd ()
  (interactive)
  (with-output-to-temp-buffer "*Tip of the day*"
    (let* ((commands (loop for s being the symbols
                           when (commandp s) collect s))
           (command (nth (random (length commands)) commands)))
      (princ
       (concat "Your tip for the day is:\n"
               "========================\n\n"
               (describe-function command)
               "\n\nInvoke with:\n\n"
               (with-temp-buffer
                 (where-is command t)
                 (buffer-string)))))))
;; --server-new-frame--
;; (add-hook 'server-switch-hook
;;          (lambda nil
;;            (let ((server-buf (current-buffer)))
;;              (bury-buffer)
;;              (switch-to-buffer-other-frame server-buf))))
(setq server-kill-new-buffers t)
;; (add-hook 'server-done-hook (lambda () (delete-frame)))
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
;; --muse--
(require 'muse-mode)                    ; load authoring mode
(require 'muse-html)                    ; load publishing styles I use
(require 'muse-latex)
(require 'muse-docbook)
;; --cscope-- 
(require 'xcscope)
(setq cscope-command-args "-q")
;; --ps-printing--
(setq ps-header-lines 1
      ps-line-number t
      ps-line-number-step 5
      ps-n-up-printing 2
      ps-paper-type (quote a4)
      ps-print-background-text (quote (("canburak" nil nil nil nil nil nil)))
      ps-print-color-p (quote black-white)
      ps-print-header-frame nil
      ps-zebra-stripes t)
;; --kernel-indentation--
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "linux")))
;; --indentation--
;(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)
;; --actions--
;(split-window-horizontally)
;; --erc--
(require 'erc)
(require 'easymenu)
    (easy-menu-add-item  nil '("tools")
      ["IRC" erc-select t])
(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
          '(("freenode.net" "#emacs" "#sympy" "#xmonad")
            ("irc.debian.org" "#debian")))
(add-hook 'erc-mode-hook
          '(lambda ()
             (require 'erc-pcomplete)
             (pcomplete-erc-setup)
             (erc-completion-mode 1)))
(require 'erc-fill)
(erc-fill-mode t)
(require 'erc-ring)
(erc-ring-mode t)
(require 'erc-netsplit)
(erc-netsplit-mode t)
(erc-timestamp-mode t)
(setq erc-timestamp-format "[%R-%m/%d]")
(erc-button-mode nil) ;slow
(setq erc-user-full-name "solka")
(setq erc-email-userid "solka")
(defun irc-maybe ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (erc :server "irc.freenode.net" :port 6667
                :nick "solka" :full-name "solka")
    (erc :server "irc.debian.org" :port 6667
                :nick "solka" :full-name "solka")))
;; --remember--
(define-key global-map (kbd "<f9> r") 'remember)
(define-key global-map (kbd "<f9> R") 'remember-region)
;; --org--
;(require 'org-install)
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
(setq dictionary-server "localhost")
(setq dictionary-default-dictionary "mueller7")
(setq dictionary-create-buttons nil)
(setq dictionary-use-single-buffer t)
(global-dictionary-tooltip-mode 1)
;(global-set-key "\C-cs" 'dictionary-search)
(global-set-key "\C-cm" 'dictionary-match-words)
(global-set-key "\C-cs" 'dictionary-lookup-definition)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(load-home-init-file t t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 ;'(variable-pitch ((t (:family "Terminus")))))
 '(variable-pitch ((t (:family "DejaVu Sans Mono")))))
