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
;; --iswitchb--
(iswitchb-mode t)
;; --color-theme--
(require 'color-theme)
(color-theme-dark-laptop)
;; --desktop--
(desktop-save-mode 1)
(setq desktop-buffers-not-to-save
      (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
              "\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb" 
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
(require 'pymacs)
(pymacs-load "ropemacs" "rope-")
(setq ropemacs-enable-autoimport 't)
;; --yasnippet--
(require 'yasnippet-bundle)
;; Emacs 23: bundled EasyPG
(require 'epa)
;;(epa-file-enable)
;; --windmove--
(windmove-default-keybindings 'meta)
;; --tramp--
;(require 'tramp)
(setq tramp-default-method "ssh")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(load-home-init-file t t))

