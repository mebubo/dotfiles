;; --require--
;;(require 'doremi-cmd)
(require 'mercurial)
(require 'xcscope)
;; --keys--
(global-set-key "\C-z" nil)
(global-set-key "\C-x\C-b" 'bs-show)
(global-set-key (kbd "\e\em") 'compile)
(define-key ctl-x-map "\C-q" 'view-mode)
;; -vars--
;;(tool-bar-mode nil)
(setq transient-mark-mode nil)
(setq view-read-only t)
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
(setq iswitchb-buffer-ignore '("^ " "^ "))
;; --color-theme--
(require 'color-theme)
(color-theme-dark-laptop)
;; --desktop--
(setq desktop-save-mode t)
(desktop-read)
;; --server--
(server-start)
;; --misc--
(put 'upcase-region 'disabled nil)
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
;; --paren--
(show-paren-mode)
;; --reftex--
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;; --AUCTeX--
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(global-set-key "\C-c\C-z" 'TeX-insert-dollar)
;; --uniquify--
(require 'uniquify)
;; --indentation--
;;(setq-default indent-tabs-mode nil)
;;(setq-default c-basic-offset 3)
;; --actions--
;;(split-window-horizontally)

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(case-fold-search t)
 '(load-home-init-file t t)
 '(uniquify-buffer-name-style (quote reverse) nil (uniquify)))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:family "Terminus")))))

(put 'downcase-region 'disabled nil)
