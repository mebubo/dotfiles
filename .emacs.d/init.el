(defmacro with-library (symbol &rest body)
  `(when (require ,symbol nil t)
     ,@body))

(set-register ?i '(file . "~/.emacs.d/init.el"))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(set-frame-font "monospace-10")

(with-library 'package
              (add-to-list 'package-archives
                           '("melpa" . "https://melpa.org/packages/") t)
              (package-initialize)

              (defvar my-packages '(
                                    better-defaults
                                    evil
                                    back-button
                                    expand-region
                                    flycheck
                                    guide-key
                                    haskell-mode
                                    helm
                                    highlight-symbol
                                    whole-line-or-region
                                    intero
                                    ))

              (defun sd-install-packages()
                (interactive)
                (unless package-archive-contents
                  (package-refresh-contents))
                (dolist (p my-packages)
                  (unless (package-installed-p p)
                    (package-install p)))))

(with-library 'better-defaults
              (ido-mode -1))

(setq inhibit-startup-message t
      visible-bell 1
      kill-whole-line t
      frame-title-format "emacs: %b %f")

(defalias 'yes-or-no-p 'y-or-n-p)

(global-set-key [C-tab] 'other-window)

(blink-cursor-mode 0)

(column-number-mode 1)

(global-set-key (kbd "C-z") nil)

(savehist-mode 1)

(server-start)

;; (desktop-save-mode 1)

(ido-mode 'buffers)
(global-set-key (kbd "M-l") 'ido-switch-buffer)
(add-to-list 'ido-ignore-buffers "^\*")

(setq winner-dont-bind-my-keys t)
(winner-mode 1)

(with-library 'flycheck
              (add-hook 'after-init-hook 'global-flycheck-mode))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(with-library 'whole-line-or-region
              (add-to-list 'whole-line-or-region-extensions-alist
                           '(comment-dwim whole-line-or-region-comment-dwim nil))
              (whole-line-or-region-mode))

(with-library 'helm
              (require 'helm-config)
              (global-set-key (kbd "M-x") 'helm-M-x)
              (global-set-key (kbd "C-x b")   'helm-mini)
              (global-set-key (kbd "C-x C-b") 'helm-buffers-list)
              (global-set-key (kbd "C-x C-r") 'helm-recentf)
              (global-set-key (kbd "M-y") 'helm-show-kill-ring)
              (setq helm-split-window-in-side-p t)
              )

(with-library 'org
              (global-set-key (kbd "C-c l") 'org-store-link)
              (global-set-key (kbd "C-c c") 'org-capture)
              (global-set-key (kbd "C-c a") 'org-agenda)
              (setq org-directory "~/dev/txt/"
                    org-default-notes-file (concat org-directory "refile.org")))

(with-library 'highlight-symbol
              (global-set-key (kbd "C-<f3>") 'highlight-symbol-at-point)
              (global-set-key (kbd "C-<f4>") 'highlight-symbol-remove-all)
              (global-set-key (kbd "C-x *") 'highlight-symbol-prev)
              (global-set-key (kbd "C-*") 'highlight-symbol-next)
              (setq highlight-symbol-on-navigation-p t))

(with-library 'expand-region
              (global-set-key (kbd "C-=") 'er/expand-region))

(with-library 'back-button
              (back-button-mode 1))

(with-library 'evil
	      (evil-mode 1))

(with-library 'guide-key
              (setq guide-key/guide-key-sequence '("C-c" "C-x"))
              (setq guide-key/recursive-key-sequence-flag t)
              (guide-key-mode 1))

(add-hook 'haskell-mode-hook 'intero-mode)
