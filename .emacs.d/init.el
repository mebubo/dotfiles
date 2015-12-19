(defmacro with-library (symbol &rest body)
  `(when (require ,symbol nil t)
     ,@body))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(set-frame-font "monospace-10")

(with-library 'package
              (add-to-list 'package-archives
                           '("melpa-stable" . "http://stable.melpa.org/packages/") t)
              (package-initialize)

              (defvar my-packages '(
				    better-defaults
                                    ;; evil
                                    back-button
                                    company
                                    company-ghc
                                    expand-region
                                    flycheck
                                    flycheck-haskell
                                    ghc
                                    guide-key
                                    haskell-mode
                                    helm
                                    highlight-symbol
                                    magit
                                    markdown-mode
                                    whole-line-or-region
                                    yaml-mode
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
      kill-whole-line t)

(blink-cursor-mode 0)

(global-set-key (kbd "C-z") nil)

(ido-mode 'buffers)
(global-set-key (kbd "M-l") 'ido-switch-buffer)
(add-to-list 'ido-ignore-buffers "^\*")

(with-library 'helm
              (require 'helm-config))

(savehist-mode 1)

(server-start)

(setq winner-dont-bind-my-keys t)
(winner-mode 1)

(with-library 'flycheck
              (add-hook 'after-init-hook 'global-flycheck-mode))

(with-library 'company
              (add-hook 'after-init-hook 'global-company-mode))

(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "google-chrome")

(with-library 'whole-line-or-region
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

(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c a") 'org-agenda)
(setq org-directory "~/dev/txt/"
      org-default-notes-file (concat org-directory "refile.org"))


(with-library 'haskell-mode
              (let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
                (setenv "PATH" (concat my-cabal-path ":" (getenv "PATH")))
                (add-to-list 'exec-path my-cabal-path))
              (add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
              (custom-set-variables '(haskell-tags-on-save t))
              (with-library 'company
                            (add-to-list 'company-backends 'company-ghc)
                            (custom-set-variables '(company-ghc-show-info t)))
              (with-library 'ghc
                            (autoload 'ghc-init "ghc" nil t)
                            (autoload 'ghc-debug "ghc" nil t)
                            (add-hook 'haskell-mode-hook (lambda () (ghc-init)))
                            )
              )

