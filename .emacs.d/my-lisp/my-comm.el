;; mail-client
(setq mail-user-agent 'gnus-user-agent)

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

;; mu4e
(setq mu4e-maildir "~/mail"
      mu4e-sent-folder "/sent"
      mu4e-drafts-folder "/drafts"
      mu4e-trash-folder "/trash"
      mu4e-refile-folder "/archive"
      mu4e-get-mail-command "true"
      mu4e-update-interval 300
      mu4e-use-fancy-chars t
      mu4e-html2text-command "html2text -utf8 -width 72")

;; erc
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(add-hook 'erc-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))
