;; mail-client
(setq mail-user-agent 'gnus-user-agent)

;; SMTP
(setq send-mail-function 'sendmail-send-it)

;; Mail address
(setq user-mail-address "dolgovs@gmail.com")
(setq user-full-name "Sergei Dolgov")

;; message
(setq message-directory "~/mail/")

;; mu4e

(with-library 'mu4e
              (setq mu4e-maildir "~/mail"
                    mu4e-sent-folder "/mu4e/sent"
                    mu4e-drafts-folder "/mu4e/drafts"
                    mu4e-trash-folder "/mu4e/trash"
                    mu4e-refile-folder "/mu4e/archive"
                    mu4e-get-mail-command "true"
                    mu4e-update-interval 300
                    mu4e-use-fancy-chars t
                    mu4e-sent-messages-behavior 'delete
                    mu4e-headers-visible-lines 15
                    mu4e-html2text-command "html2text -utf8 -width 72"))
(with-library 'org-mu4e)

;; erc
(setq erc-hide-list '("JOIN" "PART" "QUIT"))
(add-hook 'erc-mode-hook
          (lambda ()
            (setq show-trailing-whitespace nil)))
