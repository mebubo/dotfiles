(set-frame-font "monospace-10")

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

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

(server-start)

(defun kai-eshell-insert-last-word (n)
  (interactive "p")
  (insert (car (reverse
                (split-string
                 (eshell-previous-input-string (- n 1)))))))

(setq multi-eshell-shell-function '(eshell)
      multi-eshell-name "*eshell*")
