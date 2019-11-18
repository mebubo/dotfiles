(add-hook 'shell-mode-hook (lambda () (setq show-trailing-whitespace nil)))

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

(custom-set-variables
; '(comint-scroll-to-bottom-on-input t)  ; always insert at the bottom
; '(comint-scroll-to-bottom-on-output t) ; always add output at the bottom
; '(comint-scroll-show-maximum-output t) ; scroll to show max possible output
 '(comint-completion-autolist t)        ; show completion list when ambiguous
 '(comint-input-ignoredups t)           ; no duplicates in command history
 '(comint-completion-addsuffix t)       ; insert space/slash after file completion
 )

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

;; eshell
(add-hook 'eshell-mode-hook (lambda () (setq show-trailing-whitespace nil)))
(defun kai-eshell-insert-last-word (n)
  (interactive "p")
  (insert (car (reverse
                (split-string
                 (eshell-previous-input-string (- n 1)))))))

