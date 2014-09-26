(define-prefix-command 'tnt-outline-map)
(define-key evil-normal-state-map (kbd "SPC") 'tnt-outline-map)
(define-key tnt-outline-map (kbd "SPC") 'org-cycle)
; go to current heading's next sibling.
(define-key tnt-outline-map "j" 'org-forward-heading-same-level)
; go to current heading's previous sibling.
(define-key tnt-outline-map "k" 'org-backward-heading-same-level)
; go to first heading of a lower level than the current.
(define-key tnt-outline-map "l" 'outline-next-visible-heading)

(defun tnt-outline-last () (interactive)
  (when (string-match (rx-to-string '(: bos "*") t)
                      (thing-at-point 'line))
    (tnt-outline-up))
  (org-end-of-subtree))

; go to current subtree's last open item.
(define-key tnt-outline-map "e" 'tnt-outline-last)
; go to next subtree's heading.
(define-key tnt-outline-map "n" (lambda () (interactive)
                                  (tnt-outline-last)
                                  (outline-next-visible-heading 1)))

(defun tnt-outline-up () (interactive)
  (or (and (string-match (rx-to-string '(: bos "*") t)
                         (thing-at-point 'line))
           (outline-up-heading 1))
      (outline-previous-visible-heading 1)))

; go up (to current subtree's heading.)
(define-key tnt-outline-map "h" 'tnt-outline-up)
; go up and close heading.
(define-key tnt-outline-map "c" (lambda () (interactive)
                                  (tnt-outline-up)
                                  (org-cycle)))

(define-prefix-command 'tnt-move-map)
(define-key tnt-outline-map "m" 'tnt-move-map)
; move current fold down one position.
(define-key tnt-move-map "j" 'org-move-subtree-down)
; move current fold up one position.
(define-key tnt-move-map "k" 'org-move-subtree-up)

(key-chord-define tnt-outline-map "<<" 'org-promote-subtree)
(key-chord-define tnt-outline-map ">>" 'org-demote-subtree)

(defun my-org-binds ()
  ;(define-key evil-insert-state-map (kbd "SPC") )
  (define-key evil-normal-state-map "\C-m" 'org-open-at-point)
  (define-key org-mode-map "\C-t\C-lw" 'my-org-insert-link)
  (define-key org-mode-map "\C-n" 'org-next-link)
  (define-key org-mode-map "\C-p" 'org-previous-link))
(add-hook 'org-load-hook (lambda () (my-org-binds)))

(define-key evil-insert-state-map (kbd "C-&") (lambda () (interactive)
                                                (evil-backward-char)
                                                (evil-jump-item)))

(require 'org-collector)
(require 'org-wc)
(setq org-footnote-auto-label t)

(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d)")
        (sequence "ADMIN(a)" "|" "ADDED")))

(defun tnt2org-date ()
  (interactive)
  (let* ((epoch (thing-at-point 'symbol)))
    (message "%s"
             (shell-command-to-string (concat
                                       "date --d @"
                                       (substring epoch 2 -5)
                                       " -u +\"<%F %a %T>\"")))))

(defun my-org-insert-link ()
  "Insert org link where default description is set to html title."
  (interactive)
  (let* ((url (thing-at-point 'line))
         (title (get-html-title-from-url url)))
    (org-insert-link nil url title)))

(require 'mm-url) ; to include mm-url-decode-entities-string

(defun get-html-title-from-url (url)
  "Return content in <title> tag."
  (let (x1 x2 (download-buffer (url-retrieve-synchronously url)))
    (save-excursion
      (set-buffer download-buffer)
      (beginning-of-buffer)
      (setq x1 (search-forward "<title>"))
      (search-forward "</title>")
      (setq x2 (search-backward "<"))
      (mm-url-decode-entities-string (buffer-substring-no-properties x1 x2)))))
