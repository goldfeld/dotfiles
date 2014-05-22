(defun alt-buffer () (other-buffer (current-buffer) 1))
(defun switch-to-alt-buffer () (interactive) (switch-to-buffer (alt-buffer)))

(define-key evil-normal-state-map "m" nil)
(define-key evil-motion-state-map "mm" 'evil-ex)
(define-key evil-normal-state-map "mw" (kbd ":w"))
(define-key evil-normal-state-map "mb" (lambda () (interactive)
                                         (switch-to-alt-buffer)
                                         (kill-buffer (alt-buffer))))

;nnoremap <silent> mb :w<CR>:execute "keepalt b#\\| bdelete" bufnr('%')<CR>
(define-key evil-normal-state-map "mq" (kbd ":q"))
(define-key evil-normal-state-map "mv" (kbd ":vsplit"))
(define-key evil-normal-state-map "mz" (kbd ":split"))
(define-key evil-normal-state-map (kbd "m SPC")
(define-key evil-normal-state-map "mo" (kbd "O"))
  (lambda () (interactive)
    (shell-command "cd ~/longstorm/reptile && ./run")))

(define-key evil-normal-state-map "mj"
  (lambda () (interactive)
    (org-transpose-paragraphs 1)))

(define-key evil-normal-state-map "mk"
  (lambda () (interactive)
    (org-transpose-paragraphs -1)))

(defun move-line (arg)
  (evil-delete-line (line-beginning-position) (line-end-position))
  (forward-line arg)
  (evil-paste-before 1)
  (evil-delete-line (+ 1 (point)) (line-end-position))
  (forward-line (- 0 arg))
  (thing-at-point 'line)
  (evil-paste-after 1)
  (forward-line arg))

(defun org-transpose-paragraphs (arg )
 (if (and (not (or (string= "org-mode" major-mode)
                (org-at-table-p) (org-on-heading-p) (org-at-item-p)))
          (thing-at-point 'sentence))
   (progn
     (transpose-paragraphs arg)
     (backward-paragraph)
     (re-search-forward "[[:graph:]]")
     (goto-char (match-beginning 0))
     t)
   (move-line arg)))
