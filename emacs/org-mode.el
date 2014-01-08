(add-hook 'org-load-hook
	   (lambda ()
	     (define-key org-mode-map "\C-t\C-lb" 'org-open-at-point)
	     (define-key org-mode-map "\C-t\C-lw" 'my-org-insert-link)
	     (define-key org-mode-map "\C-n" 'org-next-link)
	     (define-key org-mode-map "\C-p" 'org-previous-link)))

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