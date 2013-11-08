(add-hook 'org-load-hook
	   (lambda ()
	     (define-key org-mode-map "\C-n" 'org-next-link)
	     (define-key org-mode-map "\C-p" 'org-previous-link)))
