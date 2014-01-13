(projectile-global-mode)
(setq projectile-use-git-grep t)

(defun dmenu-complete (prompt choices)
  (interactive)
  (shell-command "cd ~/hol && git checkout src/main/webapp/public/assets")
  (let* ((stripped-choices
          (-uniq (--map (file-name-nondirectory it) choices)))
         (choice
          (ido-completing-read prompt stripped-choices))
         (matching-files
          (-filter
           (lambda (file)
             (equal (file-name-nondirectory file) choice))
           choices)))
    (if (> (length matching-files) 1)
        (ido-completing-read prompt matching-files)
      (car matching-files))))

;(setq projectile-completion-system 'dmenu-complete)

;(let ((projectile-file (expand-file-name "~/.emacs.d/projectile-bookmarks.eld")))
;  (delete-file projectile-file)
;  (append-to-file "(" nil projectile-file)
;  (dolist (d '("~/leak" "~/goldfeld" "~/void" "~/longstorm"))
;    (dolist (p (directory-files (expand-file-name d)))
;      (when (and (not (string= p ".")) (not (string= p ".."))
;    (file-directory-p (concat d "/" p)))
; (append-to-file (concat "\"" d "/" p "\" ") nil projectile-file))))
; (append-to-file ")" nil projectile-file))
