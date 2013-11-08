(projectile-global-mode)

(setq projectile-globally-ignored-directories
      (append '("crossover-cljs") projectile-globally-ignored-directories))

(setq projectile-globally-ignored-files
      (append '("*~" "#*#" "*.swp" "*.swo") projectile-globally-ignored-files))

;(let ((projectile-file (expand-file-name "~/.emacs.d/projectile-bookmarks.eld")))
;  (delete-file projectile-file)
;  (append-to-file "(" nil projectile-file)
;  (dolist (d '("~/leak" "~/goldfeld" "~/void" "~/longstorm"))
;    (dolist (p (directory-files (expand-file-name d)))
;      (when (and (not (string= p ".")) (not (string= p ".."))
;    (file-directory-p (concat d "/" p)))
; (append-to-file (concat "\"" d "/" p "\" ") nil projectile-file))))
; (append-to-file ")" nil projectile-file))
