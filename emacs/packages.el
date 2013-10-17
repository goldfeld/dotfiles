(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar my-packages '(evil
                      evil-leader
                      org
                      auto-complete
		      ; project & file mgmt
                      projectile
                      flx-ido
                      helm
                      helm-projectile
                      magit
		      ; lisp
                      paredit
                      rainbow-delimiters
		      ; clojure
                      clojure-mode
                      clojure-test-mode
                      nrepl
                      ac-nrepl
		      ; color theming
                      load-theme-buffer-local
                      solarized-theme
                      zenburn-theme
                      soothe-theme
                      purple-haze-theme
                      cyberpunk-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
