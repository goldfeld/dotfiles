(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar my-packages '(evil
                      evil-leader
                      evil-matchit
                      org
                      auto-complete
                      kite
                      flycheck
                      ; project & file mgmt
                      projectile
                      flx-ido
                      magit
                      ; lisp
                      paredit
                      paredit-everywhere
                      rainbow-delimiters
                      ; clojure
                      clojure-mode
                      clojure-test-mode
                      cider
                      ac-nrepl
                      ; color theming
                      load-theme-buffer-local
                      solarized-theme
                      zenburn-theme
                      soothe-theme
                      purple-haze-theme
                      cyberpunk-theme
                      bubbleberry-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))
