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
                      ; movement
                      smooth-scrolling
                      ; project & file mgmt
                      projectile
                      flx-ido
                      magit
                      ; lisp
                      paredit
                      paredit-everywhere
                      rainbow-delimiters
                      ; js
                      js2-mode
                      kite
                      ; clojure
                      clojure-mode
                      clojure-test-mode
                      cider
                      ac-nrepl
                      ; general workflow
                      column-enforce-mode
                      auto-complete
                      flycheck
                      ; misc
                      erc-hl-nicks
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
