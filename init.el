(tool-bar-mode -1)
(menu-bar-mode -1)
(setq inhibit-splash-screen t)
(setq eshell-aliases-file (expand-file-name "~/.emacs.d/eshell/alias"))

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(defvar my-packages '(evil
                      evil-leader
                      org
                      projectile
                      flx-ido
                      helm
                      helm-projectile
                      magit
                      paredit
                      rainbow-delimiters
                      color-theme
                      clojure-mode
                      clojure-test-mode
                      nrepl
                      solarized-theme
                      zenburn-theme
                      soothe-theme
                      purple-haze-theme))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'helm-config)
(require 'rainbow-delimiters)
(require 'paredit)
(require 'projectile)

(projectile-global-mode)
(let ((projectile-file (expand-file-name "~/.emacs.d/projectile-bookmarks.eld")))
  (delete-file projectile-file)
  (append-to-file "(" nil projectile-file)
  (dolist (d '("~/leak" "~/goldfeld" "~/void" "~/longstorm"))
    (dolist (p (directory-files (expand-file-name d)))
      (when (and (not (string= p ".")) (not (string= p ".."))
		 (file-directory-p (concat d "/" p)))
	(append-to-file (concat "\"" d "/" p "\" ") nil projectile-file))))
  (append-to-file ")" nil projectile-file))

;(load-theme 'solarized-dark t)
;(load-theme 'zenburn t)
(load-theme 'purple-haze t)

(require 'evil)
(evil-mode t)

(require 'ido)
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
(setq ido-use-faces nil)
(setq ido-create-new-buffer 'always)
(setq ido-enable-tramp-completion nil)
(setq ido-enable-flex-matching t)

(require 'midnight)
(midnight-delay-set 'midnight-delay "8:00am")
(add-hook 'midnight-hook 'calendar)
; here's the place to create nightly build hooks

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)
(setq indent-tabs-mode nil)

(custom-set-faces
   '(my-tab-face            ((((class color)) (:background "grey10"))) t)
   '(my-trailing-space-face ((((class color)) (:background "gray10"))) t))

(add-hook 'font-lock-mode (function (lambda ()
    (setq font-lock-keywords
	  (append font-lock-keywords
		  '(("\t+" (0 'my-tab-face t))
		    ("^.\\{81,\\}$" (0 'my-long-line-face t))
		    ("[ \t]+$"      (0 'my-trailing-space-face t))))))))

(defun my-move-key (keymap-from keymap-to key)
  "Moves key binding from one keymap to another, deleting from the old location."
  (define-key keymap-to key (lookup-key keymap-from key))
  (define-key keymap-from key nil))
;; unbind <CR> and <Space> in evil so other modes can use them.
(my-move-key evil-motion-state-map evil-normal-state-map (kbd "RET"))
(my-move-key evil-motion-state-map evil-normal-state-map " ")

; (define-key evil-normal-state-map "c" nil)
; (define-key evil-motion-state-map "cu" 'universal-argument)

; (define-key key-translation-map (kbd "ch") (kbd "C-h"))
; (define-key key-translation-map (kbd "cx") (kbd "C-x"))

; https://github.com/emacsmirror/evil/blob/master/evil-maps.el

(define-key evil-insert-state-map "\C-h" (kbd "<backspace>"))
(define-key evil-ex-completion-map "\C-h" (kbd "<backspace>"))

(define-key evil-normal-state-map "\C-t" nil)
(define-key evil-normal-state-map "\C-t\C-h" 'ido-switch-buffer)
(define-key evil-normal-state-map "\C-t\C-n" 'projectile-find-file)
(define-key evil-normal-state-map "\C-t\C-c" 'projectile-switch-to-buffer)
(define-key evil-normal-state-map "\C-t\C-d" 'find-file)

(define-key evil-normal-state-map "\C-cf" 'org-footnote-action)

(defun alt-buffer () (other-buffer (current-buffer) 1))
(defun switch-to-alt-buffer () (interactive) (switch-to-buffer (alt-buffer)))

(define-key evil-motion-state-map "," nil)
(define-key evil-normal-state-map ",," 'switch-to-alt-buffer)
(define-key evil-normal-state-map ",.v" (kbd ":e ~/goldfeld/dotfiles/init.el"))

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
(define-key evil-normal-state-map "mo" (kbd "O"))
(define-key evil-normal-state-map "mj" (kbd ":m+"))
(define-key evil-normal-state-map "mk" (kbd "ddkP"))

(define-key evil-normal-state-map "gs" 'magit-status)

;(dolist (p '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j" "k" "l" "n" "p" "u" "x" "y"))
;  (define-key evil-normal-state-map (vconcat "m" p) (lambda () (evil-set-marker p 0))
;(define-key evil-motion-state-map (vconcat "z" [return]) "zt^")
;(define-key evil-motion-state-map (kbd "z RET") (vconcat "z" [return]))

;(define-key evil-normal-state-map "t" nil)
(define-key evil-normal-state-map "t\C-m" (lambda () (interactive) (shell-command "hooker 3")))

(add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
;(add-hook 'clojure-mode-hook 'rainbow-delimiters-mode)

(add-hook 'nrepl-interaction-mode-hook
          'nrepl-turn-on-eldoc-mode)
(setq nrepl-hide-special-buffers t)
(setq nrepl-popup-stacktraces nil)
(setq nrepl-popup-stacktraces-in-repl t)
(add-to-list 'same-window-buffer-names "*nrepl*")
(add-hook 'nrepl-mode-hook 'subword-mode)
(add-hook 'nrepl-mode-hook 'paredit-mode)
;; (add-hook 'nrepl-mode-hook 'smartparens-mode)
(add-hook 'nrepl-mode-hook 'rainbow-delimiters-mode)

;;; org-collector --- collect properties into tables
;; Copyright (C) 2008-2013 Free Software Foundation, Inc.

(defvar org-propview-default-value 0
  "Default value to insert into the propview table when the no
value is calculated either through lack of required variables for
a column, or through the generation of an error.")

(defun and-rest (list)
  (if (listp list)
      (if (> (length list) 1)
	  (and (car list) (and-rest (cdr list)))
	(car list))
    list))

(put 'org-collector-error
     'error-conditions
     '(error column-prop-error org-collector-error))

(defun org-dblock-write:propview (params)
  "collect the column specification from the #+cols line
preceeding the dblock, then update the contents of the dblock."
  (interactive)
  (condition-case er
      (let ((cols (plist-get params :cols))
	    (inherit (plist-get params :inherit))
	    (conds (plist-get params :conds))
	    (match (plist-get params :match))
	    (scope (plist-get params :scope))
	    (noquote (plist-get params :noquote))
	    (colnames (plist-get params :colnames))
	    (content-lines (org-split-string (plist-get params :content) "\n"))
	    id table line pos)
	(save-excursion
	  (when (setq id (plist-get params :id))
	    (cond ((not id) nil)
		  ((eq id 'global) (goto-char (point-min)))
		  ((eq id 'local)  nil)
		  ((setq idpos (org-find-entry-with-id id))
		   (goto-char idpos))
		  (t (error "Cannot find entry with :ID: %s" id))))
	  (unless (eq id 'global) (org-narrow-to-subtree))
	  (setq stringformat (if noquote "%s" "%S"))
	  (setq table (org-propview-to-table
		       (org-propview-collect cols stringformat conds match scope inherit
					     (if colnames colnames cols)) stringformat))
	  (widen))
	(setq pos (point))
	(when content-lines
	  (while (string-match "^#" (car content-lines))
	    (insert (pop content-lines) "\n")))
	(insert table) (insert "\n|--") (org-cycle) (move-end-of-line 1)
	(message (format "point-%d" pos))
	(while (setq line (pop content-lines))
	  (when (string-match "^#" line)
	    (insert "\n" line)))
	(goto-char pos)
	(org-table-recalculate 'all))
    (org-collector-error (widen) (error "%s" er))
    (error (widen) (error "%s" er))))

(defun org-propview-eval-w-props (props body)
  "evaluate the BODY-FORMS binding the variables using the
variables and values specified in props"
  (condition-case nil ;; catch any errors
      (eval `(let ,(mapcar
		    (lambda (pair) (list (intern (car pair)) (cdr pair)))
		    props)
	       ,body))
    (error nil)))

(defun org-propview-get-with-inherited (&optional inherit)
  (append
   (org-entry-properties)
   (delq nil
	 (mapcar (lambda (i)
		   (let* ((n (symbol-name i))
			  (p (org-entry-get (point) n 'do-inherit)))
		     (when p (cons n p))))
		 inherit))))

(defun org-propview-collect (cols stringformat &optional conds match scope inherit colnames)
  (interactive)
  ;; collect the properties from every header
  (let* ((header-props
	  (let ((org-trust-scanner-tags t) alst)
	    (org-map-entries
	     (quote (cons (cons "ITEM" (org-get-heading t))
			  (org-propview-get-with-inherited inherit)))
	     match scope)))
	 ;; read property values
	 (header-props
	  (mapcar (lambda (props)
		    (mapcar (lambda (pair)
			      (cons (car pair) (org-babel-read (cdr pair))))
			    props))
		  header-props))
	 ;; collect all property names
	 (prop-names
	  (mapcar 'intern (delete-dups
			   (apply 'append (mapcar (lambda (header)
						    (mapcar 'car header))
						  header-props))))))
    (append
     (list
      (if colnames colnames (mapcar (lambda (el) (format stringformat el)) cols))
       'hline) ;; ------------------------------------------------
     (mapcar ;; calculate the value of the column for each header
      (lambda (props) (mapcar (lambda (col)
			   (let ((result (org-propview-eval-w-props props col)))
			     (if result result org-propview-default-value)))
			 cols))
      (if conds
	  ;; eliminate the headers which don't satisfy the property
	  (delq nil
		(mapcar
		 (lambda (props)
		   (if (and-rest (mapcar
				  (lambda (col)
				    (org-propview-eval-w-props props col))
				  conds))
		       props))
		 header-props))
	  header-props)))))

(defun org-propview-to-table (results stringformat)
  ;; (message (format "cols:%S" cols))
  (orgtbl-to-orgtbl
   (mapcar
    (lambda (row)
      (if (equal row 'hline)
	  'hline
	(mapcar (lambda (el) (format stringformat el)) row)))
    (delq nil results)) '()))

; END

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("f5db04080a5133bc99721d680a11cf974d60d1df347b08841b43c3e97f52d3bf" "c5207e7b8cc960e08818b95c4b9a0c870d91db3eaf5959dd4eba09098b7f232b" default))))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
