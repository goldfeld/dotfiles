(require 'paredit)

(defun paredit-barf-all-the-way-backward ()
  (interactive)
  (paredit-split-sexp)
  (paredit-backward-down)
  (paredit-splice-sexp))

(defun paredit-barf-all-the-way-forward ()
  (interactive)
  (paredit-split-sexp)
  (paredit-forward-down)
  (paredit-splice-sexp)
  (if (eolp) (delete-horizontal-space)))

(defun paredit-slurp-all-the-way-backward ()
  (interactive)
  (catch 'done
    (while (not (bobp))
      (save-excursion
	(paredit-backward-up)
	(if (eq (char-before) ?\()
	    (throw 'done t)))
      (paredit-backward-slurp-sexp))))

(defun paredit-slurp-all-the-way-forward ()
  (interactive)
  (catch 'done
    (while (not (eobp))
      (save-excursion
	(paredit-forward-up)
	(if (eq (char-after) ?\))
	    (throw 'done t)))
      (paredit-forward-slurp-sexp))))

(define-key evil-normal-state-map (kbd "C-)") 'paredit-slurp-all-the-way-forward)
(define-key evil-normal-state-map (kbd "C-}") 'paredit-barf-all-the-way-forward)
(define-key evil-normal-state-map (kbd "C-(") 'paredit-slurp-all-the-way-backward)
(define-key evil-normal-state-map (kbd "C-{") 'paredit-barf-all-the-way-backward)
