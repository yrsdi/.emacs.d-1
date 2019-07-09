(defvar alwrite-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "+" 'alwrite-add)
    (define-key map "-" 'alwrite-remove)
    (define-key map [return] 'alwrite-toggle)
    (define-key map [remap previous-line] 'alwrite-previous-line)
    (define-key map [remap next-line] 'alwrite-next-line)
    map))

(define-derived-mode alwrite-mode special-mode "alwrite"
  (buffer-disable-undo)
  (setq truncate-lines t)
  (setq buffer-read-only t)
  (setq-local line-move-visual t)
  (setq show-trailing-whitespace nil)
  (make-local-variable 'text-property-default-nonsticky)
  (push (cons 'keymap t) text-property-default-nonsticky)
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'display-line-numbers-mode)
             (bound-and-true-p global-display-line-numbers-mode))
    (display-line-numbers-mode -1)))

(defun alwrite-add ()
  (interactive)
  (save-excursion
    (let ((str (read-from-minibuffer "Add: "))
          (inhibit-read-only t))
     (goto-char (point-max))
     (insert (format "\n%s" str)))))

(defun alwrite-remove ()
  (interactive)
  (let ((inhibit-read-only t))
    (kill-whole-line 2)))

(defun alwrite-toggle ()
  (interactive))

(defun alwrite-previous-line ()
  (interactive)
  (previous-line))

(defun alwrite-next-line ()
  (interactive)
  (next-line))

(defun alwrite ()
  (interactive)
  (switch-to-buffer "*alwrite*")
  (alwrite-mode))

(provide 'alwrite)
