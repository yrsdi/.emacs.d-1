(defvar-local alwrite-buf nil)
(defvar-local alwrite-beg nil)
(defvar-local alwrite-end nil)
(defvar-local alwrite-str nil)

(defvar alwrite-mode-map
  (let ((map (make-keymap)))
    (suppress-keymap map t)
    (define-key map "+" 'alwrite-add)
    (define-key map "-" 'alwrite-remove)
    (define-key map "p" 'alwrite-previous-line)
    (define-key map "n" 'alwrite-next-line)
    (define-key map (kbd "C-c C-c") 'alwrite-commit)
    (define-key map (kbd "C-c C-k") 'alwrite-abort)
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
  (hl-line-mode +1)
  (when (bound-and-true-p global-linum-mode)
    (linum-mode -1))
  (when (and (fboundp 'display-line-numbers-mode)
             (bound-and-true-p global-display-line-numbers-mode))
    (display-line-numbers-mode -1)))

(defun alwrite-add ()
  (interactive)
  (let ((str (read-from-minibuffer "Add: "))
        (inhibit-read-only t))
    (goto-char (point-max))
    (unless (eq (point-max) (point-min))
      (insert "\n"))
    (insert str)
    (alwrite-update-buffer-for-current-line)))

(defun alwrite-remove ()
  (interactive)
  (let ((inhibit-read-only t))
    (beginning-of-line)
    (kill-whole-line -1)))

(defun alwrite-commit ()
  (interactive)
  (with-current-buffer "*alwrite*"
    (kill-buffer-and-window)))

(defun alwrite-abort ()
  (interactive)
  (goto-char (point-min))
  (alwrite-update-buffer-for-current-line)
  (alwrite-commit))

(defun alwrite-previous-line ()
  (interactive)
  (previous-line)
  (alwrite-update-buffer-for-current-line))

(defun alwrite-next-line ()
  (interactive)
  (next-line)
  (alwrite-update-buffer-for-current-line))

(defun alwrite-update-buffer-for-current-line ()
    (alwrite-update-buffer (alwrite-current-line-string)))

(defun alwrite-current-line-string ()
  (buffer-substring (point-at-bol) (point-at-eol)))

(defun alwrite-update-buffer (str)
  (if alwrite-buf
      (let ((beg alwrite-beg)
            (cur-end alwrite-end)
            (new-end alwrite-end))
        (with-current-buffer alwrite-buf
          (progn
            (goto-char beg)
            (if cur-end
                (kill-region beg cur-end))
            (insert str)
            (setq new-end (point))))
        (setq-local alwrite-end new-end))))

(defun alwrite ()
  (interactive)
  (split-window)
  (let* ((cur (current-buffer))
        (reg (region-active-p))
        (beg (region-beginning))
        (end (region-end))
        (str (buffer-substring (region-beginning) (region-end)))
        (new (switch-to-buffer "*alwrite*"))
        (_ (alwrite-mode))
        (inhibit-read-only t))
    (setq-local alwrite-buf cur)
    (setq-local alwrite-str str)
    (setq-local alwrite-beg beg)
    (if reg
        (progn
          (setq-local alwrite-end end)
          (insert alwrite-str)))))

(provide 'alwrite)
