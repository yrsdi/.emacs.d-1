(defun t-email ()
  (interactive)
  (with-current-buffer (get-buffer-create "*email*")
    (markdown-mode)
    (toggle-word-wrap)
    (pop-to-buffer (current-buffer))))

(defun comment-eclipse ()
  (interactive)
  (let ((start (line-beginning-position))
        (end (line-end-position)))
    (when (or (not transient-mark-mode) (region-active-p))
      (setq start (save-excursion
                    (goto-char (region-beginning))
                    (beginning-of-line)
                    (point))
            end (save-excursion
                  (goto-char (region-end))
                  (end-of-line)
                  (point))))
    (comment-or-uncomment-region start end)))

(global-set-key (kbd "M-;") 'comment-eclipse)

(defun t-word-count ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "wc -w"))

(defun isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-foward-at-point (&optional regexp-p no-recursive-edit)
  (interactive)
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-foward "w_") (point)))
           (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
          (isearch-forward regexp-p no-recursive-edit)
        (setq isearch-initial-string (buffer-substring begin end))
        (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
        (isearch-forward regexp-p no-recursive-edit)))))


(defun t-config ()
  (interactive)
  (find-file "~/.emacs.d/personal/config.el"))

(defun t-newline-and-indent-up ()
  (interactive)
  (line-move -1)
  (end-of-line)
  (newline-and-indent))

(defun t-org ()
  "Find a .org file to edit."
  (interactive)
  (ido-find-file-in-dir "~/org"))

(defun t-only-buffer ()
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
             (next-win-buffer (window-buffer (next-window)))
             (this-win-edges (window-edges (selected-window)))
             (next-win-edges (window-edges (next-window)))
             (this-win-2nd (not (and (<= (car this-win-edges)
                                         (car next-win-edges))
                                     (<= (cadr this-win-edges)
                                         (cadr next-win-edges)))))
             (splitter
              (if (= (car this-win-edges)
                     (car (window-edges (next-window))))
                  'split-window-horizontally
                'split-window-vertically)))
        (delete-other-windows)
        (let ((first-win (selected-window)))
          (funcall splitter)
          (if this-win-2nd (other-window 1))
          (set-window-buffer (selected-window) this-win-buffer)
          (set-window-buffer (next-window) next-win-buffer)
          (select-window first-win)
          (if this-win-2nd (other-window 1))))))

(defun t-marked ()
  "View the current buffer in Marked.app"
  (interactive)
  (shell-command (format "open -a Marked %s" (expand-file-name (buffer-file-name (current-buffer))))))

(defun t-eshell-execute-current-line ()
  "Insert text of current line in eshell and execute."
  (interactive)
  (require 'eshell)
  (let ((command (buffer-substring
                  (save-excursion
                    (beginning-of-line)
                    (point))
                  (save-excursion
                    (end-of-line)
                    (point)))))
    (let ((buf (current-buffer)))
      (unless (get-buffer eshell-buffer-name)
        (eshell))
      (display-buffer eshell-buffer-name t)
      (switch-to-buffer-other-window eshell-buffer-name)
      (end-of-buffer)
      (eshell-kill-input)
      (insert command)
      (eshell-send-input)
      (end-of-buffer)
      (switch-to-buffer-other-window buf))))

(defun t-kill-line-save (&optional arg)
  (interactive "p")
  (save-excursion
    (copy-region-as-kill
     (point)
     (progn (if arg (forward-visible-line arg)
              (end-of-visible-line))
            (point)))))

(global-set-key (kbd "C-c C-k") 't-kill-line-save)

(defun t-eshell (name)
  (interactive "sName: ")
  (let ((eshell-buffer-name (format "*eshell: %s*" name)))
    (if (get-buffer eshell-buffer-name)
        (switch-to-buffer-other-window eshell-buffer-name)
      (eshell eshell-buffer-name))))

(defun t-eshell-execute-previous-input ()
  (interactive)
  (save-excursion
    (switch-to-buffer-other-window eshell-buffer-name)
    (call-interactively 'eshell-previous-matching-input-from-input)
    (eshell-send-input)))
