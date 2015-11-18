(require 'projectile)

(defun tj/email ()
  (interactive)
  (with-current-buffer (get-buffer-create "*email*")
    (markdown-mode)
    (toggle-word-wrap)
    (pop-to-buffer (current-buffer))))

(defun tj/comment-eclipse ()
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

(global-set-key (kbd "M-;") 'tj/comment-eclipse)

(defun tj/word-count ()
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

(defun tj/newline-and-indent-up ()
  (interactive)
  (line-move -1)
  (end-of-line)
  (newline-and-indent))

(defun tj/find-org-file ()
  "Find a .org file to edit."
  (interactive)
  (ido-find-file-in-dir "~/Dropbox/org"))

(defun tj/find-config-file ()
  "Find a personal config file to edit."
  (interactive)
  (ido-find-file-in-dir "~/.emacs.d/personal/"))

(defun tj/only-buffer ()
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

(defun tj/marked ()
  "View the current buffer in Marked.app"
  (interactive)
  (shell-command (format "open -a Marked %s" (expand-file-name (buffer-file-name (current-buffer))))))

(defun tj/kill-line-save (&optional arg)
  (interactive "p")
  (save-excursion
    (copy-region-as-kill
     (point)
     (progn (if arg (forward-visible-line arg)
              (end-of-visible-line))
            (point)))))

(global-set-key (kbd "C-c C-k") 'tj/kill-line-save)

(defun tj/eshell (name)
  (interactive "sName: ")
  (let ((eshell-buffer-name (format "*eshell: %s*" name)))
    (if (get-buffer eshell-buffer-name)
        (switch-to-buffer-other-window eshell-buffer-name)
      (eshell eshell-buffer-name))))

(defun tj/eshell-execute-previous-input ()
  (interactive)
  (save-excursion
    (switch-to-buffer-other-window eshell-buffer-name)
    (call-interactively 'eshell-previous-matching-input-from-input)
    (eshell-send-input)))

(defun tj-goto (repo)
  "Go to or clone the given dev `repo'."
  (interactive
   (list
    (ido-read-directory-name "Directory: " "~/dev/")))
  (let* ((dev-dir "~/dev/")
         (repo-dir repo))
    (if (file-exists-p repo-dir)
        (projectile-find-file-in-directory repo)
      (shell-command (format "cd %s; git clone git@github.com:travisjeffery/%s.git" dev-dir repo))
      (projectile-find-file-in-directory repo))))
(global-set-key (kbd "C-M-T") 'tj-goto)

(defun tj/delete-file-and-buffer ()
  "Delete the current file and kill the current buffer."
  (interactive)
  (delete-file (buffer-file-name (current-buffer)))
  (kill-buffer (current-buffer)))
(global-set-key (kbd "C-c K") 'tj/delete-file-and-buffer)

(defun tj/github-open-file ()
  "View the current file in a web browser on GitHub."
  (interactive)
  (let* ((root (helm-open-github--root-directory))
         (repo-path (file-relative-name (buffer-file-name) root)))
    (helm-open-github--from-file-action repo-path)))
(global-set-key (kbd "C-c o F") 'tj/github-open-file)

(defun tj/projectile-ack (&optional args)
  "Ack in specified directory."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively 'projectile-ack)))

(defun tj/projectile-mode-hook ()
  "My hook for projectile."
  (define-key projectile-mode-map (kbd "C-c p s a") #'tj/projectile-ack)
  (define-key projectile-mode-map (kbd "s-p s a") #'tj/projectile-ack))
(add-hook 'projectile-mode-hook 'tj/projectile-mode-hook)
