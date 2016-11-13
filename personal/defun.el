(defun orgtbl-to-md (start end)
  "Convert an org-mode table into markdown format"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      ;; Locate divider row
      (re-search-forward "^\\s-*|-[-+]*|?\\s-*$")
      ;; Start by replacing the +es
      (subst-char-in-region (match-beginning 0) (match-end 0) ?+ ?|)
      ;; Locate column alignment row (fixme: code below expects this to be next line)
      (re-search-forward "^\\s-*\\(|\\s-*<[lcr]>\\s-*\\)*|?\\s-*$")
      ;; For each tag, adjust the entry above it
      (beginning-of-line)
      (while (re-search-forward "<[lcr]>" (line-end-position) t)
	(pcase (char-before (1- (point)))
	  (?r (let ((cc (current-column)))
		(forward-line -1)
		(move-to-column cc)
		(skip-chars-forward "-")
		(subst-char-in-region (1- (point)) (point) ?- ?:)
		(forward-line)
		(move-to-column cc)))
	  (?l (let ((cc (current-column)))
		(forward-line -1)
		(move-to-column cc)
		(skip-chars-backward "-")
		(subst-char-in-region (point) (1+ (point)) ?- ?:)
		(forward-line)
		(move-to-column cc)))
	  (?c (let ((cc (current-column)))
		(forward-line -1)
		(move-to-column cc)
		(skip-chars-backward "-")
		(subst-char-in-region (point) (1+ (point)) ?- ?:)
		(skip-chars-forward "-:")
		(subst-char-in-region (1- (point)) (point) ?- ?:)
		(forward-line)
		(move-to-column cc)))
	  )
	)
      ;; Remove the alignment tag line
      (delete-region (line-beginning-position) (line-beginning-position 2))
      )))

(defun md-to-orgtbl (start end)
  "Convert a markdown table into org-mode format"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (search-forward-regexp "^\\s-*\\(|:?-+:?\\)*|?\\s-*$")
      (let (p1 p2 myLine)
	(setq p1 (line-beginning-position) )
	(setq p2 (line-end-position) )
	(setq myLine (buffer-substring-no-properties p1 p2))

	(subst-char-in-region p1 p2 ?: ?-)
	(insert "\n")
	(insert (replace-regexp-in-string "-+" ""
          (replace-regexp-in-string "-+:" "<r>"
	  (replace-regexp-in-string ":-+" "<l>"
	  (replace-regexp-in-string ":-+:" "<c>"
           myLine)))))
      ))))

(defun orgtbl-to-gfm (table params)
  "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
  (let* ((alignment (mapconcat (lambda (x) (if x "|--:" "|---"))
                               org-table-last-alignment ""))
         (params2
          (list
           :splice t
           :hline (concat alignment "|")
           :lstart "| " :lend " |" :sep " | ")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))

(defun orgtbl-to-gfm (table params)
  "Convert the Orgtbl mode TABLE to GitHub Flavored Markdown."
  (let* ((alignment (mapconcat (lambda (x) (if x "|--:" "|---"))
                               org-table-last-alignment ""))
         (params2
          (list
           :splice t
           :hline (concat alignment "|")
           :lstart "| " :lend " |" :sep " | ")))
    (orgtbl-to-generic table (org-combine-plists params2 params))))

(defun tjj-insert-org-to-md-table (table-name)
  (interactive "*sEnter table name: ")
  (insert "<!---
#+ORGTBL: SEND " table-name " orgtbl-to-gfm

-->
<!--- BEGIN RECEIVE ORGTBL " table-name " -->
<!--- END RECEIVE ORGTBL " table-name " -->")
  (previous-line)
  (previous-line)
  (previous-line))

(defun tjj/generalized-shell-command (command arg)
  "Unifies `shell-command' and `shell-command-on-region'. If no region is
selected, run a shell command just like M-x shell-command (M-!).  If
no region is selected and an argument is a passed, run a shell command
and place its output after the mark as in C-u M-x `shell-command' (C-u
M-!).  If a region is selected pass the text of that region to the
shell and replace the text in that region with the output of the shell
command as in C-u M-x `shell-command-on-region' (C-u M-|). If a region
is selected AND an argument is passed (via C-u) send output to another
buffer instead of replacing the text in region."
  (interactive (list (read-from-minibuffer "Shell command: " nil nil nil 'shell-command-history)
                     current-prefix-arg))
  (let ((p (if mark-active (region-beginning) 0))
        (m (if mark-active (region-end) 0)))
    (if (= p m)
        ;; No active region
        (if (eq arg nil)
            (shell-command command)
          (shell-command command t))
      ;; Active region
      (if (eq arg nil)
          (shell-command-on-region p m command t t)
        (shell-command-on-region p m command)))))

(defun tjj/email ()
  (interactive)
  (with-current-buffer (get-buffer-create "*email*")
    (markdown-mode)
    (toggle-word-wrap)
    (pop-to-buffer (current-buffer))))

(defun tjj/comment-eclipse ()
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
(global-set-key (kbd "M-;") 'tjj/comment-eclipse)

(defun tjj/word-count ()
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

(defun tjj/newline-and-indent-up ()
  (interactive)
  (line-move -1)
  (end-of-line)
  (newline-and-indent))

(defun tjj/find-config-file ()
  "Find a personal config file to edit."
  (interactive)
  (ido-find-file-in-dir "~/.emacs.d/personal/"))

(defun tjj/only-buffer ()
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

(defun tjj/toggle-window-split ()
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

(defun tjj/kill-line-save (&optional arg)
  (interactive "p")
  (save-excursion
    (copy-region-as-kill
     (point)
     (progn (if arg (forward-visible-line arg)
              (end-of-visible-line))
            (point)))))

(global-set-key (kbd "C-c C-k") 'tjj/kill-line-save)

(defun tjj/eshell (name)
  (interactive "sName: ")
  (let ((eshell-buffer-name (format "*eshell: %s*" name)))
    (if (get-buffer eshell-buffer-name)
        (switch-to-buffer-other-window eshell-buffer-name)
      (eshell eshell-buffer-name))))

(defun tjj/eshell-execute-previous-input ()
  (interactive)
  (save-excursion
    (switch-to-buffer-other-window eshell-buffer-name)
    (call-interactively 'eshell-previous-matching-input-from-input)
    (eshell-send-input)))

(defun tjj/goto (repo)
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

(defun tjj/github-open-file ()
  "View the current file in a web browser on GitHub."
  (interactive)
  (let* ((root (helm-open-github--root-directory))
         (repo-path (file-relative-name (buffer-file-name) root)))
    (helm-open-github--from-file-action repo-path)))
(global-set-key (kbd "C-c o F") 'tjj/github-open-file)

(defun tjj/projectile-ack (&optional args)
  "Ack in specified directory."
  (interactive)
  (let ((current-prefix-arg t))
    (call-interactively 'projectile-ack)))

(defun tjj/projectile-mode-hook ()
  "My hook for projectile."
  (define-key projectile-mode-map (kbd "C-c p s a") #'tjj/projectile-ack)
  (define-key projectile-mode-map (kbd "s-p s a") #'tjj/projectile-ack))
(add-hook 'projectile-mode-hook 'tjj/projectile-mode-hook)
