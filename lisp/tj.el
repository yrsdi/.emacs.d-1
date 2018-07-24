(defun disable-font-lock-mode ()
  "Disable font lock mode."
  (font-lock-mode -1))

(menu-bar-mode -1)

(add-hook 'prog-mode-hook 'disable-font-lock-mode)
(add-hook 'protobuf-mode-hook 'disable-font-lock-mode)
(add-hook 'text-mode-hook 'disable-font-lock-mode)
(add-hook 'conf-mode-hook 'disable-font-lock-mode)
(add-hook 'compilation-mode-hook 'disable-font-lock-mode)

(setq user-full-name "Travis Jeffery"
      user-mail-address "tj@travisjeffery.com")

;; turn off mode-line
;; (setq-default mode-line-format nil)

(setq-default fill-column 100)

(set-frame-font (font-spec :family "Operator Mono" :size 14 :weight 'normal))

;; open help, ack, etc. in the same window
;; (setq-default same-window-regexps '("."))
(setq-default same-window-regexps nil)

;; Always load newest byte code
(setq load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defconst savefile-dir (expand-file-name "savefile" user-emacs-directory))

;; create the savefile dir if it doesn't exist
(unless (file-exists-p savefile-dir)
  (make-directory savefile-dir))

;; the toolbar is just a waste of valuable screen estate
;; in a tty tool-bar-mode does not properly auto-load, and is
;; already disabled anyway
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; the blinking cursor is nothing, but an annoyance
(blink-cursor-mode -1)

;; disable the annoying bell ring
(setq ring-bell-function 'ignore)

;; disable startup screen
(setq inhibit-startup-screen t)

;; nice scrolling
(setq scroll-margin 0
      scroll-conservatively 100000
      scroll-preserve-screen-position 1)

;; mode line settings
(line-number-mode t)
(column-number-mode t)
(size-indication-mode t)

;; enable y/n answers
(fset 'yes-or-no-p 'y-or-n-p)

;; more useful frame title, that show either a file or a
;; buffer name (if the buffer isn't visiting a file)
(setq frame-title-format
      '((:eval (if (buffer-file-name)
		   (abbreviate-file-name (buffer-file-name))
		 "%b"))))

;; Emacs modes typically provide a standard means to change the
;; indentation width -- eg. c-basic-offset: use that to adjust your
;; personal indentation width, while maintaining the style (and
;; meaning) of any files you load.
(setq-default indent-tabs-mode nil)   ;; don't use tabs to indent
(setq-default tab-width 8)	      ;; but maintain correct appearance

;; Newline at end of file
(setq require-final-newline t)

;; delete the selection with a keypress
(delete-selection-mode t)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; revert buffers automatically when underlying files are changed externally
(global-auto-revert-mode t)

(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; hippie expand is dabbrev expand on steroids
(setq hippie-expand-try-functions-list '(try-expand-dabbrev
					 try-expand-dabbrev-all-buffers
					 try-expand-dabbrev-from-kill
					 try-complete-file-name-partially
					 try-complete-file-name
					 try-expand-all-abbrevs
					 try-expand-list
					 try-expand-line
					 try-complete-lisp-symbol-partially
					 try-complete-lisp-symbol))

(defun tj-kill-line-save (&optional arg)
  (interactive "p")
  (save-excursion
    (copy-region-as-kill
     (point)
     (progn (if arg (forward-visible-line arg)
	      (end-of-visible-line))
	    (point)))))
(global-set-key (kbd "C-c C-k") 'tj-kill-line-save)

(defun tj-eshell (name)
  (interactive "sName: ")
  (let ((eshell-buffer-name (format "*eshell: %s*" name)))
    (if (get-buffer eshell-buffer-name)
	(switch-to-buffer-other-window eshell-buffer-name)
      (eshell eshell-buffer-name))))

(defun tj-eshell-execute-previous-input ()
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

(defun tj-marked ()
  "Open this markdown file in Marked 2."
  (interactive)
  (shell-command (format "open -a \"Marked 2\" %s" (buffer-file-name))))

(defun tj-newline-and-indent-up ()
  "Open a new line above the current line."
  (interactive)
  (line-move -1)
  (end-of-line)
  (newline-and-indent))

(defun tj-ag-regexp (string)
  (interactive "sSearch string: ")
  (ag-regexp string  (projectile-project-root)))


(defun tj-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun tj-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
  current buffer's, reload dir-locals."
  (interactive)
  (let ((dir (projectile-project-root)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(when (equal default-directory dir))
	(tj-reload-dir-locals-for-current-buffer)))))

(defun tj-what-hexadecimal-value ()
  "Prints the decimal value of a hexadecimal string under cursor."
  (interactive)

  (let (input tmp p1 p2 )
    (save-excursion
      (re-search-backward "[^0-9A-Fa-fx#]" nil t)
      (forward-char)
      (setq p1 (point) )
      (re-search-forward "[^0-9A-Fa-fx#]" nil t)
      (backward-char)
      (setq p2 (point) ) )

    (setq input (buffer-substring-no-properties p1 p2) )

    (let ((case-fold-search nil) )
      (setq tmp (replace-regexp-in-string "^0x" "" input )) ; C, Perl, …
      (setq tmp (replace-regexp-in-string "^#x" "" tmp )) ; elisp …
      (setq tmp (replace-regexp-in-string "^#" "" tmp ))  ; CSS …
      )

    (message "Hex %s is %d" tmp (string-to-number tmp 16))))

(defun tj-find-config ()
  "Find a personal config file to edit."
  (interactive)
  (find-file "~/.emacs.d/init.el"))

(defun tj-toggle-window-split ()
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

(defun tj-newline-and-indent ()
  "Newline under the current line."
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "<s-return>") 'tj-newline-and-indent)

(defun tj-finder-here ()
  "Open Finder here."
  (interactive)
  (shell-command "open -a Finder $PWD"))

(defun tj-iterm-here ()
  "Open iTerm here."
  (interactive)
  (dired-smart-shell-command "open -a iTerm $PWD" nil nil))

(defun tj-eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value"
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

(defun tj-goto-match-beginning ()
  "Go to the start of current isearch match.
  Use in `isearch-mode-end-hook'."
  (when (and isearch-forward
	     (number-or-marker-p isearch-other-end)
	     (not mark-active)
	     (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

(defun tj-comment-line ()
  "Comment the current line or region."
  (interactive)
  (call-interactively #'comment-line)
  (unless (region-active-p) (forward-line -1)))

(defun tj-kill-other-buffer ()
  "Kill the other window's buffer."
  (interactive)
  (other-window 1)
  (kill-buffer)
  (other-window 1))
(global-set-key (kbd "s-z") 'tj-kill-other-buffer)

(global-set-key (kbd "M-;") 'tj-comment-line)

(global-set-key (kbd "C-RET") 'other-window)
(global-set-key (kbd "C-z") 'delete-other-windows)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "M-/") #'hippie-expand)
(global-set-key (kbd "s-/") #'hippie-expand)

(global-set-key (kbd "s-b") 'backward-to-word)
(global-set-key (kbd "s-f") 'forward-to-word)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

(global-set-key (kbd "C-h C-f") #'find-function)

;; misc useful keybindings
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)
(global-set-key (kbd "s-q") #'fill-paragraph)
(global-set-key (kbd "s-x") #'execute-extended-command)

(provide 'tj)
