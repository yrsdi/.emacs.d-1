(defun disable-font-lock-mode ()
  "Disable font lock mode."
  (font-lock-mode -1))

(menu-bar-mode -1)

(add-hook 'prog-mode-hook 'hs-minor-mode)
;; (add-hook 'prog-mode-hook 'disable-font-lock-mode)
;; (add-hook 'protobuf-mode-hook 'disable-font-lock-mode)
;; (add-hook 'text-mode-hook 'disable-font-lock-mode)
;; (add-hook 'conf-mode-hook 'disable-font-lock-mode)
;; (add-hook 'compilation-mode-hook 'disable-font-lock-mode)

(setq user-full-name "Travis Jeffery"
      user-mail-address "tj@travisjeffery.com")

;; turn off mode-line
;; (setq-default mode-line-format nil)

(setq-default fill-column 84)

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

(global-set-key (kbd "C-h A") 'apropos)

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

(setq eldoc-idle-delay 0)

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
(global-set-key (kbd "s-l") 'goto-line)

(defun tj-goland ()
  "Open current project in Goland."
  (interactive)
  (async-shell-command (format "goland %s" (projectile-project-root))))

(defun tj-markdown-convert-code-blocks ()
  (interactive)
  (cl-letf (((symbol-function 'kill-ticks)
             (lambda ()
               (search-forward "```")
               (beginning-of-line)
               (kill-whole-line)
               (point))))
    (string-rectangle (kill-ticks) (kill-ticks) "	")))

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
  "Go to or clone the given dev REPO."
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
      (setq tmp (replace-regexp-in-string "^0x" "" input ))
      (setq tmp (replace-regexp-in-string "^#x" "" tmp ))
      (setq tmp (replace-regexp-in-string "^#" "" tmp )))

    (message "Hex %s is %d" tmp (string-to-number tmp 16))))

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

(defun tj-toggle-fold ()
  "Toggle fold all lines larger than indentation on current line"
  (interactive)
  (let ((col 1))
    (save-excursion
      (back-to-indentation)
      (setq col (+ 1 (current-column)))
      (set-selective-display
       (if selective-display nil (or col 1))))))

(global-set-key (kbd "M-;") 'tj-comment-line)

(global-set-key (kbd "C-RET") 'other-window)
(global-set-key (kbd "C-z") 'delete-other-windows)

(global-set-key (kbd "C-c q") 'tj-kill-other-buffer)

;; use hippie-expand instead of dabbrev
(global-set-key (kbd "C-/") #'hippie-expand)

(global-set-key (kbd "s-b") 'backward-to-word)
(global-set-key (kbd "s-f") 'forward-to-word)

;; replace buffer-menu with ibuffer
(global-set-key (kbd "C-x C-b") #'ibuffer)

(setq markdown-command "multimarkdown")

;; align code in a pretty way
(global-set-key (kbd "C-x \\") #'align-regexp)

(global-set-key (kbd "C-h C-f") #'find-function)

;; misc useful keybindings
(global-set-key (kbd "s-<") #'beginning-of-buffer)
(global-set-key (kbd "s->") #'end-of-buffer)
(global-set-key (kbd "s-q") #'fill-paragraph)
(global-set-key (kbd "s-x") #'execute-extended-command)

(defun tj-multi-line-to-one-line (beg end)
  "Convert selected lines into one line and copy it in to the kill ring.
When transient-mark-mode is enabled, If no region is active then only the
current line is acted upon.

If the region begins or ends in the middle of a line, that entire line is
copied, even if the region is narrowed to the middle of a line.

Current position is preserved."
  (interactive "r")
  (let (str (orig-pos (point-marker)))
    (save-restriction
      (widen)
      (when (and transient-mark-mode (not (use-region-p)))
        (setq beg (line-beginning-position)
              end (line-beginning-position 2)))

      (goto-char beg)
      (setq beg (line-beginning-position))
      (goto-char end)
      (unless (= (point) (line-beginning-position))
        (setq end (line-beginning-position 2)))

      (goto-char beg)
      (setq str (replace-regexp-in-string "[ \t]*\n" "" (replace-regexp-in-string "^[ \t]+" "" (buffer-substring-no-properties beg end))))
      ;; (message "str=%s" str)
      (kill-new str)
      (goto-char orig-pos))))

(defun tj-spongebob (start end)
  "Convert string from START to END to SpOnGeBoB meme."
  (interactive "r")
  (save-excursion
    (goto-char start)
    (let ((upcase? (not (s-uppercase? (char-to-string (char-after))))))
      (while (not (eq end (point)))
        (setq upcase? (not upcase?))
        (let* ((curchar (char-after))
               (newchar (if upcase?
                            (upcase curchar)
                          (downcase curchar))))
          (delete-char 1)
          (insert-char newchar))))))

(define-key isearch-mode-map [(control return)]
  #'isearch-exit-other-end)

(defun isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

(define-key 'help-command (kbd "C-i") #'info-display-manual)

(defun tj-format-sql-region (beg end)
  "Format SQL in region from BEG to END."
  (interactive "r")
  (save-excursion
    (shell-command-on-region beg end "sql-formatter-cli" nil t)))

(defun tj-format-sql-buffer ()
  "Format SQL in buffer."
  (interactive)
  (tj-format-sql-region (point-min) (point-max)))

(defun tj-thesaurus ()
  "Browse thesaurus."
  (interactive)
  (tj--browse-word "https://www.merriam-webster.com/thesaurus/%s"))

(defun tj-dictionary ()
  "Browse dictionary."
  (interactive)
  (tj--browse-word "https://merriam-webster.com/dictionary/%s"))

(defun tj--browse-word (url)
  (let ((word
         (or
          (and (region-active-p)
               (buffer-substring-no-properties (region-beginning) (region-end)))
          (read-string "Word: "))))
    (browse-url (format url word))))

(defun tj-sql-mode-hook ()
  (add-hook 'after-save-hook 'tj-format-sql-buffer nil t))
;; (add-hook 'sql-mode-hook 'tj-sql-mode-hook)
(remove-hook 'sql-mode-hook 'tj-sql-mode-hook)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

;; improve find file at point to handle line numbers
(defvar ffap-file-at-point-line-number nil
  "Variable to hold line number from the last `ffap-file-at-point' call.")

(defadvice ffap-file-at-point (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and save it in `ffap-file-at-point-line-number' variable."
  (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
	 (name
	  (or (condition-case nil
		  (and (not (string-match "//" string)) ; foo.com://bar
		       (substitute-in-file-name string))
		(error nil))
	      string))
	 (line-number-string
	  (and (string-match ":[0-9]+" name)
	       (substring name (1+ (match-beginning 0)) (match-end 0))))
	 (line-number
	  (and line-number-string
	       (string-to-number line-number-string))))
    (if (and line-number (> line-number 0))
	(setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))

(defadvice ffap-guesser (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and save it in `ffap-file-at-point-line-number' variable."
  (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
	 (name
	  (or (condition-case nil
		  (and (not (string-match "//" string)) ; foo.com://bar
		       (substitute-in-file-name string))
		(error nil))
	      string))
	 (line-number-string
	  (and (string-match ":[0-9]+" name)
	       (substring name (1+ (match-beginning 0)) (match-end 0))))
	 (line-number
	  (and line-number-string
	       (string-to-number line-number-string))))
    (if (and line-number (> line-number 0))
	(setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))

(defadvice find-file (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (with-no-warnings (goto-line ffap-file-at-point-line-number))
    (setq ffap-file-at-point-line-number nil)))

(defadvice find-file-at-point (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (with-no-warnings (goto-line ffap-file-at-point-line-number))
    (setq ffap-file-at-point-line-number nil)))

(defun tj-commas-to-new-lines (start end)
  "Convert commas to commas with new-lines from START to END.
Useful to take a long list of arguments on one-line and split
them across multiple lines."
  (interactive "r")
  (let* ((in (buffer-substring-no-properties start end))
         (out (s-replace ", " ",\n" in)))
    (save-excursion
      (delete-region start end)
      (insert out))))

(global-set-key (kbd "<s-right>") 'sp-up-sexp)
(global-set-key (kbd "<s-left>") 'sp-down-sexp)

(defun tj-wrap-with-tags ()
  "Generates an open and close HTML snippet using the current word."
  (interactive)
  (let ((body (buffer-substring (region-beginning) (region-end))))
    (goto-char (region-beginning))
    (delete-char (string-width body))
    (yas-expand-snippet
     (concat "<${1:tag}$2>"
             body
             "</${1:$(and (string-match \"[-A-Za-z0-9:_]+\" yas-text)"
	     "(match-string 0 yas-text))}>"))))
(global-set-key (kbd "C-c C-w") 'tj-wrap-with-tags)
(define-key markdown-mode-map (kbd "C-c C-w") 'tj-wrap-with-tags)

(defun tj-insert-open-and-close-tag ()
  "Generates an open and close HTML snippet using the current word."
  (interactive)
  (let ((inserting-new-tag nil))
    (if (looking-back "[-A-Za-z0-9:_]")
	(progn (set-mark-command nil)
	       (while (looking-back "[-A-Za-z0-9:_]")
		 (backward-char)))
      (setq inserting-new-tag t)
      (set-mark-command nil)
      (insert "p")
      (exchange-point-and-mark))
    (let ((tag (buffer-substring (region-beginning) (region-end))))
      (delete-char (string-width tag))
      (cond ((string-match "\\`[bh]r\\'" tag)
	     (insert (concat "<" tag ">")))
	    ((string-match (concat "\\`\\(?:img\\|meta\\|link\\|"
				   "input\\|base\\|area\\|col\\|"
				   "frame\\|param\\)\\'")
			   tag)
	     (yas-expand-snippet (concat "<" tag " $1>$0")))
	    (t
	     (yas-expand-snippet
	      (if inserting-new-tag
		  (concat "<${1:"
			  tag
			  "}>$0</${1:"
			  "$(and (string-match \"[-A-Za-z0-9:_]+\" yas-text) "
			  "(match-string 0 yas-text))}>")
		(concat "<"
			tag
			"$1>$0</"
			tag
			">"))))))))
(global-set-key (kbd "C-c <") 'tj-insert-open-and-close-tag)
(define-key markdown-mode-map (kbd "C-c <") 'tj-insert-open-and-close-tag)

(defun init-subword ()
  (let ((adv (cons 'advice
		   (lambda ()
		     (let ((os (char-syntax ?_)))
		       (modify-syntax-entry ?_ "_")
		       ad-do-it
		       (modify-syntax-entry ?_ (string os))))))
	(fun '(subword-forward subword-kill subword-backward
			       subword-backward-kill subword-downcase subword-upcase
			       subword-transpose)))
    (dolist (f fun)
      (ad-add-advice f (list 'underscore-wrap nil t adv)
		     'around 'last)
      (ad-activate f))))

(add-hook 'after-init-hook #'init-subword)

(add-hook 'focus-out-hook 'garbage-collect)

(defun tj-prowritingaid-to-markdown-format ()
  "Fix quotes after copying from ProWritingAid."
  (interactive)
  (let ((replacements
         '(("“" . "\"")
          ("”" . "\"")
          ("’" . "'")
          ("‘" . "'")
          (" " . " "))))
    (cl-loop for (key . value) in replacements
             do (progn
                  (goto-char 0)
                  (replace-string key value)))))

(defun tj-remove-prag-prog-code-tags ()
  (interactive)
  (save-excursion
    (goto-char 0)
    (replace-regexp "^.*// END.*
" "")
    )

  (save-excursion
    (goto-char 0)
    (replace-regexp "^.*// START.*
" "")))

;; (define-key proced-mode-map (kbd "/") 'proced-narrow)

(defun tj-arrayify (beg end)
  "Wrap each line from BEG to END in quotes and join them in a line."
  (interactive "r")
  (replace-region-contents beg end
                          (lambda ()
                            (-> (buffer-substring-no-properties beg end)
                                (split-string "\n")
                                (->>
                                 (remove "")
                                 (cl-mapcar (lambda (x)
                                              (format "\"\%s\"" x))))
                                (string-join ", "))))
  (end-of-line))

(defun tj-browse-urls (beg end)
  "Browse the URL on every line between BEG and END."
  (interactive "r")
  (goto-char beg)
  (while (< (point) end)
    (beginning-of-line)
    (browse-url-at-point)
    (forward-line)))

(setq gc-cons-threshold 300000000)

(defadvice backward-kill-word (around fix activate)
  (cl-flet ((kill-region (b e) (delete-region b e)))
    ad-do-it))

(provide 'tj)
