(require 'use-package)

(setq flycheck-check-syntax-automatically '(save mode-enabled))
(setq-default tab-width 8)
(setq multi-term-program "/usr/local/bin/zsh")
(setq explicit-shell-file-name "/usr/local/bin/zsh")
(setq ring-bell-function 'ignore)
(setq scroll-margin 10)
(setq display-line-numbers nil)
(setq auto-window-vscroll nil)
(global-undo-tree-mode 0)
(set-frame-font (font-spec :family "Operator Mono" :size 14 :weight 'normal))
(add-to-list 'default-frame-alist '(font . "Operator Mono-14"))
(global-hl-line-mode -1)
(setq vc-handled-backends nil)
(setq-default sentence-end-double-space nil)
(setq-default fill-column 100)
(setq load-prefer-newer t)
(setq large-file-warning-threshold 100000000)
(setq-default auto-fill-function 'do-auto-fill)
(setq gc-cons-threshold 50000000)

;; (setq-default same-window-regexps '("."))

;;; Settings

(use-package font-core
  :config (global-font-lock-mode -1))

(use-package menu-bar
  :config (menu-bar-mode -1))

(use-package zop-to-char
  :bind
  (("M-z" . 'zop-up-to-char)
   ("M-W" . 'zop-to-char)))

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-c C-s") 'crux-create-scratch-buffer)
(global-set-key (kbd "s-b") 'backward-to-word)
(global-set-key (kbd "s-f") 'forward-to-word)

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)

(defun tj-newline-and-indent ()
  (interactive)
  (end-of-line)
  (newline-and-indent))
(global-set-key (kbd "<s-return>") 'tj-newline-and-indent)

(defun tj-iterm-here ()
  (interactive)
  (dired-smart-shell-command "open -a iTerm $PWD" nil nil))

(use-package wgrep)

(use-package dashboard
  :init
  (setq initial-buffer-choice (lambda () (switch-to-buffer "*dashboard*"))))

(use-package toggle-quotes
  :bind
  ( "C-\"" . toggle-quotes))

(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

(use-package wgrep-ag
  :config (autoload 'wgrep-ag-setup "wgrep-ag")
  :hook (ag-mode-hook . wgrep-ag-setup))

(use-package protobuf-mode
  :mode "\\.proto\\'"
  :commands (protobuf-mode)
  :hook (protobuf-mode
         . (lambda ()
             (subword-mode)
             (electric-pair-mode)
             (c-add-style "my-protobuf-style" my-protobuf-style t)))
  :config
  (progn
    (defconst my-protobuf-style
      '((c-basic-offset . 2)
        (indent-tabs-mode . nil)))))

(use-package visual-regexp
  :bind
  ("M-&" . vr/query-replace)
  ("M-/" . vr/replace))

(use-package diff-mode
  :commands diff-mode
  :hook
  (diff-mode-hook . font-lock-mode))

(use-package minibuffer
  :config
  (defun my-minibuffer-setup-hook ()
    (smartparens-mode -1)
    (electric-pair-mode -1)
    (subword-mode)
    (setq gc-cons-threshold most-positive-fixnum))

  (defun my-minibuffer-exit-hook ()
    (electric-pair-mode -1)
    (setq gc-cons-threshold 800000))

  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook))

(use-package prelude-mode
  :diminish
  :config
  (setq prelude-guru nil)
  (setq prelude-clean-whitespace-on-save t)
  :hook
  (prelude-prog-mode .
                          (lambda ()
                            (setq display-line-numbers nil)
                            (subword-mode)
                            (modify-syntax-entry ?_ "w")
                            (smartparens-mode -1)
                            ;; (electric-pair-mode)
                            ;; (electric-indent-mode)
                            )))

(use-package smartparens
  :diminish
  :bind (:map smartparens-mode-map
              ("M-k" . sp-raise-sexp)
              ("M-I" . sp-splice-sexp)
              ("M-<up>" . nil)
              ("M-<down>" . nil))
  :hook (smartparens-mode
         . (lambda ()
             (unbind-key "M-s" smartparens-mode-map))))

(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

;; (add-hook 'isearch-mode-end-hook #'endless/goto-match-beginning)

(global-set-key (kbd "C-M-g") 'prelude-google)

(defun tj-eval-and-replace (value)
  "Evaluate the sexp at point and replace it with its value"
  (interactive (list (eval-last-sexp nil)))
  (kill-sexp -1)
  (insert (format "%S" value)))

(defun endless/goto-match-beginning ()
  "Go to the start of current isearch match.
Use in `isearch-mode-end-hook'."
  (when (and isearch-forward
             (number-or-marker-p isearch-other-end)
             (not mark-active)
             (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

(use-package exec-path-from-shell

  :config
  (exec-path-from-shell-initialize))

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
(global-set-key (kbd "M-/") #'hippie-expand)

(defun tj-marked ()
  (interactive)
  (shell-command (format "open -a \"Marked 2\" %s" (buffer-file-name))))

(use-package markdown-mode
  :mode
  ("\\.markdown$" . markdown-mode)
  ("\\.md$" . markdown-mode)
  :config
  (defun my-markdown-hook ()
    (auto-fill-mode)
    (flycheck-mode))
  (add-hook 'markdown-mode-hook 'my-markdown-hook))

(use-package yaml-mode
  :mode
  ("\\.yaml" . yaml-mode))

(use-package org
  :init

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
  (setq org-agenda-files (split-string (shell-command-to-string "find ~/Dropbox/org/*")))
  (setq org-archive-location (expand-file-name "~/Dropbox/org/archive.org::* Archived Tasks"))
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-use-fast-todo-selection t)
  (setq org-src-lang-modes '(
                             ("screen" . sh)
                             ("ocaml" . tuareg)
                             ("elisp" . emacs-lisp)
                             ("lisp" . lisp)
                             ("ditaa" . artist)
                             ("asymptote" . asy)
                             ("cl" . lisp)
                             ("dot" . graphviz-dot)))

  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min)))

  (setq org-capture-templates
        (quote (("t" "Todo" entry (file "~/Dropbox/org/capture.org")
                 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                ("n" "Note" entry (file "~/Dropbox/org/capture.org")
                 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                ("j" "Journal" entry (function org-journal-find-location)
                 "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
                ("w" "org-protocol" entry (file "~/git/org/refile.org")
                 "* TODO Review %c\n%U\n" :immediate-finish t))
               ))

  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-compact-blocks t)

  (setq org-agenda-custom-commands
        (quote (("d" todo nil)
	        ("c" todo "DONE|DEFERRED|CANCELLED" nil)
	        ("w" todo "WAITING" nil)
	        ("W" agenda "" ((org-agenda-ndays 21)))
	        ("A" agenda ""
	         ((org-agenda-skip-function
	           (lambda nil
		     (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
	          (org-agenda-ndays 1)
	          (org-agenda-overriding-header "Today's Priority #A tasks: ")))
	        ("u" alltodo ""
	         ((org-agenda-skip-function
	           (lambda nil
		     (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
					       (quote regexp) "\n]+>")))
	          (org-agenda-overriding-header "Unscheduled TODO entries: "))))))

  :config
  (defun tj-org-archive-done-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/DONE" 'tree))

  (require 'org-table)
  :bind
  ("M-s c" . org-capture))

(add-to-list 'completion-styles 'initials t)
(add-to-list 'completion-styles 'subwords t)
(add-to-list 'completion-styles 'substring t)

(use-package company
  :init
  ;; (setq company-dabbrev-code-modes t)

  ;; (setq company-dabbrev-char-regexp "\\(\\sw\\|\\s_\\|_\\|-\\)")
  (setq company-ddabbrev-code-everywhere t)
  (setq company-dabbrev-code-modes t)
  (setq company-dabbrev-code-other-buffers 'all)
  (setq company-dabbrev-ignore-buffers "\\`\\'")

  (setq company-idle-delay 0.1)
  (setq company-echo-delay 0)
  (setq company-tooltip-align-annotations t)
  (setq company-tern-property-marker "")
  ;; (setq company-begin-commands '(self-insert-command))
  (setq company-minimum-prefix-length 5)
  (setq company-abort-manual-when-too-short 5)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-other-buffers 'all)
  :diminish
  :bind
  ("<tab>" . company-indent-or-complete-common)

  :config

  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it)))

(use-package company-elisp
  :after company
  :config
  (push 'company-elisp company-backends))
(setq-local company-backend '(company-elisp))

(defun dired-back-to-top ()
  (interactive)
  (goto-char (point-min))
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(define-key input-decode-map "\C-i" [C-i])

(defconst savefile-dir (expand-file-name "savefile" user-emacs-directory))

(use-package savehist
  :config
  (setq savehist-additional-variables
        ;; search entries
        '(search-ring regexp-search-ring)
        ;; save every minute
        savehist-autosave-interval 60
        ;; keep the home clean
        savehist-file (expand-file-name "savehist" savefile-dir))
  (savehist-mode +1))

(defun dired-jump-to-bottom ()
  (interactive)
  (goto-char (point-max))
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

(use-package highlight-symbol
  :ensure t
  :config
  (highlight-symbol-mode)
  (global-set-key (kbd "M-p") 'highlight-symbol-prev)
  (global-set-key (kbd "M-n") 'highlight-symbol-next))

(setq sql-indent-offset 2)

(use-package diff-hl
  :commands (diff-hl-mode diff-hl-dired-mode)
  :hook (magit-post-refresh . diff-hl-magit-post-refresh))

(use-package diff-hl-flydiff
  :commands diff-hl-flydiff-mode)

(use-package diffview
  :commands (diffview-current diffview-region diffview-message))

(use-package f)

(use-package dired
  :bind
  (("C-c J" . dired-double-jump)
   ("C-x d" . dired-jump))
  :bind (:map dired-mode-map
              ("z"     . delete-window)
              ("e"     . ora-ediff-files)
              ("l"     . dired-up-directory)
              ("Y"     . ora-dired-rsync)
              ("<tab>" . my-dired-switch-window)
              ("M-!"   . async-shell-command)
              ("M-G"))
  :preface
  (defvar mark-files-cache (make-hash-table :test #'equal))

  (defun mark-similar-versions (name)
    (let ((pat name))
      (if (string-match "^\\(.+?\\)-[0-9._-]+$" pat)
          (setq pat (match-string 1 pat)))
      (or (gethash pat mark-files-cache)
          (ignore (puthash pat t mark-files-cache)))))

  (defun dired-mark-similar-version ()
    (interactive)
    (setq mark-files-cache (make-hash-table :test #'equal))
    (dired-mark-sexp '(mark-similar-versions name)))

  (defun dired-double-jump (first-dir second-dir)
    (interactive
     (list (read-directory-name "First directory: "
                                (expand-file-name "~")
                                nil nil "dl/")
           (read-directory-name "Second directory: "
                                (expand-file-name "~")
                                nil nil "Archives/")))
    (dired first-dir)
    (dired-other-window second-dir))

  (defun my-dired-switch-window ()
    (interactive)
    (if (eq major-mode 'sr-mode)
        (call-interactively #'sr-change-window)
      (call-interactively #'other-window)))

  (defun ora-dired-rsync (dest)
    (interactive
     (list
      (expand-file-name
       (read-file-name "Rsync to: " (dired-dwim-target-directory)))))
    (let ((files (dired-get-marked-files
                  nil current-prefix-arg))
          (tmtxt/rsync-command
           "rsync -arvz --progress "))
      (dolist (file files)
        (setq tmtxt/rsync-command
              (concat tmtxt/rsync-command
                      (shell-quote-argument file)
                      " ")))
      (setq tmtxt/rsync-command
            (concat tmtxt/rsync-command
                    (shell-quote-argument dest)))
      (async-shell-command tmtxt/rsync-command "*rsync*")
      (other-window 1)))

  (defun ora-ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
          (wnd (current-window-configuration)))
      (if (<= (length files) 2)
          (let ((file1 (car files))
                (file2 (if (cdr files)
                           (cadr files)
                         (read-file-name
                          "file: "
                          (dired-dwim-target-directory)))))
            (if (file-newer-than-file-p file1 file2)
                (ediff-files file2 file1)
              (ediff-files file1 file2))
            (add-hook 'ediff-after-quit-hook-internal
                      `(lambda ()
                         (setq ediff-after-quit-hook-internal nil)
                         (set-window-configuration ,wnd))))
        (error "no more than 2 files should be marked"))))

  :config
  (ignore-errors
    (unbind-key "M-s f" dired-mode-map))

  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Make sure to remove \"Omit\" from the modeline."
    (diminish 'dired-omit-mode) dired-mode-map)

  (defadvice dired-next-line (around dired-next-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and  (not  (eobp)) (not ad-return-value))
      (forward-line)
      (setq ad-return-value(dired-move-to-filename)))
    (when (eobp)
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename))))

  (defadvice dired-previous-line (around dired-previous-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and  (not  (bobp)) (not ad-return-value))
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename)))
    (when (bobp)
      (call-interactively 'dired-next-line)))

  (defvar dired-omit-regexp-orig (symbol-function 'dired-omit-regexp))

  ;; Omit files that Git would ignore
  (defun dired-omit-regexp ()
    (let ((file (expand-file-name ".git"))
          parent-dir)
      (while (and (not (file-exists-p file))
                  (progn
                    (setq parent-dir
                          (file-name-directory
                           (directory-file-name
                            (file-name-directory file))))
                    ;; Give up if we are already at the root dir.
                    (not (string= (file-name-directory file)
                                  parent-dir))))
        ;; Move up to the parent dir and try again.
        (setq file (expand-file-name ".git" parent-dir)))
      ;; If we found a change log in a parent, use that.
      (if (file-exists-p file)
          (let ((regexp (funcall dired-omit-regexp-orig))
                (omitted-files
                 (shell-command-to-string "git clean -d -x -n")))
            (if (= 0 (length omitted-files))
                regexp
              (concat
               regexp
               (if (> (length regexp) 0)
                   "\\|" "")
               "\\("
               (mapconcat
                #'(lambda (str)
                    (concat
                     "^"
                     (regexp-quote
                      (substring str 13
                                 (if (= ?/ (aref str (1- (length str))))
                                     (1- (length str))
                                   nil)))
                     "$"))
                (split-string omitted-files "\n" t)
                "\\|")
               "\\)")))
        (funcall dired-omit-regexp-orig)))))

(use-package dired-narrow
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))

(use-package dired-ranger
  :bind (:map dired-mode-map
              ("W" . dired-ranger-copy)
              ("X" . dired-ranger-move)
              ("Y" . dired-ranger-paste)))

(use-package discover
  :disabled t
  :defer 5
  :commands global-discover-mode
  :hook (dired-mode . dired-turn-on-discover)
  :config
  (global-discover-mode 1))

(use-package docker
  :defer 15
  :diminish
  :config
  (require 'docker-images)
  (require 'docker-containers)
  (require 'docker-volumes)
  (require 'docker-networks))

(use-package sh-mode
  :init
  (setq sh-basic-offset 2)
  (setq sh-basic-indentation 2)
  :mode ("\\.bats$" . sh-mode))

(use-package vkill
  :commands vkill
  :preface
  :config
  (setq vkill-show-all-processes t))

(use-package yasnippet
  :diminish
  :config
  (yas-reload-all))

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(add-hook 'find-file-hook (lambda () (global-font-lock-mode -1)))

(use-package emmet-mode
  :init
  (setq emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode))

(setq uniquify-strip-common-suffix nil)

(add-to-list 'completion-ignored-extensions ".test")

(defun tj-disable-final-newline ()
  (interactive)
  (set (make-local-variable 'require-final-newline) nil))

(add-to-list 'exec-path "~/dev/bin")
(add-to-list 'exec-path "~/bin")


(defun magit-key-mode--add-default-options (arguments)
  (if (eq (car arguments) 'pulling)
      (list 'pulling (list "--rebase"))
    arguments)

  (if (eq (car arguments) 'pushing)
      (list 'pushing (list "-u"))
    arguments)
  )

(advice-add 'magit-key-mode :filter-args #'magit-key-mode--add-default-options)

(require 'window-number)
(setq window-number-active-background "grey")
(setq window-number-active-foreground "black")
(setq window-number-inactive-background "grey")
(setq window-number-inactive-foreground "black")
(window-number-meta-mode)

;;; Keymaps

(define-key input-decode-map [?\C-m] [C-m])

(eval-and-compile
  (mapc #'(lambda (entry)
            (define-prefix-command (cdr entry))
            (bind-key (car entry) (cdr entry)))
        '(("<C-m>" . my-ctrl-m-map)

          ("C-h e" . my-ctrl-h-e-map)

          ("C-c e" . my-ctrl-c-e-map)
          ("C-c m" . my-ctrl-c-m-map)
          ("C-c y" . my-ctrl-c-y-map)

          ("M-s =" . my-m-s-equals-map))))

(eval-after-load "prelude-mode"
  '(progn
     (define-key prelude-mode-map (kbd "C-c s") nil)
     (define-key prelude-mode-map (kbd "M-o") nil)
     (define-key prelude-mode-map (kbd "s-g") nil)
     (define-key prelude-mode-map (kbd "C-c G") nil)
     (define-key prelude-mode-map (kbd "C-c o") nil)
     (define-key prelude-mode-map (kbd "C-c C-i") nil)
     (define-key prelude-mode-map (kbd "s-o") nil)
     (define-key prelude-mode-map (kbd "C-c g") nil)
     (define-key prelude-mode-map (kbd "C-c t") nil)
     (define-key prelude-mode-map (kbd "C-c i") nil)))

(defun tj-comment-line ()
  (interactive)
  (call-interactively #'comment-line)
  (unless (region-active-p) (previous-line)))
(global-set-key (kbd "M-;") 'tj-comment-line)

(setq comment-multi-line t)
(setq-default css-indent-offset 2)

(setq-default indent-tabs-mode nil)

(global-set-key (kbd "s-s") 'save-buffer)

(add-to-list 'vc-directory-exclusion-list "node_modules")

(use-package avy
  :bind (("C-j" . avy-goto-char-timer))
  :config
  (avy-setup-default))

(use-package avy-zap
  :bind
  (("M-Z" . avy-zap-up-to-char-dwim)))

(use-package backup-walker
  :commands backup-walker-start)

(use-package centered-cursor-mode
  :commands centered-cursor-mode)

(use-package change-inner
  :bind (("M-i"     . change-inner)
         ("M-o" . change-outer)
         ("s-i" . copy-inner)
         ("s-o" . copy-outer)))

(add-hook 'cider-mode-hook #'eldoc-mode)

(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)

(defun tj-visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-push-remote)
                       "url"))
           (magit-get-current-branch))))

(eval-after-load 'magit
  '(define-key magit-mode-map "v"
     #'tj-visit-pull-request-url))

(use-package undo-tree
  ;; jww (2017-12-10): This package often breaks the ability to "undo in
  ;; region". Also, its backup files often get corrupted, so this sub-feature
  ;; is disabled in settings.el.
  :demand t
  :bind ("M-_" . undo-tree-redo)
  :config
  (setq undo-tree-history-directory-alist (quote ((".*" . "~/.cache/emacs/backups"))))
  (setq undo-tree-mode-lighter "")
  (setq undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

(use-package color-moccur
  :commands (isearch-moccur isearch-all isearch-moccur-all)
  :bind (("M-s O" . moccur)
         :map isearch-mode-map
         ("M-o" . isearch-moccur)
         ("M-O" . isearch-moccur-all)))

(use-package moccur-edit
  :after color-moccur)

(use-package eyebrowse)

(use-package eshell
  :commands (eshell eshell-command)
  :preface
  (defvar eshell-isearch-map
    (let ((map (copy-keymap isearch-mode-map)))
      (define-key map [(control ?m)] 'eshell-isearch-return)
      (define-key map [return]       'eshell-isearch-return)
      (define-key map [(control ?r)] 'eshell-isearch-repeat-backward)
      (define-key map [(control ?s)] 'eshell-isearch-repeat-forward)
      (define-key map [(control ?g)] 'eshell-isearch-abort)
      (define-key map [backspace]    'eshell-isearch-delete-char)
      (define-key map [delete]       'eshell-isearch-delete-char)
      map)
    "Keymap used in isearch in Eshell.")

  (defun tj-eshell-here ()
    (interactive)
    (eshell (f-dirname (buffer-file-name))))

  (defun eshell-initialize ()
    (defun eshell-spawn-external-command (beg end)
      "Parse and expand any history references in current input."
      (save-excursion
        (goto-char end)
        (when (looking-back "&!" beg)
          (delete-region (match-beginning 0) (match-end 0))
          (goto-char beg)
          (insert "spawn "))))

    (add-hook 'eshell-expand-input-functions 'eshell-spawn-external-command)

    (use-package em-unix
      :defer t
      :config
      (unintern 'eshell/su nil)
      (unintern 'eshell/sudo nil)))

  :init
  (add-hook 'eshell-first-time-mode-hook 'eshell-initialize)
  (require 'em-smart)

  :config
  (defun tj-eshell()
    (interactive)
    (if (projectile-project-p)
        (call-interactively 'projectile-run-eshell)
      (eshell)))

  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)

  (defun tj-eshell-mode-hook ()
    (define-key eshell-mode-map (kbd "M-r") 'counsel-esh-history)
    (setenv "PATH" (concat "/usr/local/go/bin:" "/usr/local/bin:" (getenv "PATH")))
    (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env)))

  (add-hook 'eshell-mode-hook 'tj-eshell-mode-hook)
  :bind
  (("C-x m" . tj-eshell)))

(use-package eshell-bookmark
  :hook (eshell-mode . eshell-bookmark-setup))

(use-package eshell-up
  :commands eshell-up)

(use-package eshell-z
  :after eshell)

(use-package fancy-narrow
  :bind (("M-s M-n" . fancy-narrow-to-region)
         ("M-s M-N" . fancy-widen))
  :commands (fancy-narrow-to-region fancy-widen))

(use-package magit
  :init
  (setq magit-push-always-verify nil)

  :bind (:map magit-diff-mode-map
              (("C-o" . magit-diff-visit-file-other-window)))

  :config
  ;; (defun tj-magit-push-current-to-upstream (orig-fun &rest args)
  ;;   (apply orig-fun args)
  ;;   (message "Pushed!"))
  ;; (advice-add 'magit-push-current-to-upstream :around #'tj-magit-push-current-to-upstream)

  ;; (setq magit-popup-use-prefix-argument 'default)
  ;; (plist-put magit-push-popup :default-action 'magit-push-current-to-upstream)
  ;; (plist-put magit-pull-popup :default-action 'magit-pull-from-upstream)
  ;; (plist-put magithub-dispatch-popup :default-action 'magithub-pull-request-new)
  ;; (plist-put magit-commit-popup :use-prefix 'popup)
  ;; (plist-put magit-log-popup :use-prefix 'popup)
  ;; (plist-put magit-push-popup :use-prefix 'popup)
  ;; (plist-put magit-pull-popup :use-prefix 'popup)
  ;; (plist-put magithub-dispatch-popup :use-prefix 'popup)

  (global-unset-key [tab]))

(use-package smart-forward
  :config
  :bind
  (("M-<up>" . smart-up)
   ("M-<down>" . smart-down)
   ("M-<left>" . smart-backward)
   ("M-<right>" . smart-forward)))

(use-package magithub
  :after magit
  :config
  (magithub-feature-autoinject t)

  (magit-define-section-jumper magit-jump-to-pull-requests "Pull Requests" magithub-pull-requests-list)

  (define-key magit-status-mode-map "jpr" 'magit-jump-to-pull-requests)

  (defun tj-kill-issue-url (issue-or-pr)
    "Visits ISSUE-OR-PR in the browser.
Interactively, this finds the issue at point."
    (interactive (list (magithub-interactive-issue)))
    (when-let* ((url (alist-get 'html_url issue-or-pr)))
      (kill-new url)))

  (defun tj-visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (let ((repo (magit-get "remote" (magit-get-remote) "url")))
      (if (not repo)
          (setq repo (magit-get "remote" (magit-get-push-remote) "url")))
      (visit-gh-pull-request repo)))

  (defun visit-gh-pull-request (repo)
    "Visit the current branch's PR on Github."
    (interactive)
    (message repo)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
             (replace-regexp-in-string
              "\\`.+github\\.com:\\(.+\\)\\(\\.git\\)?\\'" "\\1"
              repo)
             (magit-get-current-branch))))

  ;; (dolist
  ;;     (item magithub-confirmation)
  ;;   (magithub-confirm-set-default-behavior (car item) 'allow t))
  :bind (:map magithub-issue-view-mode-map
              (("M-w" . tj-kill-issue-url))))

(use-package re-builder
  :bind (:map reb-mode-map
              ("M-%" . reb-query-replace))
  :config
  (defun reb-query-replace (to-string)
    "Replace current RE from point with `query-replace-regexp'."
    (interactive
     (progn (barf-if-buffer-read-only)
            (list (query-replace-read-to (reb-target-binding reb-regexp)
                                         "Query replace"  t))))
    (with-current-buffer reb-target-buffer
      (query-replace-regexp (reb-target-binding reb-regexp) to-string))))



(use-package dockerfile-mode
  :mode "Dockerfile[a-zA-Z.-]*\\'")

(use-package edit-indirect
  :bind (("C-c '" . edit-indirect-region)))

(use-package hcl-mode)

(use-package restclient
  :mode
  ("\\.rest\\'" . restclient-mode)
  :config
  (defun my-response-loaded-hook ()
    (flycheck-mode -1))
  (add-hook 'restclient-response-loaded-hook 'my-response-loaded-hook)
  (defun my-restclient-hook ()
    (setq-local indent-line-function 'js-indent-line))
  (add-hook 'restclient-mode-hook 'my-restclient-hook))

(use-package osx-clipboard

  :config
  (osx-clipboard-mode))

(use-package ediff
  :bind (("M-s = b" . ediff-buffers)
         ("M-s = B" . ediff-buffers3)
         ("M-s = c" . compare-windows)
         ("M-s = =" . ediff-files)
         ("M-s = f" . ediff-files)
         ("M-s = F" . ediff-files3)
         ("M-s = r" . ediff-revision)
         ("M-s = p" . ediff-patch-file)
         ("M-s = P" . ediff-patch-buffer)
         ("M-s = l" . ediff-regions-linewise)
         ("M-s = w" . ediff-regions-wordwise))
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

(use-package git-link
  :bind ("M-s G" . git-link)
  :commands (git-link git-link-commit git-link-homepage))

(use-package git-timemachine
  :commands git-timemachine)

(use-package gitattributes-mode
  :defer 5)

(use-package gitconfig-mode
  :defer 5)

(use-package github-pullrequest
  :commands (github-pullrequest-new
             github-pullrequest-checkout))

(use-package gitignore-mode
  :defer 5)

(use-package gitpatch
  :commands gitpatch-mail)

(use-package google-this
  :bind (("M-s /" . google-this-search)))

(use-package goto-last-change
  :bind ("C-x C-/" . goto-last-change))

(use-package ialign
  :bind ("M-s [" . ialign-interactive-align))

(use-package operate-on-number
  :bind ("M-s '" . operate-on-number-at-point))

(use-package shift-number
  :bind (("M-s +" . shift-number-up)
         ("M-s -" . shift-number-down)))

(use-package swiper
  :diminish
  :after ivy
  :bind (:map swiper-map
              ("M-y" . yank)
              ("M-%" . swiper-query-replace)
              ("C-'" . isearch-forward-regexp)
              ("M-h" . swiper-avy)
              ("M-c" . swiper-mc))
  :commands swiper-from-isearch
  :init
  (bind-keys :map isearch-mode-map ("M-s" . swiper-from-isearch)))

(use-package word-count
  :bind ("M-s W" . word-count-mode))

(use-package company-tern

  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-tern)))

(use-package js2-mode

  :init
  (setq js-indent-level 2)
  (setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
  (setq-default js2-strict-inconsistent-return-warning nil)

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist)))


  ;; :mode
  ;; ("\\.js$" . js2-jsx-mode)
  ;; ("\\.js\\'" . js2-jsx-mode)
  ;; ("\\.json\\'" . js2-jsx-mode)

  :interpreter ("node" . js2-jsx-mode)
  :bind
  ("M-j" . comment-indent-new-line)
  ("C-c C-j" . js2-jump-to-definition)
  ("M-." . tern-find-definition)
  :config
  (defun js2-match-async-arrow-function ()
    (when (and (js2-contextual-kwd-p (js2-current-token) "async")
               (/= (js2-peek-token) js2-FUNCTION)
               (/= (js2-peek-token) js2-DOT))
      (js2-record-face 'font-lock-keyword-face)
      (js2-get-token)
      t))
  (defun my-js2-mode-hook ()
    (electric-indent-mode 1)
    (tern-mode)
    (flycheck-mode)
    (subword-mode))
  (add-hook 'js2-mode-hook 'my-js2-mode-hook))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(yas-global-mode)

(use-package web-mode

  :init
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-auto-opening t)
  (setq web-mode-enable-auto-pairing t)
  (add-to-list 'web-mode-comment-formats '("jsx" . "//"))
  (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
  (setq web-mode-tag-auto-close-style 2)
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")))
  (setq web-mode-engines-alist
        '(("reactjs" . "\\.js$")))
  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))
  (setq tj--javascript-common-imenu-regex-list
        '(("Controller" "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
          ("Module" "[. \t]module( *['\"]\\([a-zA-Z0-9_.]+\\)['\"], *\\[" 1)
          ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
          ("Class" "class[ \t]+\\([a-zA-Z_.]+\\)" 1)
          ("Constant" "const[ \t]+\\([a-zA-Z_.]+\\)" 1)
          ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
          ))
  (defun tj--js-imenu-make-index ()
    (save-excursion
      (imenu--generic-function tj--javascript-common-imenu-regex-list)))
  (defun tj-web-mode-hook nil
    (subword-mode)
    (tern-mode)

    (setq comment-start "//"
          comment-end   "")
    (setq-local imenu-create-index-function 'tj-js-imenu-make-index))
  (add-hook 'web-mode-hook #'tj-web-mode-hook)
  :mode
  ("\\.hbs$" . web-mode)
  ("\\.eex$" . web-mode)
  ("\\.js$" . web-mode)
  :config
  (defun tjhtml-insert-open-and-close-tag ()
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
               (yas/expand-snippet (concat "<" tag " $1>$0")))
              (t
               (yas/expand-snippet
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
  (defun tjerb-insert-or-toggle-erb-tag ()
    "Insert an ERb tag if the point isn't currently in one, or toggle the type."
    (interactive)
    (let ((action))
      (if (looking-at "[\s\t\n]*<%")
          (setq action 'insert)
        (save-excursion
          (let ((regex (concat "\\`<%.*%>\\'")))
            (while (or (not (region-active-p))
                       (not (or (and (= (point-min) (region-beginning))
                                     (= (point-max) (region-end)))
                                (string-match regex (buffer-substring-no-properties
                                                     (region-beginning)
                                                     (region-end))))))
              (let ((expand-region-fast-keys-enabled))
                (er/expand-region 1)))
            (let ((matched (buffer-substring-no-properties (region-beginning)
                                                           (region-end))))
              (if (string-match regex matched)
                  (progn (goto-char (+ (if (< (point) (mark)) (point) (mark)) 2))
                         (cond ((looking-at "=")
                                (delete-char 1))
                               ((looking-at "#")
                                (delete-char 1)
                                (insert "="))
                               (t
                                (insert "#"))))
                (setq action 'insert))))))
      (if (eq action 'insert)
          (progn (insert "<%=  %>")
                 (backward-char 3)))))
  :bind
  ("M-." . tern-find-definition)
  ("C-c >" . tjerb-insert-or-toggle-erb-tag)
  ("C-c <" . tjhtml-insert-open-and-close-tag))

(use-package ag
  :config
  (defun tj-ag-here (arg)
    (interactive "sSearch string: ")
    (ag-regexp arg default-directory))

  (global-set-key (kbd "C-c C-a") 'ag-regexp))

(defun goimports ()
  "Formats the current buffer according to the goimports tool."
  (interactive)
  (let ((tmpfile (make-temp-file "goimports" nil ".go"))
        (patchbuf (get-buffer-create "*Goimports patch*"))
        (errbuf (get-buffer-create "*Goimports Errors*"))
        (coding-system-for-read 'utf-8)
        (coding-system-for-write 'utf-8))

    (with-current-buffer errbuf
      (setq buffer-read-only nil)
      (erase-buffer))
    (with-current-buffer patchbuf
      (erase-buffer))

    (write-region nil nil tmpfile)

    ;; We're using errbuf for the mixed stdout and stderr output. This
    ;; is not an issue because goimports -w does not produce any stdout
    ;; output in case of success.
    (if (zerop (call-process "goimports" nil errbuf nil "-w" tmpfile))
        (if (zerop (call-process-region (point-min) (point-max) "diff" nil patchbuf nil "-n" "-" tmpfile))
            (progn
              (kill-buffer errbuf)
              (message "Buffer is already goimportsed"))
          (go--apply-rcs-patch patchbuf)
          (kill-buffer errbuf)
          (message "Applied goimports"))
      (message "Could not apply goimports. Check errors for details")
      (goimports--process-errors (buffer-file-name) tmpfile errbuf))

    (kill-buffer patchbuf)
    (delete-file tmpfile)))

(defun goimports--process-errors (filename tmpfile errbuf)
  ;; Convert the goimports stderr to something understood by the compilation mode.
  (with-current-buffer errbuf
    (goto-char (point-min))
    (insert "goimports errors:\n")
    (while (search-forward-regexp (concat "^\\(" (regexp-quote tmpfile) "\\):") nil t)
      (replace-match (file-name-nondirectory filename) t t nil 1))
    (compilation-mode)
    (display-buffer errbuf)))

(use-package company-quickhelp
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

(use-package python-mode)

(eval-after-load 'company
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

(use-package company-go
  :defer t
  :config
  (set (make-local-variable 'company-backends)
       '((company-dabbrev-code company-go))))

(use-package go-eldoc
  :defer t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-mode
  :defer t
  :init
  (load "~/dev/src/github.com/stapelberg/expanderr/expanderr.el")
  (setq go-test-verbose t)
  (setq gofmt-command "goimports")
  (setenv "GOPATH" (expand-file-name (concat (getenv "HOME") "/dev")))
  (setenv "GOROOT" "/usr/local/go")
  (setq company-go-show-annotation t)
  (eval-after-load "go-mode"
    '(progn
       (define-key go-mode-map (kbd "C-c C-a") nil)))
  :bind (:map go-mode-map
              ("M-b" . subword-backward)
              ("M-f" . subword-forward))
  :bind
  ("C-c C-d" . go-guru-describe)
  ("C-c C-i" . goimports)
  ("C-c C-r" . godoctor-rename)
  ("C-c C-c" . godoc-at-point)
  ("C-c C-t" . go-test-current-file)
  ("C-c g" . godoc)
  ("M-j" . comment-indent-new-line)
  ("M-." . go-guru-definition)
  ("C-," . go-guru-definition-other-window)
  ("C-c <C-m>" . tj-go-kill-doc)

  :config
  (setq tab-width 8)

  (setq-local compilation-read-command nil)

  (defun tj-go-find-file ()
    "Find file under $GOROOT."
    (interactive)
    (counsel-find-file "/usr/local/go/src/"))

  (use-package go-errcheck
    :config
    (defun tj-go-errcheck ()
      (interactive)
      (let ((default-directory (projectile-project-root)))
        (go-errcheck nil nil nil))))
  (add-hook 'before-save-hook 'gofmt-before-save)

  (defun tj-go-kill-doc ()
    "Kill the doc for the thing at point."
    (interactive)
    (let ((funcinfo (go-eldoc--get-funcinfo)))
      (if funcinfo
          (go-eldoc--format-signature funcinfo)
        (let ((bounds (go-eldoc--bounds-of-go-symbol)))
          (when bounds
            (let ((curinfo (go-eldoc--get-cursor-info bounds)))
              (when curinfo
                (kill-new (format "%s" curinfo))
                (message (format "killed: %s" curinfo)))))))))

  (defun my-go-hook ()
    (setq display-line-numbers nil)
    (which-function-mode)
    (highlight-symbol-mode)
    (subword-mode)
    (flycheck-mode)
    (electric-indent-mode)
    (selected-minor-mode 1)
    (go-guru-hl-identifier-mode)
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet")))
  (add-hook 'go-mode-hook 'my-go-hook)
  (use-package gotest))

(use-package eacl
  :config
  (setq eacl-grep-program "grep --exclude-dir=.git --exclude-dir=vendor")
  :bind
  (("C-x C-l" . eacl-complete-line)))

(use-package go-gen-test
  :after go-mode)

(use-package embrace
  :config
  (setq  embrace-show-help-p nil)
  :bind
  (("M-s M-e" . embrace-commander)))

(use-package iy-go-to-char
  :bind
  (("M-s M-j" . iy-go-up-to-char)
   ("M-s M-J" . iy-go-to-char-backward)))

(use-package bm
  :bind (("M-s b" . bm-toggle)
         ("M-s ." . bm-next)
         ("M-s ," . bm-previous))
  :commands (bm-repository-load
             bm-buffer-save
             bm-buffer-save-all
             bm-buffer-restore)
  :init
  (add-hook' after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save))))
(use-package projectile
  :config
  (setq projectile-switch-project-action #'projectile-commander)
  (add-to-list 'projectile-globally-ignored-directories "Godeps/_workspace")
  ;; (add-to-list 'projectile-globally-ignored-directories "_build")
  (add-to-list 'projectile-globally-ignored-directories "deps")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")

  (eval-after-load "projectile"
    '(progn (setq magit-repo-dirs (mapcar (lambda (dir)
                                            (substring dir 0 -1))
                                          (remove-if-not (lambda (project)
                                                           (file-directory-p (concat project "/.git/")))
                                                         (projectile-relevant-known-projects))))
            (setq magit-repo-dirs-depth 1)))

  (def-projectile-commander-method ?e "Open eshell in root directory." (call-interactively 'projectile-run-eshell))
  (def-projectile-commander-method ?a "Run ag in the project." (let ((current-prefix-arg 1)) (call-interactively 'projectile-ag)))
  (def-projectile-commander-method ?c "Compile project." (call-interactively 'projectile-compile-project))
  (def-projectile-commander-method ?d "Open project root in dired." (call-interactively 'projectile-dired))
  (def-projectile-commander-method ?t "Test project." (call-interactively 'projectile-test-project))
  (def-projectile-commander-method ?G "Open ile in git." (call-interactively 'github-browse-file))

  (def-projectile-commander-method ?d "Open project root in dired." (call-interactively 'projectile-dired))
  (def-projectile-commander-method ?u
    "Git fetch."
    (magit-status)
    (if (fboundp 'magit-fetch-from-upstream)
        (call-interactively #'magit-fetch-from-upstream)
      (call-interactively #'magit-fetch-current)))
  (def-projectile-commander-method ?p
    "Git push."
    (magit-status)
    (if (fboundp 'magit-push-current-to-upstream)
        (call-interactively #'magit-push-current-to-upstream)
      (call-interactively #'magit-push-current)))

  :bind
  (
   ("C-c t" . projectile-toggle-between-implementation-and-test)
   ("C-c C-p" . projectile-test-project)
   ("M-m" . projectile-commander)
   ("C-c P" . 'projectile-switch-project)))

(global-set-key (kbd "C-c c") #'embrace-commander)

;; unset text scaling key bindings, use 'text-scale-adjust instead
(global-unset-key (kbd "C--"))
(global-unset-key (kbd "C-+"))

(use-package web-beautify)

(use-package notmuch
  :config
  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "/usr/local/bin/msmtp")
  (setq message-sendmail-extra-arguments '("--read-envelope-from"))
  (setq message-sendmail-f-is-evil 't)
  (setq notmuch-fcc-dirs "G/[Gmail]Sent Mail")
  (setq message-directory "G/[Gmail]Drafts"))

(defun tj-esformatter ()
  (interactive)
  (shell-command (format "esformatter -i %s" (buffer-file-name))))

(use-package whitespace
  :diminish)

(defun zap-to-isearch (rbeg rend)
  "Kill the region between the mark and the closest portion of
  the isearch match string. The behaviour is meant to be analogous
  to zap-to-char; let's call it zap-to-isearch. The deleted region
  does not include the isearch word. This is meant to be bound only
  in isearch mode.
  The point of this function is that oftentimes you want to delete
  some portion of text, one end of which happens to be an active
  isearch word. The observation to make is that if you use isearch
  a lot to move the cursor around (as you should, it is much more
  efficient than using the arrows), it happens a lot that you could
  just delete the active region between the mark and the point, not
  include the isearch word."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (let* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds))
         )
    (if (< (mark) ismin)
        (kill-region (mark) ismin)
      (if (> (mark) ismax)
          (kill-region ismax (mark))
        (error "Internal error in isearch kill function.")))
    (isearch-exit)))
(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)

(use-package sql
  :init
  (defun my-sql-mode-hook ()
    (electric-indent-mode -1))
  (add-hook 'sql-mode-hook 'my-sql-mode-hook)

  :config
  (eval-after-load "sql"
    '(load-library "sql-indent")))

(defun tjtitle-case-region-or-line (*begin *end)
  "Title case text between nearest brackets, or current line, or text selection.
Capitalize first letter of each word, except words like {to, of, the, a, in, or, and, …}. If a word already contains cap letters such as HTTP, URL, they are left as is.

When called in a elisp program, *begin *end are region boundaries.
URL `http://ergoemacs.org/emacs/elisp_title_case_text.html'
Version 2015-05-07"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (let (
           -p1
           -p2
           (-skipChars "^\"<>(){}[]“”‘’‹›«»「」『』【】〖〗《》〈〉〔〕"))
       (progn
         (skip-chars-backward -skipChars (line-beginning-position))
         (setq -p1 (point))
         (skip-chars-forward -skipChars (line-end-position))
         (setq -p2 (point)))
       (list -p1 -p2))))
  (let* (
         (-strPairs [
                     [" A " " a "]
                     [" And " " and "]
                     [" At " " at "]
                     [" As " " as "]
                     [" By " " by "]
                     [" Be " " be "]
                     [" Into " " into "]
                     [" In " " in "]
                     [" Is " " is "]
                     [" It " " it "]
                     [" For " " for "]
                     [" Of " " of "]
                     [" Or " " or "]
                     [" On " " on "]
                     [" Via " " via "]
                     [" The " " the "]
                     [" That " " that "]
                     [" To " " to "]
                     [" Vs " " vs "]
                     [" With " " with "]
                     [" From " " from "]
                     ["'S " "'s "]
                     ]))
    (save-excursion
      (save-restriction
        (narrow-to-region *begin *end)
        (upcase-initials-region (point-min) (point-max))
        (let ((case-fold-search nil))
          (mapc
           (lambda (-x)
             (goto-char (point-min))
             (while
                 (search-forward (aref -x 0) nil t)
               (replace-match (aref -x 1) 'FIXEDCASE 'LITERAL)))
           -strPairs))))))



(use-package terraform-mode
  :config
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode))

(use-package multifiles
  :bind
  ("C-!" . mf/mirror-region-in-multifiles))

(defun tj-counsel-ag ()
  (interactive)
  (counsel-ag nil (projectile-project-root)))

(defun tj-ag-regexp (string)
  (interactive "sSearch string: ")
  (ag-regexp string  (projectile-project-root)))

(defun tj-semaphore-open-branch ()
  "Open branch in Semaphore CI"
  (interactive)
  (let* ((branch (magit-get-current-branch))
         (group
          (thread-first (magit-get "remote" "origin" "url")
            (split-string ":")
            last
            first
            (split-string "\\.")
            first))
         (group
          (if (string-prefix-p "confluentinc" group)
              (replace-regexp-in-string "confluentinc" "confluent" group)
            group)))
    (browse-url (format "https://semaphoreci.com/%s/branches/%s" group branch))))

(eval-after-load 'magit
  '(define-key magit-mode-map "S"
     #'tj-semaphore-open-branch))

(use-package dash)

(use-package ivy
  :diminish
  :config
  (setq ivy-initial-inputs-alist nil)
  :bind
  (("M-R" . ivy-resume)))


(use-package copy-as-format
  :bind ("M-s M-w" . copy-as-format)
  :init
  (setq copy-as-format-default "github"))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package compile
  :no-require

  :bind (("C-c c" . compile)
         ("M-O"   . show-compilation))

  :bind (:map compilation-mode-map
              (("z" . delete-window)
               ("RET" . tj-compile-goto-error-same-window)))
  :bind (:map compilation-minor-mode-map
              ("RET" . tj-compile-goto-error-same-window))
  :bind (:map compilation-button-map
              ("RET" . tj-compile-goto-error-same-window))
  :bind (:map grep-mode-map
              ("RET" . tj-compile-goto-error-same-window))

  :preface

  (defun tj-compile-goto-error-same-window ()
    (interactive)
    (let ((display-buffer-overriding-action
           '((display-buffer-reuse-window
              display-buffer-same-window)
             (inhibit-same-window . nil))))
      (call-interactively #'compile-goto-error)))

  (defun show-compilation ()
    (interactive)
    (let ((it
           (catch 'found
             (dolist (buf (buffer-list))
               (when (string-match "\\*compilation\\*" (buffer-name buf))
                 (throw 'found buf))))))
      (if it
          (display-buffer it)
        (call-interactively 'compile))))

  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

  :hook ((compilation-filter . compilation-ansi-color-process-output)))

(use-package dired-toggle
  :bind ("M-s d" . dired-toggle)
  :preface
  (defun my-dired-toggle-mode-hook ()
    (interactive)
    (visual-line-mode 1)
    (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
    (setq-local word-wrap nil))
  :hook (dired-toggle-mode . my-dired-toggle-mode-hook))

(use-package auto-yasnippet
  :after yasnippet
  :bind (("C-c y a" . aya-create)
         ("C-c y e" . aya-expand)
         ("C-c y o" . aya-open-line)))

(use-package counsel
  :after ivy
  :demand t
  :diminish
  :bind
  (("C-*"     . counsel-org-agenda-headlines)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-g" . find-file-go)
   ("C-c e l" . counsel-find-library)
   ("C-c e q" . counsel-set-variable)
   ("C-h e l" . counsel-find-library)
   ("C-h e u" . counsel-unicode-char)
   ("C-h f"   . counsel-describe-function)
   ("C-x r b" . counsel-bookmark)
   ("M-x"     . counsel-M-x)
   ("C-x <C-m>"     . counsel-M-x)
   ("M-s f"     . tj-counsel-ag)
   ("M-s a"     . tj-ag-regexp)
   ("C-c i" . counsel-imenu)
   ("M-y" . counsel-yank-pop)
   ("M-s j" . counsel-dired-jump)
   ("M-s n" . counsel-file-jump))
  :commands counsel-minibuffer-history
  :init
  (bind-key "M-r" #'counsel-minibuffer-history minibuffer-local-map)
  :config

  (defun find-file-go (arg)
    (interactive "P")
    (let*
        ((pkg (or
               (and arg (read-string "PKG: "))
               (thing-at-point 'filename)))
         (dir (f-join (getenv "GOPATH") "src" pkg)))
      (projectile-find-file-in-directory dir)))

  (defun ag-go (arg)
    (interactive "P")
    (let*
        ((pkg (or
               (and arg (read-string "PKG: "))
               (thing-at-point 'filename)))
         (dir (f-join (getenv "GOPATH") "src" pkg))
         (search (read-string "Search string: ")))
      (ag search dir)))

  ;; (defun projectile-go-pkg (pkg)
  ;;   (interactive "sPKG: ")
  ;;   (let ((dir (f-join (getenv "GOPATH") "src" pkg)))
  ;;     (projectile-find-file-in-directory dir)))

  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-projectile-find-file . ivy--sort-files-by-date))

  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-find-file . ivy--sort-files-by-date)))

(use-package counsel-projectile
  :after counsel
  :config
  (setcdr (assoc 'counsel-projectile-find-file ivy-sort-functions-alist)
	  'file-newer-than-file-p)
  (setcdr (assoc 'counsel-projectile-switch-project ivy-sort-functions-alist)
	  'file-newer-than-file-p)
  :bind
  (("s-t" . counsel-projectile-find-file)
   ("C-c p f" . counsel-projectile-find-file)
   ("C-c p p" . counsel-projectile-switch-project)))

(use-package github-browse-file
  :bind
  (("M-s g" . github-browse-file)))

(use-package eval-expr
  :bind ("M-:" . eval-expr)
  :config
  (defun eval-expr-minibuffer-setup ()
    (local-set-key (kbd "<tab>") #'lisp-complete-symbol)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (paredit-mode)))

(use-package selected
  :diminish selected-minor-mode
  :bind (:map selected-keymap
              ("s-[" . align-code)
              ("s-f" . fill-region)
              ("s-U" . unfill-region)
              ("s-d" . downcase-region)
              ("s-u" . upcase-region)
              ("s-s" . sort-lines))
  :config
  (selected-global-mode 1))

(use-package ace-window
  :bind* ("<C-return>" . ace-window))

(use-package ace-mc
  :bind (("<C-m> h"   . ace-mc-add-multiple-cursors)
         ("<C-m> M-h" . ace-mc-add-single-cursor)))

(use-package diminish :demand t)

(use-package mc-extras
  :after multiple-cursors
  :bind (("<C-m> M-C-f" . mc/mark-next-sexps)
         ("<C-m> M-C-b" . mc/mark-previous-sexps)
         ("<C-m> C-d"   . mc/remove-current-cursor)
         ("<C-m> C-k"   . mc/remove-cursors-at-eol)
         ("<C-m> M-d"   . mc/remove-duplicated-cursors)
         ("<C-m> |"     . mc/move-to-column)
         ("<C-m> ~"     . mc/compare-chars)))

(use-package mc-freeze
  :after multiple-cursors
  :bind ("<C-m> f" . mc/freeze-fake-cursors-dwim))

(use-package mc-rect
  :after multiple-cursors
  :bind ("C-\"" . mc/rect-rectangle-to-multiple-cursors))

(use-package multiple-cursors
  :defer 5
  :after selected
  :preface
  (defun reactivate-mark ()
    (interactive)
    (activate-mark))
  :bind (
         ("<C-m> e" . mc/edit-lines)
         ("<C-m> C-e" . mc/edit-ends-of-lines)
         ("<C-m> C-a" . mc/edit-beginnings-of-lines)
         ("<C-m> a" . mc/mark-all-dwim)
         ("<C-m> <" . mc/mark-previous-like-this)
         ("<C-m> >" . mc/mark-next-like-this)
         ;; Extra multiple cursors stuff
         ("<C-m> %" . mc/insert-numbers)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package ace-jump-mode
  :defer t)

(use-package browse-url
  :bind
  (("C-c x" . browse-url-at-point)))

(use-package deft
  :commands deft
  :bind
  (("C-x D" . deft))
  :config
  (setq deft-directory "~/Dropbox/org")
  (setq deft-extensions '("org" "md"))
  (setq deft-default-extension "org")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 0))

(use-package org-mode
  :defer t
  :init
  (setq org-default-notes-file (expand-file-name "~/Dropbox/org/capture.org"))

  (setq org-startup-folded nil)

  (defun tj-org-capture ()
    (interactive)
    (find-file org-default-notes-file))
  :bind
  (("C-c o" . tj-org-capture))
  :config
  (setq org-src-tab-acts-natively t)
  (setq org-default-notes-file (expand-file-name "~/Dropbox/org/capture.org"))
  (setq org-directory (expand-file-name "~/Dropbox/org"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-use-speed-commands t) )

(use-package org-brain
  :config
  (setq org-id-track-globally t)
  (setq org-id-locations-file "~/.emacs.d/.org-id-locations")
  (setq org-brain-visualize-default-choices 'all)
  (setq org-brain-title-max-length 12)
  (setq org-brain-path (expand-file-name "~/Dropbox/org/brain")))

(use-package org-journal
  :defer t
  :config
  (setq org-journal-dir "~/Dropbox/journal"))

(use-package org-web-tools
  :defer t)

(use-package helpful-
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))

(use-package alert
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

(use-package ace-link
  :defer 10
  :config
  (ace-link-setup-default)

  (add-hook 'org-mode-hook
            #'(lambda () (bind-key "C-c C-o" #'ace-link-org org-mode-map)))
  (add-hook 'gnus-summary-mode-hook
            #'(lambda () (bind-key "M-o" #'ace-link-gnus gnus-summary-mode-map)))
  (add-hook 'gnus-article-mode-hook
            #'(lambda () (bind-key "M-o" #'ace-link-gnus gnus-article-mode-map)))
  (add-hook 'ert-results-mode-hook
            #'(lambda () (bind-key "o" #'ace-link-help ert-results-mode-map)))

  (bind-key "C-c M-o" 'ace-link-addr))

(use-package ace-mc
  :bind (("<C-m> h"   . ace-mc-add-multiple-cursors)
         ("<C-m> M-h" . ace-mc-add-single-cursor)))

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

(defun tj-insert-org-to-md-table (table-name)
  (interactive "*sEnter table name: ")
  (insert "<!---
#+ORGTBL: SEND " table-name " orgtbl-to-gfm

-->
<!--- BEGIN RECEIVE ORGTBL " table-name " -->
<!--- END RECEIVE ORGTBL " table-name " -->")
  (previous-line)
  (previous-line)
  (previous-line))

(defun tj-generalized-shell-command (command arg)
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

(defun tj-comment-eclipse ()
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
(global-set-key (kbd "M-;") 'tj-comment-eclipse)

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

(defun tj-newline-and-indent-up ()
  (interactive)
  (line-move -1)
  (end-of-line)
  (newline-and-indent))

(defun tj-find-config ()
  "Find a personal config file to edit."
  (interactive)
  (find-file "~/.emacs.d/personal/config.el"))

(defun tj-only-buffer ()
  (interactive)
  (mapc 'kill-buffer (cdr (buffer-list (current-buffer)))))

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

(require 'f)
(require 'eshell)

(defun eshell/goto (&optional repo)
  "cd to `repo', cloning if necessary."
  (interactive)
  (let ((segment-path (concat "~/dev/segmentio/" repo))
        (dev-path (concat "~/dev/" repo)))
    (if (file-exists-p segment-path)
        (eshell/cd segment-path)
      (if (file-exists-p dev-path)
          (eshell/cd dev-path)))))

(defun eshell/clear ()
  "Clear eshell's buffer.'"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(defun bundler--installed? ()
  "`t' if the bundle is available in the working directory."
  (equal (call-process-shell-command "which bundle > /dev/null 2>&1") 0))

(defun bundler--in-bundled-project? ()
  "`t' if the project is bundled."
  (f-traverse-upwards
   (lambda (path)
     (f-exists? (f-join path "Gemfile")))
   default-directory))

(defun eshell-execute-current-line ()
  "Insert current line at the end of the buffer."
  (interactive)
  (let ((command (buffer-substring
                  (save-excursion
                    (beginning-of-line)
                    (point))
                  (save-excursion
                    (end-of-line)
                    (point)))))
    (eshell--execute-command command t)))

(defun eshell-insert-command (text &optional func)
  "Insert a command at the end of the buffer."
  (interactive)
  (goto-char eshell-last-output-end)
  (insert-and-inherit text)
  (funcall (or func 'eshell-send-input)))

(defun eshell--execute-command (command save-excursion?)
  (let ((body #'(lambda nil
                  (eshell-insert-command command))))
    (if save-excursion?
        (save-excursion
          (funcall body))
      (funcall body))))

(defun eshell/run-with-bundler (&rest args)
  "Run the `ARGS' with bundler when in a bundled project."
  (let ((cmd (eshell-flatten-and-stringify args)))
    (if (and (bundler--installed?) (bundler--in-bundled-project?))
        (eshell--execute-command (format "bundle exec %s" cmd) nil)
      (eshell--execute-command cmd nil))))

(defun get-previous-indentation ()
  "Get the column of the previous indented line"
  (interactive)
  (save-excursion
    (progn
      (move-beginning-of-line nil)
      (skip-chars-backward "\n \t")
      (back-to-indentation))
    (current-column)))

(defun get-current-indentation ()
  "Return column at current indentation"
  (interactive)
  (save-excursion
    (progn
      (back-to-indentation)
      (current-column))))

(defun point-at-current-indentation ()
  "Return point at current indentation"
  (interactive)
  (save-excursion
    (progn
      (move-to-column (get-current-indentation))
      (point))))

(defun point-at-column-on-line (col)
  "Returns the point at `col` on the current line"
  (interactive)
  (save-excursion
    (progn
      (move-to-column col)
      (point))))

(defun ig-move-line-to-column (col)
  "Move the line to col; fill with all spaces if moveing forward"
  (interactive "p")
  (let ((point-at-cur-indent (point-at-current-indentation))
	(col-at-cur-indent (get-current-indentation)))
    (cond (
	   (= col 0)
	   ;; delete to beginning of line or do nothing
	   (if (= col-at-cur-indent 0)
	       nil
	     (delete-region point-at-cur-indent (point-at-column-on-line 0))))
	  (
	   (< col col-at-cur-indent)
	   ;; delete from our current point BACK to col
	   (delete-region (point-at-column-on-line col) point-at-cur-indent))
	  (
	   (> col col-at-cur-indent)
	   ;; delete all text from indent to beginning of line
	   (progn
	     (delete-region point-at-cur-indent (point-at-column-on-line 0))
	     (move-beginning-of-line nil)
	     ;; add spaces forward
	     (insert-string (make-string col ?\s)))))))

(defun ig-indent-sql ()
  "Indent by `tab-width` at most 1 time greater than the previously indented line otherwise go to the beginning of the line indent forward by `tab-width`"
  (let ((previous (get-previous-indentation))
        (current (get-current-indentation)))
    (cond ( ;; exactly at previous line's indentation
           (= previous current)
	   (ig-move-line-to-column (+ current tab-width)))

	  ( ;; current is greater than previous
	   (> current previous)
	   ;; exactly at one indentation forward from previous lines indent
	   (if (= tab-width (- current previous))
	       ;; move line to beginning
	       (ig-move-line-to-column 0)
	     ;; go back to previous indentation level
	     (ig-move-line-to-column previous)))

          (t
	   (ig-move-line-to-column (+ current tab-width))))))


(add-hook 'sql-mode-hook
          (function (lambda ()
                      (make-local-variable 'indent-line-function)
                      (setq indent-line-function 'ig-indent-sql))))


(global-set-key (kbd "C-RET") 'other-window)
(global-set-key (kbd "C-z") 'delete-other-windows)
(global-set-key (kbd "M-F") 'forward-to-word)
(global-set-key (kbd "M-f") 'forward-word)

;; (use-package server
;;   :no-require
;;   :hook (after-init . server-start))

(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir (projectile-project-root)))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))

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
