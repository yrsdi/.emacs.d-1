;;; Environment

(eval-and-compile
  (setq load-path
	(append (delete-dups load-path)
		(list (format "%s%s" user-emacs-directory "lisp")))))

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(set-frame-font "IBM Plex Mono Text 10" nil t)

(define-key isearch-mode-map (kbd "C-o") #'isearch-occur)

(define-key input-decode-map [?\C-m] [C-m])

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(use-package bind-key :ensure t)

(setq use-package-verbose t)

(eval-and-compile
  (mapc #'(lambda (entry)
            (define-prefix-command (cdr entry))
            (bind-key (car entry) (cdr entry)))
        '(("<C-m>" . my-ctrl-m-map) ;; mc
          ("C-c b" . my-ctrl-c-b-map) ;; for bm
          ("C-c m" . my-ctrl-c-o-map) ;; for org
          ("C-c o" . my-ctrl-c-m-map) ;; for occur
          ("C-c y" . my-ctrl-c-y-map) ;; aya
          ("M-i" . my-m-i-map)
          ("M-o" . my-m-o-map)
          )))


(use-package undo-fu
  :ensure t
  :bind (("C-z"   . undo-fu-only-undo)
         ("C-S-z" . undo-fu-only-redo)))

(use-package visual-fill-column
  :ensure t
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  (add-hook 'markdown-mode-hook 'visual-line-mode))

(use-package lisp-mode
  :diminish
  :config
  (defun visit-ielm ()
    "Switch to default `ielm' buffer.
	  Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))

  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package ielm
  :ensure t
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode) )

(use-package avy
  :ensure t
  :bind (
         ("M-T" . avy-goto-word-1)
	 ("<C-return>" . avy-goto-char-timer))
  :config
  (avy-setup-default)
  (setq avy-background t))

(use-package ido
  :ensure t
  :config
  (setq ido-use-faces nil))

(use-package ido-at-point
  :ensure t
  :config
  (ido-at-point-mode))

(use-package ido-vertical-mode
  :ensure t
  :config
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))

(use-package cider
  :ensure t)

(use-package clojure-mode
  :ensure t
  :hook
  (clojure-mode . eldoc-mode)
  (inf-clojure-mode . eldoc-mode)
  (clojure-mode . clj-refactor-mode)
  (clojure-mode . paredit-mode))

(use-package clj-refactor
  :ensure t
  :config
  (cljr-add-keybindings-with-prefix "<C-m> C-r"))

(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice (lambda () (switch-to-buffer "*dashboard*")))
  :config
  (setq dashboard-banner-logo-title "Do the work.")
  (setq dashboard-startup-banner nil)
  (dashboard-setup-startup-hook))

(use-package magit
  :ensure t
  :bind (:map magit-diff-mode-map
	      (("C-o" . magit-diff-visit-file-other-window)))
  :config
  (add-hook 'after-save-hook 'magit-after-save-refresh-status)

  (setq auto-revert-buffer-list-filter
	'magit-auto-revert-repository-buffers-p)

  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  ;; (setq vc-handled-backends '(Git))
  ;; (setq magit-push-always-verify nil)

  (setq vc-follow-symlinks t)
  (setq magit-refresh-status-buffer t)

  (magit-define-section-jumper magit-jump-to-recent-commits "Recent commits" recent "HEAD~10..HEAD")

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


  ;; (remove-hook 'server-switch-hook 'magit-commit-diff)

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


  (defun magit-key-mode--add-default-options (arguments)
    (if (eq (car arguments) 'pulling)
	(list 'pulling (list "--rebase"))
      arguments)

    (if (eq (car arguments) 'pushing)
	(list 'pushing (list "-u"))
      arguments)
    )

  (advice-add 'magit-key-mode :filter-args #'magit-key-mode--add-default-options)
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package magit-diff-flycheck
  :ensure t)

(use-package forge
  :ensure t
  :config
  (setq forge-topic-list-limit '(3 . -1)
        forge-pull-notifications nil))

(use-package ivy-rich
  :ensure t
  :config  (setq ivy-virtual-abbreviate 'full
                 ivy-rich-switch-buffer-align-virtual-buffer t)
  (setq ivy-rich-path-style 'abbrev)
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode))

(use-package copy-as-format
  :ensure t
  :init
  (setq copy-as-format-default "github"))

(use-package abbrev
  :diminish
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package compile
  :init
  (require 'grep)
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
  :ensure t
  :preface
  (defun my-dired-toggle-mode-hook ()
    (interactive)
    (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
    (setq-local word-wrap nil))
  :hook (dired-toggle-mode . my-dired-toggle-mode-hook))

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package auto-yasnippet
  :ensure t
  :after yasnippet
  :bind (("C-c y a" . aya-create)
         ("C-c y e" . aya-expand)
         ("C-c y o" . aya-open-line)))

(use-package git-timemachine
  :ensure t
  :bind (("s-g" . git-timemachine)))

(use-package ag
  :ensure t
  :bind
  ("C-c A" . ag))

(use-package smart-forward
  :ensure t
  :config
  :bind
  (("M-<up>" . smart-up)
   ("M-<down>" . smart-down)
   ("M-<left>" . smart-backward)
   ("M-<right>" . smart-forward)))

(use-package ibuffer-projectile
  :ensure t
  :config
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (ibuffer-projectile-set-filter-groups)
	      (unless (eq ibuffer-sorting-mode 'alphabetic)
		(ibuffer-do-sort-by-alphabetic)))))

(use-package re-builder
  :bind (:map reb-mode-map
	      ("M-%" . reb-query-replace))
  :config
  (setq reb-re-syntax 'string)
  (defun reb-query-replace (to-string)
    "Replace current RE from point with `query-replace-regexp'."
    (interactive
     (progn (barf-if-buffer-read-only)
	    (list (query-replace-read-to (reb-target-binding reb-regexp)
					 "Query replace"  t))))
    (with-current-buffer reb-target-buffer
      (query-replace-regexp (reb-target-binding reb-regexp) to-string))))



(use-package edit-indirect
  :ensure t
  :bind (("C-c '" . edit-indirect-region)))

(use-package hcl-mode :ensure t)

(use-package restclient
  :ensure t
  :mode
  ("\\.rest\\'" . restclient-mode)
  :config
  (defun tj-response-loaded-hook ()
    (flycheck-mode -1))
  (add-hook 'restclient-response-loaded-hook 'tj-response-loaded-hook)
  (defun tj-restclient-hook ()
    (setq-local indent-line-function 'js-indent-line))
  (add-hook 'restclient-mode-hook 'tj-restclient-hook))

(use-package osx-clipboard
  :ensure t
  :config
  (osx-clipboard-mode))

(use-package ediff
  :ensure t
  :config

  (defun ediff-copy-both-to-C ()
    (interactive)
    (ediff-copy-diff ediff-current-difference nil 'C nil
                     (concat
                      (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                      (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))

  (defun add-d-to-ediff-mode-map () (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
  (add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)
  :init
  (setq ediff-split-window-function 'split-window-vertically)
  (setq ediff-merge-split-window-function 'split-window-vertically)
  :bind (("C-c = b" . ediff-buffers)
         ("C-c = f" . ediff-files)
         ("C-c = r" . ediff-revision)
         ("C-c = l" . ediff-regions-linewise)
         ("C-c = w" . ediff-regions-wordwise)
         ("C-c = c" . compare-windows)))

(use-package git-link
  :ensure t
  :commands (git-link git-link-commit git-link-homepage)
  :bind ("C-c G" . git-link))

(use-package git-timemachine
  :ensure t
  :commands git-timemachine)

(use-package gitattributes-mode
  :ensure t
  :defer 5)

(use-package gitconfig-mode
  :ensure t
  :defer 5)

(use-package github-pullrequest
  :ensure t
  :commands (github-pullrequest-new
	     github-pullrequest-checkout))

(use-package gitignore-mode
  :ensure t
  :defer 5)

(use-package gitpatch
  :ensure t
  :commands gitpatch-mail)

(use-package google-this
  :ensure t
  :bind ("C-c /" . google-this-search))

(use-package goto-last-change
  :ensure t
  :bind ("C-x C-/" . goto-last-change))

(use-package ialign
  :ensure t
  :bind
  ("C-c [" . ialign-interactive-align))

(use-package operate-on-number
  :ensure t
  :bind ("C-c #" . operate-on-number-at-point))

(use-package shift-number
  :ensure t
  :bind (("C-c -" . shift-number-down)
         ("C-c +" . shift-number-up)))

(use-package diminish
  :ensure t
  :demand t)

(use-package ivy
  :ensure t
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq projectile-completion-system 'ivy)
  (bind-keys :map minibuffer-local-map
             ((kbd "C-r") #'counsel-minibuffer-history))
  :bind
  (("C-c C-r" . ivy-resume)
   ("M-R" . ivy-resume)
   ("C-x b" . ivy-switch-buffer)
   ("C-x B" . ivy-switch-buffer-other-window)))

(use-package swiper
  :ensure t
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
  (bind-keys :map isearch-mode-map ("C-i" . swiper-from-isearch)))

(use-package company-tern
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-tern)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))

(use-package company-quickhelp
  :ensure t
  :defer t)

(eval-after-load 'company
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

(use-package eldoc
  :ensure t
  :diminish)

(use-package go-mode
  :ensure t

  :bind
  (:map go-mode-map
        ("M-j" . comment-indent-new-line)
        ("<tab>" . company-indent-or-complete-common)
	("M-b" . subword-backward)
	("M-f" . subword-forward)
	("M-d" . subword-kill)
	("C-c C-t" . go-test-current-file)
	("C-c C-c" . lsp-describe-thing-at-point)
	("C-c M-t" . go-test-current-test)
	;; ("C-c <C-m>" . tj-go-kill-doc)
        ("C-c C-e" . tj-go-err))
  :config

  (setq go-test-args "-race -v")

  (defun tj-go-err ()
    (interactive)
    (if (region-active-p)
        (let ((body (buffer-substring-no-properties (region-beginning) (region-end))))
          (goto-char (region-beginning))
          (delete-char (string-width body))
          (yas-expand-snippet
           (concat "if err := ${1:" body "}; err != nil {\n"
                   "$0\n"
                   "}")))
      (yas-expand-snippet
           (concat "if err != nil {\n$0\n}"))))

  (defun my-try-go-mod (dir)
    "Find go project root for DIR."
    (if (and dir
             (not (f-descendant-of-p dir (or (getenv "GOPATH")
                                             (concat (getenv "HOME") "/go")))))
        (let ((result (locate-dominating-file dir "go.mod")))
          (if result
              (cons 'transient (expand-file-name result))
            (cons 'transient dir)))
      (when dir
        (cons 'transient dir))))


  (defun my-go-project-setup ()
    "Set project root for go project."
    (setq-local project-find-functions (list #'my-try-go-mod #'project-try-vc)))

  (add-hook 'go-mode-hook #'my-go-project-setup)

  (use-package go-eldoc
    :ensure t)

  (use-package godoctor :ensure t)

  ;; override this func for testify
  (defun go-test-current-test ()
    "Launch go test on the current test."
    (interactive)
    (cl-destructuring-bind (test-suite test-name) (go-test--get-current-test-info)
      (let ((test-flag (if (> (length test-suite) 0) "-testify.m " "-run "))
            (additional-arguments (if go-test-additional-arguments-function
                                      (funcall go-test-additional-arguments-function
                                               test-suite test-name) "")))
        (when test-name
          (if (go-test--is-gb-project)
              (go-test--gb-start (s-concat "-test.v=true -test.run=" test-name "\\$ ."))
            (go-test--go-test (s-concat test-flag test-name additional-arguments "\\$ .")))))))

  (load "~/dev/src/github.com/stapelberg/expanderr/expanderr.el")
    (setq gofmt-command "goimports")

  (setq tab-width 8)

  (setq-local compilation-read-command nil)

  (use-package go-add-tags :ensure t)

  (use-package go-errcheck
    :ensure t
    :config
    (defun tj-go-errcheck ()
      (interactive)
      (let ((default-directory (projectile-project-root)))
	(go-errcheck nil nil nil))))

  (defun tj-turn-on-gofmt-before-save ()
    (interactive)
    (add-hook 'before-save-hook 'gofmt-before-save))

  (defun tj-turn-off-gofmt-before-save ()
    (interactive)
    (remove-hook 'before-save-hook 'gofmt-before-save))

  (tj-turn-on-gofmt-before-save)

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

  (defun tj-go-hook ()
    (setq imenu-generic-expression
          '(("type" "^[ \t]*type *\\([^ \t\n\r\f]*[ \t]*\\(struct\\|interface\\)\\)" 1)
            ("func" "^func *\\(.*\\)" 1)))

    (when (boundp 'lsp-clients-go-library-directories)
      (add-to-list 'lsp-clients-go-library-directories "/home/tj/dev/pkg/mod")
      (add-to-list 'lsp-clients-go-library-directories "/home/tj/go"))

    (which-function-mode)
    (highlight-symbol-mode)
    (subword-mode)
    (flycheck-mode)
    (go-eldoc-setup)
    (electric-indent-mode)
    (electric-pair-mode 1)
    (selected-minor-mode 1)
    ;; (font-lock-mode -1)
    (setq company-backends '(company-go))
    (go-guru-hl-identifier-mode)
    (if (not (string-match "go" compile-command))
	(set (make-local-variable 'compile-command)
	     "go build -v && go test -v && go vet")))
  (add-hook 'go-mode-hook 'tj-go-hook)

  (use-package go-guru
    :ensure t)

  (use-package gotest :ensure t)

  :hook
  (go-mode . tj-go-hook))

(use-package selected :ensure t)

(use-package winner
  :diminish
  :config
  (winner-mode +1)
  :bind
  (("M-[" . winner-undo)
   ("M-]" . winner-redo)))

(use-package eacl
  :ensure t
  :config
  (setq eacl-grep-program "grep --exclude-dir=.git --exclude-dir=vendor")
  :bind
  (("C-x C-l" . eacl-complete-line)))

(use-package go-gen-test
  :ensure t
  :after go-mode)

(use-package embrace
  :ensure t
  :config
  (setq	 embrace-show-help-p nil)
  :bind
  ("C-c M-e" . embrace-commander))

(use-package ido-completing-read+
  :ensure t
  :config
  (ido-ubiquitous-mode))


(use-package bm
  :ensure t
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
  :ensure t
  :config



  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-mode-line nil)
  (setq projectile-sort-order 'modification-time)
  ;; (setq projectile-sort-order 'default)

  ;; Fix the issue where the older project name is prepended to 'Find File:'
  ;; prompt after `projectile-switch-project'
  ;; https://github.com/bbatsov/projectile/issues/1067#issuecomment-270656085
  ;; https://github.com/bbatsov/projectile/issues/1067#issuecomment-270686996
  ;; (defun projectile-project-name-old ()
  ;;   "Return project name."
  ;;   (if projectile-project-name
  ;;       projectile-project-name
  ;;     (let ((project-root
  ;;            (condition-case nil
  ;;       	 (projectile-project-root)
  ;;              (error nil))))
  ;;       (if project-root
  ;;           (funcall projectile-project-name-function project-root)
  ;;         "-"))))
  ;; (advice-add 'projectile-project-name :override #'projectile-project-name-old)

  ;; (def-projectile-commander-method ?f "Find file." (call-interactively 'counsel-fzf))
  (def-projectile-commander-method ?f "Find file." (call-interactively 'projectile-find-file-dwim))
  (setq projectile-switch-project-action #'projectile-commander)
  (add-to-list 'projectile-globally-ignored-directories "Godeps/_workspace")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  ;; (add-to-list 'projectile-globally-ignored-directories "_build")
  (add-to-list 'projectile-globally-ignored-directories "deps")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")

  (projectile-global-mode 1)

  :bind
  (("C-c t" . projectile-toggle-between-implementation-and-test)
   ("C-c p p" . projectile-switch-project)
   ("C-c p i" . projectile-invalidate-cache)
   ("C-c C-p" . projectile-test-project)
   ("C-c P" . 'projectile-switch-project)))

(use-package web-beautify :ensure t)

(use-package deadgrep
  :ensure t
  :config
  (setq deadgrep-executable "rg --hidden")
  :bind
  ("C-c a" . deadgrep))

(use-package pt
  :ensure t)

(use-package expand-region
  :ensure t
  :config
  (er/add-html-mode-expansions)
  :bind ("C-=" . er/expand-region))

(use-package elisp-slime-nav
  :ensure t
  :diminish
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode)))

(use-package paredit
  :ensure t
  :init
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  :diminish
  :bind (:map paredit-mode-map
              ("M-;" . nil)
              ("M-r" . nil)
              ("M-I" . paredit-splice-sexp)))

(use-package smex :ensure t)

(use-package paren
  :config
  (show-paren-mode +1))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package tj)

;; saveplace remembers your location in a file when saving files
(require 'saveplace)


(use-package saveplace
  :config
  (defconst savefile-dir (expand-file-name "savefile" user-emacs-directory))

  ;; create the savefile dir if it doesn't exist
  (unless (file-exists-p savefile-dir)
    (make-directory savefile-dir))

  (setq save-place-file (expand-file-name "saveplace" savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

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

;; (use-package recentf
;;   :config
;;   (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
;; 	recentf-max-saved-items 500
;; 	recentf-max-menu-items 15
;; 	;; disable recentf-cleanup on Emacs start, because it can cause
;; 	;; problems with remote files
;; 	recentf-auto-cleanup 'never)
;;   (recentf-mode +1))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package highlight-symbol
  :ensure t
  :diminish
  :config
  (highlight-symbol-mode)
  :bind
  (("M-p" . highlight-symbol-prev)
   ("M-n" . highlight-symbol-next)))

(use-package diffview
  :commands (diffview-current diffview-region diffview-message))

(use-package f :ensure t)

(use-package dired
  :bind
  (("C-x d" . dired-jump)
   ("C-x D" . counsel-dired-jump))
  :bind (:map dired-mode-map
	      ("z"     . delete-window)
	      ("e"     . ora-ediff-files)
	      ("l"     . dired-up-directory)
	      ("Y"     . ora-dired-rsync)
	      ("<tab>" . tj-dired-switch-window)
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

  (defun tj-dired-switch-window ()
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
  (defun dired-back-to-top ()
    (interactive)
    (goto-char (point-min))
    (dired-next-line 4))

  (define-key dired-mode-map
    (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

  (defun dired-jump-to-bottom ()
    (interactive)
    (goto-char (point-max))
    (dired-next-line -1))

  (define-key dired-mode-map
    (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)


  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x)

  (ignore-errors
    (unbind-key "M-s f" dired-mode-map))

  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Make sure to remove \"Omit\" from the modeline."
    (diminish 'dired-omit-mode) dired-mode-map)

  (defadvice dired-next-line (around dired-next-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and	 (not  (eobp)) (not ad-return-value))
      (forward-line)
      (setq ad-return-value(dired-move-to-filename)))
    (when (eobp)
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename))))

  (defadvice dired-previous-line (around dired-previous-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and	 (not  (bobp)) (not ad-return-value))
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
  :ensure t
  :bind (:map dired-mode-map
	      ("/" . dired-narrow)))

(use-package package-lint
  :ensure t)

(use-package dired-ranger
  :bind (:map dired-mode-map
	      ("W" . dired-ranger-copy)
	      ("X" . dired-ranger-move)
	      ("Y" . dired-ranger-paste)))

(use-package sh-mode
  :init
  (setq sh-basic-offset 2)
  (setq sh-basic-indentation 2)
  :mode ("\\.bats$" . sh-mode))

(use-package anzu
  :ensure t
  :diminish
  :bind (("M-%" . anzu-query-replace-regexp)
	 ("C-M-%" . anzu-query-replace))
  :hook
  (prog-mode . anzu-mode))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill)
  (global-set-key [remap mark-sexp] 'easy-mark))

(use-package easy-kill-extras
  :ensure t
  :config

  (global-set-key (kbd "M-@") 'easy-mark-word)
  (global-set-key (kbd "C-M-@") 'easy-mark-sexp)

  (global-set-key [remap zap-to-char] 'easy-mark-to-char)

  ;; Integrate `expand-region' functionality with easy-kill
  (define-key easy-kill-base-map (kbd "o") 'easy-kill-er-expand)
  (define-key easy-kill-base-map (kbd "i") 'easy-kill-er-unexpand)


  ;; Add the following tuples to `easy-kill-alist', preferrably by
  ;; using `customize-variable'.
  (add-to-list 'easy-kill-alist '(?^ backward-line-edge ""))
  (add-to-list 'easy-kill-alist '(?$ forward-line-edge ""))
  (add-to-list 'easy-kill-alist '(?b buffer ""))
  (add-to-list 'easy-kill-alist '(?< buffer-before-point ""))
  (add-to-list 'easy-kill-alist '(?> buffer-after-point ""))
  (add-to-list 'easy-kill-alist '(?f string-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?F string-up-to-char-forward ""))
  (add-to-list 'easy-kill-alist '(?t string-to-char-backward ""))
  (add-to-list 'easy-kill-alist '(?T string-up-to-char-backward "")))

(use-package exec-path-from-shell
  :ensure t
  :init
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "GOROOT" "GOPATH"))
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)))

(use-package move-text
  :ensure t
  :bind
  (("M-P" . move-text-up)
   ("M-N" . move-text-down)))

(use-package whitespace
  :config
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 84) ;; limit line length
  (setq whitespace-style '(face empty lines trailing)))

(use-package markdown-mode
  :config

  (unless (executable-find "pandoc")
    (message "install pandoc"))
  (setq markdown-command "pandoc --section-divs --from=markdown_github --highlight-style=haddock --self-contained --smart --to=html5 --css=$HOME/.config/css/style.css")

  :mode
  ("\\.markdown$" . markdown-mode)
  ("\\.md$" . markdown-mode)
  :hook
  ((markdown-mode . font-lock-mode)
   (markdown-mode . writegood-mode))
  :ensure t)

(use-package writegood-mode
  :ensure t)

(use-package yaml-mode
  :mode
  ("\\.yaml" . yaml-mode)
  :ensure t)

(use-package org
  :hook (org-mode . font-lock-mode)
  :config

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
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


  (setq org-default-notes-file (expand-file-name "~/Dropbox/org/capture.org"))

  (setq org-startup-folded nil)

  (defun tj-org-capture ()
    (interactive)
    (find-file org-default-notes-file))

  (defun tj-org-archive-done-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/DONE" 'tree))

  (require 'org-table)

  (defun tj-org-replace-link-by-link-description ()
    "Replace an org link by its description or if empty its address"
    (interactive)
    (if (org-in-regexp org-bracket-link-regexp 1)
        (save-excursion
          (let ((remove (list (match-beginning 0) (match-end 0)))
                (description (if (match-end 3)
                                 (org-match-string-no-properties 3)
                               (org-match-string-no-properties 1))))
            (apply 'delete-region remove)
            (insert description)))))

  (setq org-src-tab-acts-natively t)
  (setq org-default-notes-file (expand-file-name "~/Dropbox/org/capture.org"))
  (setq org-directory (expand-file-name "~/Dropbox/org"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-use-speed-commands t)


  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
		(sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))

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

  :bind (("C-c m c" . org-capture)
         ("C-c m t" . org-todo-list)
         ("C-c m m" . orgs-tagsview)))

(use-package org-journal
  :ensure t
  :defer t
  :config
  (setq org-journal-dir "~/Dropbox/journal"))



(use-package org-web-tools
  :ensure t
  :defer t)

(use-package alert
  :ensure t
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

(use-package ace-link
  :ensure t
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
  :ensure t
  :bind (("<C-m> h"   . ace-mc-add-multiple-cursors)
	 ("<C-m> M-h" . ace-mc-add-single-cursor)))

(use-package multiple-cursors
  :ensure t
  :defer 5
  :after selected
  :preface

  (defun reactivate-mark ()
    (interactive)
    (activate-mark))

  :config
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)

  :bind (
         ("<C-m> <C-m>" . mc/edit-lines)
         ("<C-m> C-SPC" . mc/mark-pop)
         ("<C-m> C-e" . mc/edit-ends-of-lines)
         ("<C-m> C-a" . mc/edit-beginnings-of-lines)
         ("<C-m> a" . mc/mark-all-dwim)
         ("<C-m> C-x" . reactivate-mark)
         ("<C-m> C-SPC" . mc/mark-pop)
         ("<C-m> <" . mc/mark-previous-like-this)
         ("<C-m> >" . mc/mark-next-like-this)
         ;; Extra multiple cursors stuff
         ("<C-m> %" . mc/insert-numbers)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click)))

(use-package mc-extras
  :ensure t
  :after multiple-cursors
  :bind (("<C-m> M-C-f" . mc/mark-next-sexps)
         ("<C-m> M-C-b" . mc/mark-previous-sexps)
         ("<C-m> C-d"   . mc/remove-current-cursor)
         ("<C-m> C-k"   . mc/remove-cursors-at-eol)
         ("<C-m> M-d"   . mc/remove-duplicated-cursors)
         ("<C-m> |"     . mc/move-to-column)
         ("<C-m> ~"     . mc/compare-chars)))

(use-package phi-search
  :ensure t
  :bind (:map mc/keymap
              ("C-r" . phi-search-backward)
              ("C-s" . phi-search)))



(use-package ace-jump-mode
  :ensure t
  :defer t)

(use-package browse-url
  :bind
  (("C-c x" . browse-url-at-point)))

(use-package pdf-tools
  :ensure t
  :config
  (pdf-tools-install)
  (pdf-loader-install))

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

(add-to-list 'completion-styles 'initials t)
(add-to-list 'completion-styles 'subwords t)
(add-to-list 'completion-styles 'substring t)

(use-package company
  :ensure t
  :diminish
  :config

  ;; Ignore go test -c output files
  (add-to-list 'completion-ignored-extensions ".test")
  (setq company-ddabbrev-code-everywhere t)
  (setq company-dabbrev-code-modes t)
  (setq company-dabbrev-code-other-buffers 'all)
  (setq company-dabbrev-ignore-buffers "\\`\\'")
  (setq company-idle-delay 0)
  (setq company-echo-delay 0)
  (setq company-tooltip-align-annotations t)
  (setq company-tern-property-marker "")
  (setq company-minimum-prefix-length 3)
  (setq company-abort-manual-when-too-short 3)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-other-buffers 'all)

  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  ;; (defadvice company-pseudo-tooltip-unless-just-one-frontend
  ;;     (around only-show-tooltip-when-invoked activate)
  ;;   (when (company-explicit-action-p)
  ;;     ad-do-it))
  )

(use-package company-elisp
  :after company
  :config
  (push 'company-elisp company-backends))

(use-package cask-mode
  :ensure t)

(use-package company
  :ensure t
  :hook
  (prog-mode . company-mode))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
	 ("M-Z" . zop-to-char)))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
	 ("s-i" . imenu-anywhere)))

(use-package flyspell
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
	ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode))

(use-package flycheck
  :ensure t
  :config
  (setq flycheck-check-syntax-automatically '(save mode-enable))
  (setq flycheck-idle-change-delay 4)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package super-save
  :ensure t
  :diminish
  :config
  (super-save-mode +1))

(use-package crux
  :ensure t
  :bind (("C-c d" . crux-duplicate-current-line-or-region)
	 ("C-c n" . crux-cleanup-buffer-or-region)
	 ("C-c f" . crux-recentf-find-file)
	 ("C-M-z" . crux-indent-defun)
	 ("C-c u" . crux-view-url)
	 ("C-c w" . crux-swap-windows)
	 ("C-c D" . crux-delete-file-and-buffer)
	 ("C-c r" . crux-rename-buffer-and-file)
	 ("C-c k" . crux-kill-other-buffers)
	 ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
	 ("C-c I" . crux-find-user-init-file)
	 ("C-c S" . crux-find-shell-init-file)
	 ("s-r" . crux-recentf-find-file)
	 ("s-j" . crux-top-join-line)
	 ("C-^" . crux-top-join-line)
	 ("C-S-k" . crux-kill-whole-line)
	 ("C-k" . kill-line)
	 ("C-<backspace>" . crux-kill-line-backwards)
	 ("s-o" . crux-smart-open-line-above)
	 ([remap move-beginning-of-line] . crux-move-beginning-of-line)
	 ([(shift return)] . crux-smart-open-line)
	 ([(control shift return)] . crux-smart-open-line-above)
	 ([remap kill-whole-line] . crux-kill-whole-line)
	 ("C-c s" . crux-ispell-word-then-abbrev)))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode +1))

;; backups

(setq
 backup-by-copying t
 delete-old-versions t
 kept-new-versions 10
 kept-old-versions 2
 vc-make-backup-files t
 version-control t)

(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))

(add-hook 'before-save-hook  'force-backup-of-buffer)

(use-package undo-propose
  :ensure t)

(use-package goto-chg
  :ensure t
  :bind
  (("C-c ." . goto-last-change)
   ("C-c ," . goto-last-change-reverse)))

(use-package color-moccur
  :ensure t
  :commands (isearch-moccur isearch-all isearch-moccur-all)
  :bind (("C-c o o" . occur)
         ("C-c o O" . moccur)
         :map isearch-mode-map
	 ("M-o" . isearch-moccur)
	 ("M-O" . isearch-moccur-all)))

(use-package isearch
  :config

  (setq isearch-lazy-highlight 'all-windows)
  (setq isearch-allow-scroll t)
  (setq lazy-highlight-cleanup t)

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

  :bind
  (("C-s" . isearch-forward-regexp)
   ("C-M-s" . isearch-forward)
   ("C-r" . isearch-backward-regexp)
   ("C-M-r" . isearch-backward))
  (:map isearch-mode-map
        ("M-z" . zap-to-isearch)))

(use-package moccur-edit
  :after color-moccur)

(use-package ace-window
  :ensure t
  :diminish
  :bind* ("<s-return>" . ace-window))

(use-package counsel
  :diminish
  :ensure t
  :config

  (defun go-find-file ()
    "Find file under $GOROOT."
    (interactive)
    (find-file (format "%s/src/" (getenv "GOROOT"))))

  (defun tj-counsel-ag ()
    (interactive)
    (counsel-ag nil (projectile-project-root)))

  (setq counsel-find-file-at-point t)
  :bind
  (("C-*"     . counsel-org-agenda-headlines)
   ("C-x C-f" . counsel-find-file)
   ("C-c p g" . go-find-file)
   ("C-h f"   . counsel-describe-function)
   ("C-h v"   . counsel-describe-variable)
   ("C-x r b" . counsel-bookmark)
   ("M-x"     . counsel-M-x)
   ("C-x <C-m>"     . counsel-M-x)
   ("C-c i" . counsel-imenu)
   ("M-y" . counsel-yank-pop))
  :commands counsel-minibuffer-history
  :init
  :config
  (defun ag-go (arg)
    (interactive "P")
    (let*
        ((pkg (or
               (and arg (read-string "PKG: "))
               (thing-at-point 'filename)))
         (dir (f-join (getenv "GOPATH") "src" pkg))
         (search (read-string "Search string: ")))
      (ag search dir))))

(use-package smartparens
  :ensure t
  :bind (:map smartparens-mode-map
              ("M-(" . sp-wrap-round)
              ("C-)" . sp-forward-slurp-sexp)
              ("C-}" . sp-forward-barf-sexp)
              ("C-{" . sp-backward-barf-sexp)
              ("C-(" . sp-backward-slurp-sexp)
              ("C-'" . sp-rewrap-sexp)
              ("M-S" . sp-split-sexp)
              ("M-J" . sp-join-sexp)
              ("M-W" . sp-copy-sexp))
  :config
  (require 'smartparens-config)
  (setq sp-ignore-modes-list '(minibuffer-inactive-mode eval-expression-minibuffer-setup))
  (sp-local-pair 'js2-mode "{ " " }" :trigger-wrap "{")
  :hook
  (prog-mode . smartparens-mode))

(use-package counsel-projectile
  :ensure t
  :config
  (setq counsel-projectile-remove-current-buffer t)
  (setq counsel-projectile-remove-current-project t)
  (setq counsel--find-file-matcher 'counsel--find-file-matcher)

  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-projectile-find-file . ivy--sort-files-by-date))

  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-find-file . ivy--sort-files-by-date))

  :bind
  (("C-\\" . counsel-projectile-find-file)
   ("C-c p f" . counsel-projectile-find-file)))

(use-package github-browse-file
  :ensure t
  :bind ("C-c g" . github-browse-file))

(use-package minibuffer
  :config
  (defun my-minibuffer-setup-hook ()
    (smartparens-mode -1)
    (electric-pair-mode -1)
    (subword-mode)
    (setq gc-cons-threshold most-positive-fixnum))

  (defun my-minibuffer-exit-hook ()
    (electric-pair-mode 1)
    (setq gc-cons-threshold 800000))

  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook))


(use-package eval-expr
  :ensure t
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

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :diminish
  :config
  (volatile-highlights-mode +1))

(when (eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super) ; Left Windows key

  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key

  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper) ; Menu/App key

  (add-to-list 'exec-path "C:/Program Files/Git/bin")
  (add-to-list 'exec-path "C:/Program Files/Git/mingw64/bin")
  (setenv "PATH" (concat "C:/Program Files/Git/bin;" "C:/Program Files/Git/mingw64/bin;" (getenv "PATH")))
  ;; needed for arc-mode
  (add-to-list 'exec-path "C:/Program Files/7-Zip"))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

;; (defadvice split-window (after move-point-to-new-window activate)
;;   "Moves the point to the newly created window after splitting."
;;   (other-window 1))

;; Functions and bindings.


(setq comment-multi-line t)
(setq-default indent-tabs-mode nil)

(use-package eshell
  :commands (eshell eshell-command)
  :preface
  (defvar eshell-isearch-map
    (let ((map (copy-keymap isearch-mode-map)))
      (define-key map [(control ?m)] 'eshell-isearch-return)
      (define-key map [return]	     'eshell-isearch-return)
      (define-key map [(control ?r)] 'eshell-isearch-repeat-backward)
      (define-key map [(control ?s)] 'eshell-isearch-repeat-forward)
      (define-key map [(control ?g)] 'eshell-isearch-abort)
      (define-key map [backspace]    'eshell-isearch-delete-char)
      (define-key map [delete]	     'eshell-isearch-delete-char)
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

  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)

  (defun tj-eshell-mode-hook ()
    (define-key eshell-mode-map (kbd "M-r") 'counsel-esh-history)
    (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env)))

  (add-hook 'eshell-mode-hook 'tj-eshell-mode-hook)
  :bind
  (("C-x m" . eshell)))


(use-package eshell-bookmark
  :ensure t
  :hook (eshell-mode . eshell-bookmark-setup))

(use-package eshell-up
  :ensure t
  :commands eshell-up)

(use-package eshell-z
  :ensure t
  :after eshell)

(use-package fancy-narrow
  :ensure t
  :commands (fancy-narrow-to-region fancy-widen))

(use-package wgrep :ensure t)

(use-package string-edit :ensure t)

(use-package json-snatcher :ensure t)

(use-package wgrep-ag
  :ensure t
  :config (autoload 'wgrep-ag-setup "wgrep-ag")
  :hook (ag-mode-hook . wgrep-ag-setup))

(use-package visual-regexp
  :ensure t
  :bind
  ("M-&" . vr/query-replace)
  ("M-/" . vr/replace))

(use-package avy-zap
  :ensure t
  :bind
  (("C-c e" . zap-up-to-char)
   ("C-c E" . avy-zap-up-to-char-dwim)
   ("M-Z" . avy-zap-up-to-char-dwim)))

(use-package backup-walker
  :ensure t
  :commands backup-walker-start)

(use-package change-inner
  :ensure t
  :bind (("M-i" . change-inner)
	 ("M-o" . change-outer)
	 ("C-c M-i" . copy-inner)
	 ("C-c M-o" . copy-outer)))

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'"
  :commands (protobuf-mode)
  :hook (protobuf-mode
	 . (lambda ()
	     (subword-mode)
	     (electric-pair-mode)
             (tj-protobuf-imenu-configure)
	     (c-add-style "tj-protobuf-style" tj-protobuf-style t)))
  :config

  (setq tj-protobuf-imenu-generic-expression
        '(("Message" "^message *\\([a-zA-Z0-9_]+\\)" 1)
          ("Service" "^service *\\([a-zA-Z0-9_]+\\)" 1)))

  (defun tj-protobuf-imenu-configure ()
    (interactive)
    (setq imenu-generic-expression tj-protobuf-imenu-generic-expression))

  (progn
    (defconst tj-protobuf-style
      '((c-basic-offset . 2)
	(indent-tabs-mode . nil)))))

(use-package js2-mode
  :ensure t
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
  :config
  (defun js2-match-async-arrow-function ()
    (when (and (js2-contextual-kwd-p (js2-current-token) "async")
	       (/= (js2-peek-token) js2-FUNCTION)
	       (/= (js2-peek-token) js2-DOT))
      (js2-record-face 'font-lock-keyword-face)
      (js2-get-token)
      t))
  (defun tj-js2-mode-hook ()
    (electric-indent-mode 1)
    (tern-mode)
    (flycheck-mode)
    (subword-mode))
  (add-hook 'js2-mode-hook 'tj-js2-mode-hook))

(use-package bm
  :bind (("C-c b b" . bm-toggle)
         ("C-c b n" . bm-next)
         ("C-c b l" . bm-show-all)
         ("C-c b p" . bm-previous))
  :commands (bm-repository-load
             bm-buffer-save
             bm-buffer-save-all
             bm-buffer-restore)
  :ensure t
  :init
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save))))

(use-package magit-todos
  :ensure t
  :commands (magit-todos-mode)
  :config
  (setq magit-todos-exclude-globs '("dist" "node_modules")))


(use-package terraform-mode
  :ensure t
  :config
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode))

(use-package multifiles
  :ensure t
  :bind
  ("C-!" . mf/mirror-region-in-multifile))

(use-package toggle-quotes
  :ensure t
  :bind
  ( "C-\"" . toggle-quotes))

(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

(use-package highlight-indentation
  :ensure t
  :config
  (set-face-background 'highlight-indentation-face "#e3e3d3")
  (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode)
  (add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode))

(use-package indent-tools
  :ensure t
  :config
  (add-hook 'yaml-mode-hook 'indent-tools-minor-mode))

(use-package plain-theme
  :ensure t
  :config
  (load-theme 'plain t))

(use-package gist
  :ensure t)

(require 'resmacro)
(global-set-key (kbd "C-x (") 'resmacro-start-macro)

(require 'wordswitch)
(require 'titlecase)

(use-package unfill
  :ensure t
  :bind
  (("M-Q" . unfill-paragraph)))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)

(use-package lsp-mode
  :ensure t
  :bind
  (("C-c C-r" . lsp-rename))
  :config


  (add-to-list 'lsp-disabled-clients '(web-mode . angular-ls))
  (add-to-list 'lsp-disabled-clients '(markdown-mode . angular-ls))
  (add-to-list 'lsp-disabled-clients '(mhtml-mode . angular-ls))

  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "html-languageserver")
                     :activation-fn (lambda (&rest _args)
                                   (string-match-p ".*\.html$" (buffer-file-name)))
                  :priority 1
                  :add-on? t
                  :server-id 'html-ls))

  ;; (setq lsp-eldoc-render-all nil)

  (add-to-list 'lsp-language-id-configuration '(clojure-mode . "clojure-mode"))

  :hook
  (prog-mode . lsp-deferred)
  :commands (lsp lsp-deferred lsp-find-definition)
  :init
  (setq lsp-auto-guess-root t))

(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-doc-enable nil
        lsp-ui-doc-delay 0.1
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t))

(use-package company-lsp
  :ensure t
  :commands company-lsp
  :config
  (setq company-lsp-cache-candidates 'auto)
  (add-to-list 'company-backends 'company-lsp))

;; (use-package eglot
;;   :ensure t
;;   :bind
;;   (("C-c C-c" . eglot-help-at-point)
;;    ("C-c C-r" . eglot-rename))
;;   :hook (go-mode . eglot-ensure))

(use-package vdiff
  :ensure t)

(use-package vdiff-magit
  :ensure t)

(use-package dot-mode
  :ensure t
  :config
  (setq dot-mode-global-mode t)
  (dot-mode))

(use-package iedit
  :ensure t)

(use-package frog-jump-buffer
  :ensure t)

(use-package github-review
  :ensure t)

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview))

(use-package vterm
  :ensure t
  :custom (vterm-install t)
  :config
  (defun vterm--rename-buffer-as-title (title)
    (when (ignore-errors (file-directory-p title))
      (cd-absolute title))
    (rename-buffer (format "term %s" title)))
  (add-hook 'vterm-set-title-functions 'vterm--rename-buffer-as-title)
  :hook
  (vterm-mode . disable-font-lock-mode))

(use-package flycheck-vale
  :ensure t
  :config
  (flycheck-vale-setup))

(use-package proced-narrow
  :ensure t
  :after proced
  :bind (:map proced-mode-map
              ("/" . proced-narrow)))

(use-package mw-thesaurus
  :ensure t
  :bind (:map markdown-mode-map
              ("C-c C-c d" . mw-thesaurus-lookup-at-point)))

(require 'go-mod)
(require 'prag-prog)
(require 'maven-search)

(use-package server
  :no-require
  :hook (after-init . server-start))
