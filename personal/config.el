(require 'use-package)

;; (setq mac-allow-anti-aliasing t)
(setq prelude-guru nil)

(setq flycheck-check-syntax-automatically '(save
                                            mode-enabled))
(setq-default tab-width 2)
(setq multi-term-program "/usr/local/bin/zsh")
(setq explicit-shell-file-name "/usr/local/bin/zsh")
(setq ring-bell-function 'ignore)
(setq prelude-clean-whitespace-on-save t)
(setq scroll-margin 10)

(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-font-lock-mode -1)

(add-hook 'prelude-prog-mode-hook
          (lambda ()
            (smartparens-mode -1)
            (electric-pair-mode)
            (electric-indent-mode)
            )
          t)

(setq prelude-whitespace nil)
(setq prelude-clean-whitespace-on-save nil)

(setq company-tooltip-align-annotations t)
(setq company-tern-property-marker "")

(menu-bar-mode -1)

(set-frame-font (font-spec :family "Operator Mono" :size 14 :weight 'normal))
(add-to-list 'default-frame-alist '(font . "Operator Mono-14"))

(global-hl-line-mode -1)

(setq vc-handled-backends nil)

(add-hook 'isearch-mode-end-hook
          #'endless/goto-match-beginning)

(defun endless/goto-match-beginning ()
  "Go to the start of current isearch match.
Use in `isearch-mode-end-hook'."
  (when (and isearch-forward
             (number-or-marker-p isearch-other-end)
             (not mark-active)
             (not isearch-mode-end-hook-quit))
    (goto-char isearch-other-end)))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(defun tj-marked ()
  (interactive)
  (shell-command (format "open -a \"Marked 2\" %s" (buffer-file-name))))

(use-package markdown-mode
  :ensure t

  :init
  (add-to-list 'company-dabbrev-code-modes 'markdown-mode)

  :mode
  ("\\.markdown$" . markdown-mode)
  ("\\.md$" . markdown-mode)
  
  :config  
  (defun my-markdown-hook ()
    (flycheck-mode))
  (add-hook 'markdown-mode-hook 'my-markdown-hook))

(use-package org
  :ensure t
  :init
  (setq org-agenda-files (split-string (shell-command-to-string "find ~/Dropbox/org/*")))
  (setq org-src-lang-modes '(
                             ("screen" . sh)
                             ("ocaml" . tuareg)
                             ("elisp" . emacs-lisp)
                             ("lisp" . lisp)
                             ("ditaa" . artist)
                             ("asymptote" . asy)
                             ("cl" . lisp)
                             ("dot" . graphviz-dot)))
  :config
  (require 'org-table))

(add-to-list 'completion-styles 'initials t)
(add-to-list 'completion-styles 'subwords t)
(add-to-list 'completion-styles 'substring t)

(use-package company
  :ensure t
  :init
  (setq company-dabbrev-code-modes t
        company-dabbrev-code-everywhere t)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 3)
  (setq company-abort-manual-when-too-short 3)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)
  (setq company-clang-arguments '("-framework" "UIKit"))
  :bind
  ("TAB" . company-indent-or-complete-common))

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

(use-package jade-mode
  :ensure t
  :config
  (setq sws-tab-width 4))

(use-package highlight-symbol
  :ensure t
  :config
  (add-hook 'lisp-mode-hook 'highlight-symbol-mode)
  (add-hook 'ruby-mode-hook 'highlight-symbol-mode))

(use-package robe
  :ensure t)

(use-package ruby-mode
  :init
  (setq ruby-deep-indent-paren nil)
  :ensure t)

(use-package sh-mode
  :init
  (setq sh-basic-offset 2)
  (setq sh-basic-indentation 2)
  :mode ("\\.bats$" . sh-mode))

(use-package vkill
  :commands vkill
  :ensure t
  :bind ("C-x L" . vkill-and-helm-occur)
  :preface
  (defun vkill-and-helm-occur ()
    (interactive)
    (vkill)
    (call-interactively #'helm-occur))
  :config
  (setq vkill-show-all-processes t))

(use-package yasnippet
  :ensure t
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
  :ensure t
  :init
  (setq emmet-move-cursor-between-quotes t)
  :config
  (add-hook 'sgml-mode-hook 'emmet-mode)
  (add-hook 'css-mode-hook  'emmet-mode)
  (add-hook 'web-mode-hook  'emmet-mode))

(setq uniquify-strip-common-suffix nil)

(add-to-list 'package-archives
             '("SunriseCommander" . "http://joseito.republika.pl/sunrise-commander/") t)

(require 'sunrise-commander)
(require 'sunrise-x-loop)
(require 'sunrise-x-tree)


(add-to-list 'completion-ignored-extensions ".test")

(defun tj-disable-final-newline ()
  (interactive)
  (set (make-local-variable 'require-final-newline) nil))

(add-to-list 'exec-path "~/dev/bin")

(global-set-key (kbd "s-t") 'projectile-find-file)
(global-set-key (kbd "s-T") 'prelude-goto-symbol)
(global-set-key (kbd "C-M-t") 'projectile-switch-project)

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
(window-number-mode)

(require 'dot-mode)
(add-hook 'find-file-hooks 'dot-mode-on)


(eval-after-load "prelude-mode"
  '(progn
     (define-key prelude-mode-map (kbd "C-c s") nil)
     (define-key prelude-mode-map (kbd "C-c i") nil)))

(setq helm-split-window-default-side "right")

(defun tj-comment-line ()
  (interactive)
  (call-interactively #'comment-line)
  (unless (region-active-p) (previous-line)))

(global-set-key (kbd "M-;") 'tj-comment-line)

(setq comment-multi-line t)
(setq-default css-indent-offset 2)
(add-to-list 'projectile-globally-ignored-directories "Godeps/_workspace")
(add-to-list 'projectile-globally-ignored-directories "_build")
(add-to-list 'projectile-globally-ignored-directories "deps")
(add-to-list 'projectile-globally-ignored-directories "node_modules")
(setq-default indent-tabs-mode nil)

(global-set-key (kbd "s-s") 'save-buffer)

(add-to-list 'vc-directory-exclusion-list "node_modules")

(with-eval-after-load 'speedbar
  (speedbar-add-ignored-directory-regexp "node_modules")
  (mapc
   (lambda (ext)
     (speedbar-add-supported-extension ext))
   '(".rb" ".z?sh" ".go" ".hs" ".clj")))

(defun tj/projectile-find-other-file (&optional args)
  "Find other js file for `ARGS'."
  (interactive)
  (cond
   ((derived-mode-p `js-mode)
    (let* ((root (projectile-project-root))
           (this-file (buffer-file-name))
           (file-no-prefix (s-chop-prefix root this-file)))
      (if (s-prefix-p "test" file-no-prefix t)
          (find-file-other-window (concat root (s-chop-prefix "test" file-no-prefix)))
        (find-file-other-window (concat (file-name-as-directory (concat root "test")) (s-chop-prefix root this-file))))))
   ((derived-mode-p 'go-mode)
    (let* ((this-file (buffer-file-name))
           (file-no-prefix (s-chop-suffix ".go" this-file)))
      (if (s-ends-with? "test" file-no-prefix t)
          (find-file-other-window (concat (s-chop-suffix "_test" file-no-prefix) ".go"))
        (find-file-other-window (concat file-no-prefix "_test.go")))))
   (t (apply 'projectile-find-other-file args))))

(defun my-projectile-hook ()
  "Customize projectile how I want it."
  (define-key projectile-mode-map (kbd "C-c p a") 'tj/projectile-find-other-file)
  (define-key projectile-mode-map (kbd "s-p a") 'tj/projectile-find-other-file))
(add-hook 'projectile-mode-hook 'my-projectile-hook)

(use-package avy
  :ensure t
  :bind
  ("C-j" . avy-goto-char-2)
  ("M-g g" . avy-goto-line))

(use-package avy)

(use-package avy-zap
  :ensure t
  :bind
  (("M-Z" . avy-zap-up-to-char-dwim)))

(add-hook 'cider-mode-hook #'eldoc-mode)

(add-hook 'cider-repl-mode-hook #'subword-mode)
(add-hook 'cider-repl-mode-hook #'paredit-mode)

(defun tj/visit-pull-request-url ()
  "Visit the current branch's PR on Github."
  (interactive)
  (browse-url
   (format "https://github.com/%s/pull/new/%s"
           (replace-regexp-in-string
            "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
            (magit-get "remote"
                       (magit-get-remote)
                       "url"))
           (cdr (or (magit-get-remote-branch)
                    (user-error "No remote branch"))))))

(eval-after-load 'magit
  '(define-key magit-mode-map "v"
     #'tj/visit-pull-request-url))

(use-package nlinum
  :ensure t
  :config
  (global-nlinum-mode 1))

(setq nlinum--width 3)
(setq nlinum-format "%d ")

(use-package eshell
  :ensure t
  
  :init
  (require 'em-smart)
  
  :bind ("M-s" . other-window-or-split)

  :config

  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)

  (defun tjj-eshell-mode-hook ()
    (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
    (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env)))
  
  
  (add-hook 'eshell-mode-hook 'tjj-eshell-mode-hook)
  
  (use-package multi-eshell
    :ensure t

    :init
    (setq multi-eshell-shell-function '(eshell))))

(use-package magit
  :ensure t
  :init
  (setq magit-push-always-verify nil)
  :config
  (global-unset-key [tab]))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile")

(use-package docker
  :init
  :ensure t)

(use-package hcl-mode
  :ensure t)

(use-package sr-speedbar
  :ensure t)

(use-package projectile-speedbar
  :ensure t
  :bind (("C-c s" . projectile-speedbar-open-current-buffer-in-tree)))

(use-package magit :ensure t)

(use-package ag :ensure t)

(use-package restclient :ensure t
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
  :ensure t
  :config
  (osx-clipboard-mode))

(use-package ediff
  :ensure t
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

(use-package company-tern
  :ensure t
  :defer t
  :init  
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-tern)))

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

  
  :mode
  ("\\.js\\'" . js2-mode)
  ("\\.json\\'" . js2-mode)
  
  :interpreter ("node" . js2-mode)
  :bind
  ("M-j" . c-indent-new-comment-line)
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
    (electric-indent-mode -1)
    (tern-mode)
    (flycheck-mode)
    (subword-mode))
  (add-hook 'js2-mode-hook 'my-js2-mode-hook))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(yas-global-mode)

(use-package web-mode
  :ensure t
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
  (defun tj--web-mode-hook nil
    (subword-mode)
    (setq comment-start "//"
          comment-end   "")
    (setq-local imenu-create-index-function 'tj--js-imenu-make-index))
  (add-hook 'web-mode-hook #'tj--web-mode-hook)
  :mode
  ("\\.hbs$" . web-mode)
  ("\\.eex$" . web-mode)
  ("\\.js$" . web-mode)
  :config
  (defun tj/html-insert-open-and-close-tag ()
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
  (defun tj/erb-insert-or-toggle-erb-tag ()
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
  ("C-c >" . tj/erb-insert-or-toggle-erb-tag)
  ("C-c <" . tj/html-insert-open-and-close-tag))

(use-package ag
  :ensure t
  :config
  (global-set-key (kbd "C-x C-a") 'ag-regexp))

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

(use-package company-quickhelp          ; Documentation popups for Company
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

(use-package company-go
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-go)))

(use-package go-eldoc
  :ensure t
  :defer t
  :init
  (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package go-rename
  :ensure t
  :defer t
  :init
  (autoload 'go-rename "go-rename" nil t))

(use-package go-mode
  :defer t
  :ensure t
  :init
  ;; (setq gofmt-command "gofmt")  
  (setenv "GOPATH" (expand-file-name (concat (getenv "HOME") "/dev")))
  (setenv "GOROOT" "/usr/local/opt/go/libexec")
  (setq company-go-show-annotation t)
  
  :bind

  ("C-c C-d" . go-guru-describe)
  ("C-c C-g" . helm-go-package)
  ("C-c C-c" . godoc-at-point)
  ("M-." . go-guru-definition)
  
  :config
  (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (add-to-list (make-local-variable 'company-backends) 'company-go)
  (defun my-go-hook ()
    (subword-mode)
    (flycheck-mode)
    (if (not (string-match "go" compile-command))
        (set (make-local-variable 'compile-command)
             "go build -v && go test -v && go vet")))
  (add-hook 'go-mode-hook 'my-go-hook)
  (use-package gotest :ensure t))

(use-package elixir-mode
  :ensure t
  :init
  (defun my-elixir-hook ()
    (subword-mode)
    (flycheck-mode)
    (alchemist-mode))
  (add-hook 'elixir-mode-hook 'my-elixir-hook))

(use-package elixir-yasnippets
  :ensure t)

(use-package alchemist
  :ensure t
  :bind
  ("M-j" . newline-and-indent))

(use-package embrace
  :ensure t)


(use-package ag
  :ensure t)

(use-package helm-ag
  :ensure t
  :bind (("s-F" . ag))
  )

(use-package helm
  :ensure t
  :bind (("C-c h"   . helm-command-prefix)
         ("C-h a"   . helm-apropos)
         ("C-x f"   . helm-multi-files)
         ("C-c C-o"   . helm-occur)
         ("C-c i" . helm-imenu)
         ("M-H"     . helm-resume)
         ("M-x"     . helm-M-x)
         ("C-x C-m"     . helm-M-x)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("M-y" . helm-show-kill-ring)
         )

  :preface
  (defun my-helm-find ()
    (interactive)
    (helm-find nil))

  :config

  (use-package helm-swoop
    :ensure t

    :init

    (setq helm-buffers-fuzzy-matching t)
    (setq helm-recentf-fuzzy-match t)
    (setq helm-imenu-fuzzy-match t)
    (setq helm-semantic-fuzzy-match t)
    (setq helm-apropos-fuzzy-match t)
    (setq helm-M-x-fuzzy-match t) 
    (setq helm-swoop-use-fuzzy-match t)
    (setq helm-swoop-use-line-number-face t)
    
    :bind (("M-i" . helm-swoop))

    :config

    (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch))
  
  (use-package helm-mode
    :diminish helm-mode
    :init
    (helm-mode 1))

  (use-package helm-multi-match)

  (helm-autoresize-mode 1)

  (bind-key "<tab>" #'helm-execute-persistent-action helm-map)
  (bind-key "C-i" #'helm-execute-persistent-action helm-map)
  (bind-key "C-z" #'helm-select-action helm-map)
  (bind-key "A-v" #'helm-previous-page helm-map))


(global-set-key (kbd "C-c c") #'embrace-commander)

(use-package web-beautify
  :ensure t)

(defadvice compile-goto-error (around my-compile-goto-error activate)
  (let ((display-buffer-overriding-action '(display-buffer-reuse-window (inhibit-same-window . nil))))
    ad-do-it))

;; Funcs


(defun tjj-esformatter ()
  (interactive)
  (shell-command (format "esformatter -i %s" (buffer-file-name))))

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
  :ensure t

  :init

  (defun my-sql-mode-hook ()
    (electric-indent-mode -1))
  (add-hook 'sql-mode-hook 'my-sql-mode-hook)
  
  :config
  (eval-after-load "sql"
    '(load-library "sql-indent")))

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

(defun generalized-shell-command (command arg)
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

(server-start)
