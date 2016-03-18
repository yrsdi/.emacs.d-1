(require 'use-package)

(setq-default global-font-lock-mode nil)
(setq mac-allow-anti-aliasing t)
(setq-default tab-width 2)
(setq multi-term-program "/usr/local/bin/zsh")
(setq explicit-shell-file-name "/usr/local/bin/zsh")
(setq ring-bell-function 'ignore)
(setq prelude-clean-whitespace-on-save t)
(setq scroll-margin 10)
(setq prelude-whitespace nil)
(setq prelude-clean-whitespace-on-save nil)

(disable-theme 'zenburn)
(global-font-lock-mode -1)
(menu-bar-mode -1)
(set-frame-font (font-spec :family "Operator Mono" :size 13 :weight 'normal))
(global-hl-line-mode -1)

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

(use-package markdown-mode
  :ensure t
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
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 3)
  (setq company-abort-manual-when-too-short 3)
  (setq company-dabbrev-downcase nil)
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


(use-package projectile-rails
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'robe-mode)
  (add-to-list 'company-backends 'company-robe)
  (add-hook 'projectile-mode-hook 'projectile-rails-on))

(use-package sh-mode
  :init
  (setq sh-basic-offset 2)
  (setq sh-basic-indentation 2)
  :mode ("\\.bats$" . sh-mode))

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


(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)
(global-set-key (kbd "M-z") 'zap-up-to-char)

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
(global-set-key (kbd "s-F") 'ag)

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
  '(define-key prelude-mode-map (kbd "C-c s") nil))

(setq helm-split-window-default-side "right")
(setq comment-multi-line t)
(setq-default css-indent-offset 2)
(add-to-list 'projectile-globally-ignored-directories "Godeps/_workspace")
(add-to-list 'projectile-globally-ignored-directories "_build")
(add-to-list 'projectile-globally-ignored-directories "deps")
(add-to-list 'projectile-globally-ignored-directories "node_modules")
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "RET") 'newline-and-indent)

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

(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)

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



(use-package linum
  :ensure t
  :init
  (setq linum-format "%d ")
  :config
  (global-linum-mode 1))

(use-package eshell
  :ensure t
  :init
  (require 'em-smart)
  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)
  (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  :bind ("M-s" . other-window-or-split))

(use-package magit
  :ensure t
  :init
  (setq magit-push-always-verify nil)
  :config
  (global-unset-key [tab]))

(use-package dockerfile-mode
  :ensure t
  :mode "Dockerfile")

(use-package hcl-mode
  :ensure t)

(use-package sr-speedbar
  :ensure t)

(use-package projectile-speedbar
  :ensure t
  :bind (("C-c s" . projectile-speedbar-open-current-buffer-in-tree)))

(use-package magit :ensure t)

(use-package ag :ensure t)
(use-package smex :ensure t)
(use-package smartparens :ensure t)
(use-package restclient :ensure t
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

(use-package js2-mode
  :ensure t
  :init
  (setq js-indent-level 2)
  (setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
  (setq-default js2-strict-inconsistent-return-warning nil)
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))
  :mode ("\\.js\\'" . js2-mode)
  :interpreter ("node" . js2-mode)
  :bind
  ("M-j" . c-indent-new-comment-line)
  ("C-c C-j" . js2-jump-to-definition)
  :config
  (defun js2-match-async-arrow-function ()
    (when (and (js2-contextual-kwd-p (js2-current-token) "async")
               (/= (js2-peek-token) js2-FUNCTION)
               (/= (js2-peek-token) js2-DOT))
      (js2-record-face 'font-lock-keyword-face)
      (js2-get-token)
      t))
  (use-package company-tern
    :ensure t
    :init
    (setq company-tern-property-marker "")
    :config
    (add-to-list 'company-backends 'company-tern))
  (defun my-js2-mode-hook ()
    (electric-indent-mode -1)
    (tern-mode)
    (flycheck-mode)
    (subword-mode))
  (add-hook 'js2-mode-hook 'my-js2-mode-hook))

(yas-global-mode)

(use-package web-mode
  :ensure t
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-auto-opening t)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-tag-auto-close-style 2)
  :mode
  ("\\.hbs$" . web-mode)
  ("\\.eex$" . web-mode)
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
  ("C-c <" . tj/html-insert-open-and-close-tag)
  )

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

(use-package go-mode
  :defer t
  :ensure t
  :init
  (setq gofmt-command "goimports")  
  (setenv "GOPATH" (expand-file-name (concat (getenv "HOME") "/dev")))
  (setenv "GOROOT" "/usr/local/opt/go/libexec")
  :config
  (load-file "$GOPATH/src/golang.org/x/tools/cmd/oracle/oracle.el")
  (add-hook 'before-save-hook 'gofmt-before-save)
  (use-package company-go :ensure t)
  (set (make-local-variable 'company-backends) '(company-go))
  (bind-key "M-." 'godef-jump)
  (defun my-go-hook ()
    (company-mode)
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

(defadvice compile-goto-error (around my-compile-goto-error activate)
  (let ((display-buffer-overriding-action '(display-buffer-reuse-window (inhibit-same-window . nil))))
    ad-do-it))

(server-start)
