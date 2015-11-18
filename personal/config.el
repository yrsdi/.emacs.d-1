;; turn of theme
(disable-theme 'zenburn)
(setq-default global-font-lock-mode nil)
(global-font-lock-mode -1)

(menu-bar-mode -1)

;; font
(setq mac-allow-anti-aliasing t)
(setq default-frame-alist '((font . "FiraCode-12")))
(set-frame-font (font-spec :family "Fira Code" :size 12))

(global-linum-mode -1)
(global-hl-line-mode -1)

(setq-default tab-width 2)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(defun eshell-mode-hook-func ()
  (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (define-key eshell-mode-map (kbd "M-s") 'other-window-or-split))
(add-hook 'eshell-mode-hook 'eshell-mode-hook-func)

(setq org-agenda-files (split-string (shell-command-to-string "find ~/Dropbox/org/*")))

;; company mode config
(setq company-idle-delay .3)

(setq company-clang-arguments '("-framework" "UIKit"))

;; terminal
(setq multi-term-program "/bin/bash")
(setq explicit-shell-file-name "/bin/bash")

;; dired
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

(setq prelude-clean-whitespace-on-save t)

(add-hook 'jade-mode-hook
          (lambda () (setq sws-tab-width 4)))

(require 'highlight-symbol)
(add-hook 'lisp-mode-hook 'highlight-symbol-mode)
(add-hook 'ruby-mode-hook 'highlight-symbol-mode)


(add-to-list 'auto-mode-alist '("\\.bats$" . sh-mode))
(add-hook 'sh-mode-hook (lambda nil
                          (setq sh-basic-offset 2)
                          (setq sh-basic-indentation 2)))

(eval-after-load "web-mode"
  '(setq web-mode-tag-auto-close-style 2))

(require 'yasnippet)
(add-to-list 'yas/root-directory "~/.emacs.d/yasnippet-snippets")
(yas-reload-all)

(defun check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun do-yas-expand ()
  (let ((yas/fallback-behavior 'return-nil))
    (yas/expand)))

(defun tab-indent-or-complete ()
  (interactive)
  (if (minibufferp)
      (minibuffer-complete)
    (if (or (not yas/minor-mode)
            (null (do-yas-expand)))
        (if (check-expansion)
            (company-complete-common)
          (indent-for-tab-command)))))

;; (global-set-key [tab] 'tab-indent-or-complete)

(add-hook 'find-file-hook (lambda () (global-font-lock-mode -1)))
(add-hook 'magit-mode-hook (lambda () (global-unset-key [tab])))

(add-hook 'sgml-mode-hook 'emmet-mode)
(add-hook 'css-mode-hook  'emmet-mode)
(add-hook 'web-mode-hook  'emmet-mode)
(setq emmet-move-cursor-between-quotes t)

(setq ring-bell-function 'ignore)

(server-start)
(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)
(global-set-key (kbd "M-Z") 'zap-up-to-char)

(setq uniquify-strip-common-suffix nil)

(add-to-list 'package-archives
             '("SunriseCommander" . "http://joseito.republika.pl/sunrise-commander/") t)

(require 'sunrise-commander)
(require 'sunrise-x-loop)
(require 'sunrise-x-tree)

(setq org-src-lang-modes '(
                           ("screen" . sh)
                           ("ocaml" . tuareg)
                           ("elisp" . emacs-lisp)
                           ("lisp" . lisp)
                           ("ditaa" . artist)
                           ("asymptote" . asy)
                           ("cl" . lisp)
                           ("dot" . graphviz-dot)))

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

(defun tj/web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  )
(add-hook 'web-mode-hook  'tj/web-mode-hook)

(eval-after-load "prelude-mode"
  '(define-key prelude-mode-map (kbd "C-c s") nil))

(setq helm-split-window-default-side "right")
(setq comment-multi-line t)
(setq-default css-indent-offset 2)
(add-to-list 'projectile-globally-ignored-directories "Godeps/_workspace")
(setq-default indent-tabs-mode nil)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-c s") 'sr-speedbar-toggle)
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

(global-set-key (kbd "C-j") 'ace-jump-mode)

(add-hook 'cider-mode-hook #'eldoc-mode)
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(global-set-key (kbd "TAB") #'company-indent-or-complete-common)
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

(require 'org-table)

(osx-clipboard-mode)
