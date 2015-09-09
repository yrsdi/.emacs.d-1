(disable-theme 'zenburn)
(menu-bar-mode -1)

(setq mac-allow-anti-aliasing nil)
(set-frame-font (font-spec :family "Monaco" :size 10))
(setq default-frame-alist '((font . "Monaco-10")))
(global-linum-mode -1)
(global-hl-line-mode -1)

(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(add-hook 'objc-mode-hook 'company-mode)
(add-hook 'objc-mode-hook (lambda ()
                            (set (make-local-variable 'company-backends) '(company-clang))
                            (company-mode)))


(setq mac-allow-anti-aliasing t)

(defun eshell-mode-hook-func ()
  (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH")))
  (define-key eshell-mode-map (kbd "M-s") 'other-window-or-split))

(add-hook 'eshell-mode-hook 'eshell-mode-hook-func)

(setq-default tab-width 2)

(setq org-agenda-files (split-string (shell-command-to-string "find ~/Dropbox/org/*")))

(setq company-idle-delay .3)

(add-to-list 'company-dabbrev-code-modes 'markdown-mode)

(setq multi-term-program "/bin/bash")
(setq explicit-shell-file-name "/bin/bash")


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

(setq company-clang-arguments '("-framework" "UIKit"))

(setq prelude-clean-whitespace-on-save t)

(add-hook 'jade-mode-hook
          (lambda () (setq sws-tab-width 4)))

(require 'highlight-symbol)
(add-hook 'js3-mode-hook 'highlight-symbol-mode)
(add-hook 'lisp-mode-hook 'highlight-symbol-mode)
(add-hook 'ruby-mode-hook 'highlight-symbol-mode)
(add-hook 'markdown-mode-hook 'highlight-symbol-mode)

(add-to-list 'auto-mode-alist '("\\.bats$" . sh-mode))
(add-hook 'sh-mode-hook (lambda nil
                          (setq sh-basic-offset 2)
                          (setq sh-basic-indentation 2)))

;; for edit in chrome extension
(edit-server-start)

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

(global-set-key [tab] 'tab-indent-or-complete)

(setq-default global-font-lock-mode nil)

(global-font-lock-mode -1)

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
(global-set-key "\M-Z" 'zap-up-to-char)

(setq uniquify-strip-common-suffix nil)

(add-to-list 'load-path "~/.emacs.d/el-get/el-get")

(require 'el-get)

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))

(add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")

(add-to-list 'package-archives
             '("SunriseCommander" . "http://joseito.republika.pl/sunrise-commander/") t)

(require 'sunrise-commander)
(require 'sunrise-x-loop)
(require 'sunrise-x-tree)

(setq multi-term-program "/bin/zsh")

(add-to-list 'completion-ignored-extensions ".test")

(el-get 'sync)

(defun tj-disable-final-newline ()
  (interactive)
  (set (make-local-variable 'require-final-newline) nil))

(add-to-list 'exec-path "~/dev/bin")

(global-set-key (kbd "s-t") 'projectile-find-file)
(global-set-key (kbd "s-F") 'ack)

(window-numbering-mode)

(require 'dot-mode)
(add-hook 'find-file-hooks 'dot-mode-on)
