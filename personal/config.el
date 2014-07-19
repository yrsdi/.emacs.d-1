(load-theme 'solarized-light t)
(setq mac-allow-anti-aliasing nil)
(set-frame-font (font-spec :family "Monaco" :size 10))
(setq default-frame-alist '((font . "Monaco-10")))
(global-linum-mode 1)

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

(setq org-agenda-files (split-string (shell-command-to-string "find ~/org/*")))

(setq company-idle-delay .3)

(add-to-list 'company-dabbrev-code-modes 'markdown-mode)

(setq multi-term-program "/bin/bash")
(setq explicit-shell-file-name "/bin/bash")


(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))

(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))

(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)

(setq company-clang-arguments '("-framework" "UIKit"))

(setq prelude-clean-whitespace-on-save t)

(add-hook 'jade-mode-hook
          (lambda () (setq sws-tab-width 4)))

(require 'highlight-symbol)
(highlight-symbol-mode)


;; for edit in chrome extension
(edit-server-start)
