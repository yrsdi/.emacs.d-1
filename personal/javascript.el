(require 's)

(setq-default js2-allow-rhino-new-expr-initializer nil)
(setq-default js2-enter-indents-newline nil)
(setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "describe" "it" "setup" "afterEach" "beforeEach" "before" "after"))
(setq-default js2-idle-timer-delay 0.1)
(setq-default js2-indent-on-enter-key nil)
(setq-default js2-mirror-mode nil)
(setq-default js2-strict-inconsistent-return-warning nil)
(setq-default js2-include-rhino-externs nil)
(setq-default js2-include-gears-externs nil)
(setq-default js2-concat-multiline-strings 'eol)
(setq-default js2-rebind-eol-bol-keys nil)
(setq-default js2-basic-offset 2)
(setq-default js2-bounce-indent-p t)
;; (setq-default js2-bounce-indent-p nil)
(setq-default js2-mode-indent-ignore-first-tab nil)
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason
(setq js2-auto-indent-p nil)

(autoload 'flymake-jshint "flymake-jshint"
  "Error and linting support mode for JavaScript." t nil)

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))
(add-hook 'js2-mode-hook (lambda () (electric-indent-mode -1)))
(add-hook 'js2-mode-hook (lambda () (electric-pair-mode -1)))

(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;; After js2 has parsed a js file, we look for jslint globals decl comment ("/* global Fred, _, Harry */") and
;; add any symbols to a buffer-local var of acceptable global vars
;; Note that we also support the "symbol: true" way of specifying names via a hack (remove any ":true"
;; to make it look like a plain decl, and any ':false' are left behind so they'll effectively be ignored as
;; you can;t have a symbol called "someName:false"
(add-hook 'js2-post-parse-callbacks
          (lambda ()
            (when (> (buffer-size) 0)
              (let ((btext (replace-regexp-in-string
                            ": *true" " "
                            (replace-regexp-in-string "[\n\t ]+" " " (buffer-substring-no-properties 1 (buffer-size)) t t))))
                (mapc (apply-partially 'add-to-list 'js2-additional-externs)
                      (split-string
                       (if (string-match "/\\* *global *\\(.*?\\) *\\*/" btext) (match-string-no-properties 1 btext) "")
                       " *, *" t))
                ))))

(require 'json)

;; Tern.JS
(add-to-list 'company-backends 'company-tern)

(setq company-tern-property-marker "")


(defun my-aget (key map)
  (cdr (assoc key map)))

(add-hook 'js2-mode-hook (lambda () (yas-minor-mode-on)))
(add-hook 'js2-mode-hook (lambda () (subword-mode)))

(defun tj/projectile-find-other-file (&optional args)
  (interactive)
  (if (derived-mode-p 'js2-mode)
    (let* ((root (projectile-project-root))
           (this-file (buffer-file-name))
           (file-no-prefix (s-chop-prefix root this-file))
           )
      (if (s-prefix-p "test" file-no-prefix t)
          (find-file-other-window (concat root (s-chop-prefix "test" file-no-prefix)))
        (find-file-other-window (concat (file-name-as-directory (concat root "test")) (s-chop-prefix root this-file)))
        )
)

    (apply 'projectile-find-other-file args)))

(add-hook 'js2-mode-hook (lambda () (define-key js2-mode-map (kbd "C-c p a") 'tj/projectile-find-other-file)))
(add-hook 'js2-mode-hook (lambda () (define-key js2-mode-map (kbd "s-p a") 'tj/projectile-find-other-file)))
(add-hook 'projectile-mode-hook (lambda () (define-key projectile-mode-map (kbd "C-c p a") 'tj/projectile-find-other-file)))
(add-hook 'projectile-mode-hook (lambda () (define-key projectile-mode-map (kbd "s-p a") 'tj/projectile-find-other-file)))

;; (add-hook 'js2-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends)
;;                  '((company-dabbrev-code company-yasnippet)))))

(add-hook 'js2-mode-hook (lambda () (tern-mode)))

(provide 'javascript)
