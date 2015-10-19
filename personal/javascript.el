(require 's)

(setq-default js2-allow-rhino-new-expr-initializer nil)
(setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON" "describe" "it" "setup" "afterEach" "beforeEach" "before" "after"))
(setq-default js2-idle-timer-delay 0.1)
(setq-default js2-mirror-mode nil)
(setq-default js2-strict-inconsistent-return-warning nil)
(setq-default js2-include-rhino-externs nil)
(setq-default js2-include-gears-externs nil)
(setq-default js2-concat-multiline-strings 'eol)
(setq-default js2-rebind-eol-bol-keys nil)
(setq-default js2-basic-offset 2)
(setq-default js2-bounce-indent-p t)
;; (setq-default js2-mode-indent-ignore-first-tab nil)
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason
(setq js-indent-level 2)

(autoload 'flymake-jshint "flymake-jshint"
  "Error and linting support mode for JavaScript." t nil)

;; (add-to-list 'interpreter-mode-alist '("node" . js2-mode))
(add-to-list 'interpreter-mode-alist '("node" . js-mode))

(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))


(add-hook 'js2-mode-hook (lambda () (electric-indent-mode -1)))
;; (add-hook 'js2-mode-hook '(lambda ()
;;                             (local-set-key (kbd "RET") 'newline-and-indent)))
(add-hook 'js2-mode-hook
          '(lambda ()
             (local-set-key
              (kbd "C-o")
              (lambda ()
                (interactive)
                (previous-line)
                (end-of-line)
                (js2-line-break)))))

(add-hook 'js2-mode-hook (lambda () (electric-pair-mode -1)))

;; (add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$" . web-mode))

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
  (cond
   ((or (derived-mode-p 'js2-mode) (derived-mode-p `js-mode))
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

(add-hook 'js2-mode-hook (lambda () (define-key js2-mode-map (kbd "C-c p a") 'tj/projectile-find-other-file)))
(add-hook 'js2-mode-hook (lambda () (define-key js2-mode-map (kbd "s-p a") 'tj/projectile-find-other-file)))
(add-hook 'projectile-mode-hook (lambda () (define-key projectile-mode-map (kbd "C-c p a") 'tj/projectile-find-other-file)))
(add-hook 'projectile-mode-hook (lambda () (define-key projectile-mode-map (kbd "s-p a") 'tj/projectile-find-other-file)))

;; (add-hook 'js2-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends)
;;                  '((company-dabbrev-code company-yasnippet)))))

(add-hook 'js2-mode-hook (lambda () (tern-mode)))


;; Custom indentation function since JS2 indenting is terrible.
;; Uses js-mode's (espresso-mode) indentation semantics.
;;
;; Based on: http://mihai.bazon.net/projects/editing-javascript-with-emacs-js2-mode
;; (Thanks!)
(defun my-js2-indent-function ()
  (interactive)
  (save-restriction
    (widen)
    (let* ((inhibit-point-motion-hooks t)
           (parse-status (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation)))
           (indentation (js--proper-indentation parse-status))
           node)

      (save-excursion

        ;; I like to indent case and labels to half of the tab width
        (back-to-indentation)
        (if (looking-at "case\\s-")
            (setq indentation (+ indentation (/ js-indent-level 2))))

        ;; consecutive declarations in a var statement are nice if
        ;; properly aligned, i.e:
        ;;
        ;; var foo = "bar",
        ;;     bar = "foo";
        (setq node (js2-node-at-point))
        (when (and node
                   (= js2-NAME (js2-node-type node))
                   (= js2-VAR (js2-node-type (js2-node-parent node))))
          (setq indentation (+ 4 indentation))))

      (indent-line-to indentation)
      (when (> offset 0) (forward-char offset)))))

(defun my-js2-mode-hook ()
  (if (not (boundp 'js--proper-indentation))
      (progn (js-mode)
             (remove-hook 'js2-mode-hook 'my-js2-mode-hook)
             (js2-mode)
             (add-hook 'js2-mode-hook 'my-js2-mode-hook)))
  (set (make-local-variable 'indent-line-function) 'my-js2-indent-function)
  (define-key js2-mode-map [(return)] 'newline-and-indent)
  (message "JS2 mode hook ran."))

(defun my-js-mode-hook ()
  (subword-mode)
  (yas-minor-mode-on)
  (flycheck-mode 1)
  (electric-indent-mode -1)
  (tern-mode)
  (message "js-mode hook ran."))

(define-key js-mode-map (kbd "M-j") 'c-indent-new-comment-line)

(add-hook 'js-mode-hook 'my-js-mode-hook)

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint)))

;; use eslint with web-mode for jsx files
;; (flycheck-add-mode 'javascript-eslint 'js-mode)

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(json-jsonlist)))


(provide 'javascript)
