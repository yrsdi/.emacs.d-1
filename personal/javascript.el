(add-to-list 'interpreter-mode-alist '("node" . js-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js-mode))
(add-to-list 'auto-mode-alist '("\\.hbs$" . web-mode))
(add-to-list 'company-backends 'company-tern)

(setq js-indent-level 2)
(setq company-tern-property-marker "")

(defun my-js-mode-hook ()
  "My hook to custom `js-mode' how I want it."
  (subword-mode)
  (yas-minor-mode-on)
  (flycheck-mode 1)
  (electric-indent-mode -1)
  (tern-mode)
  (define-key js-mode-map (kbd "M-j") 'c-indent-new-comment-line)
  (message "js-mode hook ran."))
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
