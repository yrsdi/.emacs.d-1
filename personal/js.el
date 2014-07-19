;; (autoload 'js2-mode "js2-mode" nil t)
;; (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
;; (add-to-list 'interpreter-mode-alist
;;              '("node" . js2-mode))
;; (setq-default js2-basic-offset 2)
;; (setq js-indent-level 2)
;; (setq js2-bounce-indent t)
;; (setq-default js2-allow-rhino-new-expr-initializer nil)
;; (setq-default js2-auto-indent-p nil)
;; (setq-default js2-enter-indents-newline nil)
;; (setq-default js2-global-externs '("exports" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
;; (setq-default js2-idle-timer-delay 0.1)
;; (setq-default js2-indent-on-enter-key nil)
;; (setq-default js2-mirror-mode nil)
;; (setq-default js2-strict-inconsistent-return-warning nil)
;; (setq-default js2-auto-indent-p t)
;; (setq-default js2-include-rhino-externs nil)
;; (setq-default js2-include-gears-externs nil)
;; (setq-default js2-concat-multiline-strings 'eol)
;; (setq-default js2-rebind-eol-bol-keys nil)

;; ;; Let flycheck handle parse errors
;; (setq-default js2-show-parse-errors nil)
;; (setq-default js2-strict-missing-semi-warning nil)
;; (setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

;; (defun js2-fetch-autolint-externs (file)
;;   (let* ((settings (with-temp-buffer
;;                      (insert-file-literally file)
;;                      (javascript-mode)
;;                      (let (kill-ring kill-ring-yank-pointer) (kill-comment 1000))
;;                      (->> (buffer-substring (point-min) (point-max))
;;                        (s-trim)
;;                        (s-chop-prefix "module.exports = ")
;;                        (s-chop-suffix ";")
;;                        (json-read-from-string))))
;;          (predef (->> settings
;;                    (my-aget 'linterOptions)
;;                    (my-aget 'predef))))
;;     (--each (append predef nil)
;;       (add-to-list 'js2-additional-externs it))))

;; (require 'js2-refactor)
;; (js2r-add-keybindings-with-prefix "C-c C-r")

;; (add-hook 'js2-mode-hook 'subword-mode)

;; (add-to-list 'company-dabbrev-code-modes 'js2-mode)

(setq load-path (cons "~/.emacs.d/vendor/js3-mode/" load-path))
(load "js3")


;; (setq-default js3-allow-rhino-new-expr-initializer nil)
;; (setq-default js3-idle-timer-delay 0.1)
;; (setq-default js3-mirror-mode nil)
;; (setq-default js3-strict-inconsistent-return-warning nil)
;; (setq-default js3-include-rhino-externs nil)
;; (setq-default js3-include-gears-externs nil)
;; (setq-default js3-concat-multiline-strings 'eol)
;; (setq-default js3-rebind-eol-bol-keys nil)

(autoload 'js3-mode "js3-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
(add-to-list 'interpreter-mode-alist
             '("node" . js3-mode))

(setq-default js3-global-externs '("global" "process" "exports" "module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))

(setq-default js3-bounce-indent t)
(setq-default js3-auto-indent-p t)
(setq-default js3-enter-indents-newline t)
(setq-default js3-expr-indent-offset 0)
(setq-default js3-indent-on-enter-key nil)
(setq-default js3-lazy-commas t)
(setq-default js3-lazy-dots t)
(setq-default js3-lazy-operators t)
(setq-default js3-lazy-semicolons t)
(setq-default js3-paren-indent-offset 2)
(setq-default js3-curly-indent-offset 0)
(setq-default js3-square-indent-offset 2)
(setq-default js3-indent-dots t)
(setq-default js3-consistent-level-indent-inner-bracket t)

(setq-default js-indent-level 2)


;; Let flycheck handle parse errors
(setq-default js3-show-parse-errors nil)
(setq-default js3-strict-missing-semi-warning nil)
(setq-default js3-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

(defun js3-fetch-autolint-externs (file)
  (let* ((settings (with-temp-buffer
                     (insert-file-literally file)
                     (javascript-mode)
                     (let (kill-ring kill-ring-yank-pointer) (kill-comment 1000))
                     (->> (buffer-substring (point-min) (point-max))
                       (s-trim)
                       (s-chop-prefix "module.exports = ")
                       (s-chop-suffix ";")
                       (json-read-from-string))))
         (predef (->> settings
                   (my-aget 'linterOptions)
                   (my-aget 'predef))))
    (--each (append predef nil)
      (add-to-list 'js3-additional-externs it))))

(add-hook 'js3-mode-hook 'subword-mode)

(add-to-list 'company-dabbrev-code-modes 'js-mode)
(add-to-list 'company-dabbrev-code-modes 'js3-mode)

(provide 'js)
