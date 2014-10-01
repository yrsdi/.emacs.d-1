(setq-default js2-allow-rhino-new-expr-initializer nil)
(setq-default js2-auto-indent-p t)
(setq-default js2-enter-indents-newline t)
(setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
(setq-default js2-idle-timer-delay 0.1)
(setq-default js2-indent-on-enter-key t)
(setq-default js2-mirror-mode nil)
(setq-default js2-strict-inconsistent-return-warning nil)
(setq-default js2-include-rhino-externs nil)
(setq-default js2-include-gears-externs nil)
(setq-default js2-concat-multiline-strings 'eol)
(setq-default js2-rebind-eol-bol-keys nil)
(setq-default js2-basic-offset 2)
(setq-default js2-bounce-indent-p t)
(setq-default js2-mode-indent-ignore-first-tab nil)
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning t) ;; jshint does not warn about this now for some reason

(autoload 'flymake-jshint "flymake-jshint"
  "Error and linting support mode for JavaScript." t nil)

(add-to-list 'interpreter-mode-alist '("node" . js2-mode))

(add-hook 'js2-mode-hook (lambda () (flycheck-mode 1)))
(add-hook 'js2-mode-hook (lambda () (electric-indent-mode -1)))

(require 'js2-refactor)
(js2r-add-keybindings-with-prefix "C-c C-m")

(add-to-list 'auto-mode-alist '("\\.json$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(defun js2r--setup-wrapping-pair (open close)
  (define-key js2-mode-map (read-kbd-macro open) (lambda (js2r--self-insert-wrapping open close)))
  (unless (s-equals? open close)
    (define-key js2-mode-map (read-kbd-macro close) (lambda (js2r--self-insert-closing open close)))))

(defun js2r--self-insert-wrapping (open close)
  (cond
   ((use-region-p)
    (save-excursion
      (let ((beg (region-beginning))
            (end (region-end)))
        (goto-char end)
        (insert close)
        (goto-char beg)
        (insert open))))

   ((and (s-equals? open close)
         (looking-back (regexp-quote open))
         (looking-at (regexp-quote close)))
    (forward-char (length close)))

   ((js2-mode-inside-comment-or-string)
    (funcall 'self-insert-command 1))

   (:else
    (let ((end (js2r--something-to-close-statement)))
      (insert open close end)
      (backward-char (+ (length close) (length end)))
      (js2r--remove-all-this-cruft-on-backward-delete)))))

(defun js2r--remove-all-this-cruft-on-backward-delete ()
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "DEL") 'undo-tree-undo)
     (define-key map (kbd "C-h") 'undo-tree-undo)
     map) nil))

(defun js2r--self-insert-closing (open close)
  (if (and (looking-back (regexp-quote open))
           (looking-at (regexp-quote close)))
      (forward-char (length close))
    (funcall 'self-insert-command 1)))

(defun js2r--does-not-need-semi ()
  (save-excursion
    (back-to-indentation)
    (or (looking-at "if ")
        (looking-at "function ")
        (looking-at "for ")
        (looking-at "while ")
        (looking-at "try ")
        (looking-at "} else "))))

(defun js2r--comma-unless (delimiter)
  (if (looking-at (concat "[\n\t\r ]*" (regexp-quote delimiter)))
      ""
    ","))

(defun js2r--something-to-close-statement ()
  (cond
   ((and (js2-block-node-p (js2-node-at-point)) (looking-at " *}")) ";")
   ((not (eolp)) "")
   ((js2-array-node-p (js2-node-at-point)) (js2r--comma-unless "]"))
   ((js2-object-node-p (js2-node-at-point)) (js2r--comma-unless "}"))
   ((js2-object-prop-node-p (js2-node-at-point)) (js2r--comma-unless "}"))
   ((js2-call-node-p (js2-node-at-point)) (js2r--comma-unless ")"))
   ((js2r--does-not-need-semi) "")
   (:else ";")))

(js2r--setup-wrapping-pair "(" ")")
(js2r--setup-wrapping-pair "{" "}")
(js2r--setup-wrapping-pair "[" "]")
(js2r--setup-wrapping-pair "\"" "\"")
(js2r--setup-wrapping-pair "'" "'")

;;

(define-key js2-mode-map (kbd "C-c RET jt") 'jump-to-test-file)
(define-key js2-mode-map (kbd "C-c RET ot") 'jump-to-test-file-other-window)
(define-key js2-mode-map (kbd "C-c RET js") 'jump-to-source-file)
(define-key js2-mode-map (kbd "C-c RET os") 'jump-to-source-file-other-window)
(define-key js2-mode-map (kbd "C-c RET jo") 'jump-between-source-and-test-files)
(define-key js2-mode-map (kbd "C-c RET oo") 'jump-between-source-and-test-files-other-window)

(define-key js2-mode-map (kbd "C-c RET dp") 'js2r-duplicate-object-property-node)

(define-key js2-mode-map (kbd "C-c RET ta") 'toggle-assert-refute)

(defadvice js2r-inline-var (after reindent-buffer activate)
  (cleanup-buffer))

(defun js2-hide-test-functions ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (ignore-errors
      (while (re-search-forward "\"[^\"]+\": function (")
        (js2-mode-hide-element)))))

(define-key js2-mode-map (kbd "C-c t") 'js2-hide-test-functions)

;; When renaming/deleting js-files, check for corresponding testfile
(define-key js2-mode-map (kbd "C-x C-r") 'js2r-rename-current-buffer-file)
(define-key js2-mode-map (kbd "C-x C-k") 'js2r-delete-current-buffer-file)

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

(defun my-aget (key map)
  (cdr (assoc key map)))

(defun js2-fetch-autolint-externs (file)
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
      (add-to-list 'js2-additional-externs it))))

(defun cjsp--eldoc-innards (beg)
  (save-excursion
    (goto-char beg)
    (search-forward "=")
    (let ((start (point)))
      (search-forward "*/")
      (forward-char -2)
      (buffer-substring-no-properties start (point)))))

(defun cjsp--indentation-of-html-line (html line-number)
  (with-temp-buffer
    (insert html)
    (html-mode)
    (indent-region (point-min) (point-max))
    (goto-line line-number)
    (back-to-indentation)
    (current-column)))

(defun cjsp--line-number-in-eldoc (p beg)
  (save-excursion
    (goto-char p)
    (let ((l (line-number-at-pos)))
      (goto-char beg)
      (- l (line-number-at-pos) -1))))

(defun js2-lineup-comment (parse-status)
  "Indent a multi-line block comment continuation line."
  (let* ((beg (nth 8 parse-status))
         (first-line (js2-same-line beg))
         (p (point))
         (offset (save-excursion
                   (goto-char beg)
                   (cond

                    ((looking-at "/\\*:DOC ")
                     (+ 2 (current-column)
                        (cjsp--indentation-of-html-line
                         (cjsp--eldoc-innards beg)
                         (cjsp--line-number-in-eldoc p beg))))

                    ((looking-at "/\\*")
                     (+ 1 (current-column)))

                    (:else 0)))))
    (unless first-line
      (indent-line-to offset))))

(add-hook 'js2-mode-hook (lambda () (yas-minor-mode-on)))
(add-hook 'js2-mode-hook (lambda () (subword-mode)))



(add-hook 'js2-mode-hook
          (lambda ()
            (set (make-local-variable 'company-backends)
                 '((company-dabbrev-code company-yasnippet)))))

(provide 'javascript)
