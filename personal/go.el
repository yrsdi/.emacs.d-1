(setenv "GOPATH" (expand-file-name (concat (getenv "HOME") "/dev")))

(require 'company-go)

(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook 'subword-mode)
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()

                         (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

;; (add-hook 'before-save-hook 'gofmt-before-save)

(defun my-go-mode-hook ()
                                        ; Use goimports instead of go-fmt
(setq gofmt-command "goimports")
                                        ; Call Gofmt before saving
(add-hook 'before-save-hook 'gofmt-before-save)
                                        ; Customize compile command to run go build
(if (not (string-match "go" compile-command))
    (set (make-local-variable 'compile-command)
         "go build -v && go test -v && go vet"))

(local-set-key (kbd "M-.") 'godef-jump))
(add-hook 'go-mode-hook 'my-go-mode-hook)

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

(provide 't-go)
