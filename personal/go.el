(setenv "GOPATH" (expand-file-name (concat (getenv "HOME") "/dev")))

(require 'company-go)

(add-hook 'before-save-hook 'gofmt-before-save)

(add-hook 'go-mode-hook 'subword-mode)
(add-hook 'go-mode-hook 'company-mode)
(add-hook 'go-mode-hook (lambda ()

                         (set (make-local-variable 'company-backends) '(company-go))
                          (company-mode)))

(add-hook 'before-save-hook 'gofmt-before-save)

(provide 't-go)
