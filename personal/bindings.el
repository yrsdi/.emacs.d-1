(require 'helm-open-github)

(eval-after-load 'prelude-mode
  '(progn
     (define-key prelude-mode-map (kbd "C-c o") nil)
     (define-key prelude-mode-map (kbd "C-c u") nil)
     ))
(define-prefix-command 't-open-map)
(global-set-key (kbd "C-c o") 't-open-map)
(global-set-key (kbd "C-c o o") 'prelude-open-with)
(global-set-key (kbd "C-c o f") 'helm-open-github-from-file)
(global-set-key (kbd "C-c o c") 'helm-open-github-from-commit)
(global-set-key (kbd "C-c o i") 'helm-open-github-from-issues)
(global-set-key (kbd "C-c o p") 'helm-open-github-from-pull-requests)
(global-set-key (kbd "C-c u") 'browse-url-at-point)

(global-set-key (kbd "<C-return>") 't-newline-and-indent-up)

(global-set-key (kbd "C-\\") 'er/expand-region)
