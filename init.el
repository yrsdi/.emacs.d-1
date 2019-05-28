;;; Environment

(eval-and-compile
  (setq load-path
	(append (delete-dups load-path)
		'("~/.emacs.d/lisp"))))

(require 'package)

(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
;; keep the installed packages in .emacs.d
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))
(package-initialize)
;; update the package metadata is the local cache is missing
(unless package-archive-contents
  (package-refresh-contents))

(set-frame-font "Hack 10" nil t)

(define-key isearch-mode-map (kbd "C-o") #'isearch-occur)

(define-key input-decode-map [?\C-m] [C-m])

(eval-and-compile
  (mapc #'(lambda (entry)
            (define-prefix-command (cdr entry))
            (bind-key (car entry) (cdr entry)))
        '(("C-,"   . my-ctrl-comma-map)
          ("<C-m>" . my-ctrl-m-map)

          ("C-h e" . my-ctrl-h-e-map)
          ("C-h x" . my-ctrl-h-x-map)

          ("C-c b" . my-ctrl-c-b-map)
          ("C-c e" . my-ctrl-c-e-map)
          ("C-c m" . my-ctrl-c-m-map)
          ("C-c w" . my-ctrl-c-w-map)
          ("C-c y" . my-ctrl-c-y-map)
          ("C-c H" . my-ctrl-c-H-map)
          ("C-c N" . my-ctrl-c-N-map)
          ("C-c (" . my-ctrl-c-open-paren-map)
          ("C-c -" . my-ctrl-c-minus-map)
          ("C-c =" . my-ctrl-c-equals-map)
          ("C-c ." . my-ctrl-c-r-map)
          )))

(define-key isearch-mode-map [(control return)]
  #'isearch-exit-other-end)
(defun isearch-exit-other-end ()
  "Exit isearch, at the opposite end of the string."
  (interactive)
  (isearch-exit)
  (goto-char isearch-other-end))

(define-key 'help-command (kbd "C-i") #'info-display-manual)

;; smart tab behavior - indent or complete
(setq tab-always-indent 'complete)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)

(setq use-package-verbose t)

(use-package visual-fill-column
  :ensure t
  :config
  (add-hook 'visual-line-mode-hook #'visual-fill-column-mode)
  (add-hook 'markdown-mode-hook 'visual-line-mode))

;; improve find file at point to handle line numbers
(defvar ffap-file-at-point-line-number nil
  "Variable to hold line number from the last `ffap-file-at-point' call.")

(defadvice ffap-file-at-point (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and save it in `ffap-file-at-point-line-number' variable."
  (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
	 (name
	  (or (condition-case nil
		  (and (not (string-match "//" string)) ; foo.com://bar
		       (substitute-in-file-name string))
		(error nil))
	      string))
	 (line-number-string
	  (and (string-match ":[0-9]+" name)
	       (substring name (1+ (match-beginning 0)) (match-end 0))))
	 (line-number
	  (and line-number-string
	       (string-to-number line-number-string))))
    (if (and line-number (> line-number 0))
	(setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))

(defadvice ffap-guesser (after ffap-store-line-number activate)
  "Search `ffap-string-at-point' for a line number pattern and save it in `ffap-file-at-point-line-number' variable."
  (let* ((string (ffap-string-at-point)) ;; string/name definition copied from `ffap-string-at-point'
	 (name
	  (or (condition-case nil
		  (and (not (string-match "//" string)) ; foo.com://bar
		       (substitute-in-file-name string))
		(error nil))
	      string))
	 (line-number-string
	  (and (string-match ":[0-9]+" name)
	       (substring name (1+ (match-beginning 0)) (match-end 0))))
	 (line-number
	  (and line-number-string
	       (string-to-number line-number-string))))
    (if (and line-number (> line-number 0))
	(setq ffap-file-at-point-line-number line-number)
      (setq ffap-file-at-point-line-number nil))))

(defadvice find-file (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (with-no-warnings (goto-line ffap-file-at-point-line-number))
    (setq ffap-file-at-point-line-number nil)))

(defadvice find-file-at-point (after ffap-goto-line-number activate)
  "If `ffap-file-at-point-line-number' is non-nil goto this line."
  (when ffap-file-at-point-line-number
    (with-no-warnings (goto-line ffap-file-at-point-line-number))
    (setq ffap-file-at-point-line-number nil)))

(defun init-subword ()
  (let ((adv (cons 'advice
		   (lambda ()
		     (let ((os (char-syntax ?_)))
		       (modify-syntax-entry ?_ "_")
		       ad-do-it
		       (modify-syntax-entry ?_ (string os))))))
	(fun '(subword-forward subword-kill subword-backward
			       subword-backward-kill subword-downcase subword-upcase
			       subword-transpose)))
    (dolist (f fun)
      (ad-add-advice f (list 'underscore-wrap nil t adv)
		     'around 'last)
      (ad-activate f))))

(add-hook 'after-init-hook #'init-subword)

(setq auto-mode-alist
      (cons '("\\.mod$" . text-mode) auto-mode-alist))

(use-package lisp-mode
  :diminish
  :config
  (defun visit-ielm ()
    "Switch to default `ielm' buffer.
	  Start `ielm' if it's not already running."
    (interactive)
    (crux-start-or-switch-to 'ielm "*ielm*"))

  (add-hook 'emacs-lisp-mode-hook #'eldoc-mode)
  (define-key emacs-lisp-mode-map (kbd "C-c C-z") #'visit-ielm)
  (define-key emacs-lisp-mode-map (kbd "C-c C-c") #'eval-defun)
  (define-key emacs-lisp-mode-map (kbd "C-c C-b") #'eval-buffer)
  (add-hook 'lisp-interaction-mode-hook #'eldoc-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'eldoc-mode))

(use-package ielm
  :ensure t
  :config
  (add-hook 'ielm-mode-hook #'eldoc-mode) )

(use-package avy
  :ensure t
  :bind (
         ("M-T" . avy-goto-word-1)
	 ("<C-return>" . avy-goto-char-timer))
  :config
  (avy-setup-default)
  (setq avy-background t))

(use-package ido
  :ensure t
  :config
  (setq ido-use-faces nil))

(use-package ido-at-point
  :ensure t
  :config
  (ido-at-point-mode))

(use-package ido-vertical-mode
  :ensure t
  :config
  (setq ido-vertical-define-keys 'C-n-C-p-up-down-left-right))


(use-package dashboard
  :ensure t
  :init
  (setq initial-buffer-choice (lambda () (switch-to-buffer "*dashboard*")))
  :config
  (setq dashboard-banner-logo-title "Do the work.")
  (setq dashboard-startup-banner nil)
  (dashboard-setup-startup-hook))

(use-package magit
  :ensure t
  :bind (:map magit-diff-mode-map
	      (("C-o" . magit-diff-visit-file-other-window)))
  :config
  (setq auto-revert-buffer-list-filter
	'magit-auto-revert-repository-buffers-p)
  (remove-hook 'magit-refs-sections-hook 'magit-insert-tags)
  ;; (setq vc-handled-backends '(Git))
  (setq magit-push-always-verify nil)
  (setq magit-refresh-status-buffer nil)

  (magit-define-section-jumper magit-jump-to-recent-commits "Recent commits" recent "HEAD~10..HEAD")
  (define-key magit-status-mode-map "jrc" 'magit-jump-to-recent-commits)

  (defun tj-semaphore-open-branch ()
    "Open branch in Semaphore CI"
    (interactive)
    (let* ((branch (magit-get-current-branch))
           (group
            (thread-first (magit-get "remote" "origin" "url")
              (split-string ":")
              last
              first
              (split-string "\\.")
              first))
           (group
            (if (string-prefix-p "confluentinc" group)
                (replace-regexp-in-string "confluentinc" "confluent" group)
              group)))
      (browse-url (format "https://semaphoreci.com/%s/branches/%s" group branch))))

  (eval-after-load 'magit
    '(define-key magit-mode-map "S"
       #'tj-semaphore-open-branch))


  ;; (remove-hook 'server-switch-hook 'magit-commit-diff)

  (defun tj-visit-pull-request-url ()
    "Visit the current branch's PR on Github."
    (interactive)
    (browse-url
     (format "https://github.com/%s/pull/new/%s"
	     (replace-regexp-in-string
	      "\\`.+github\\.com:\\(.+\\)\\.git\\'" "\\1"
	      (magit-get "remote"
			 (magit-get-push-remote)
			 "url"))
	     (magit-get-current-branch))))

  (eval-after-load 'magit
    '(define-key magit-mode-map "v"
       #'tj-visit-pull-request-url))


  (defun magit-key-mode--add-default-options (arguments)
    (if (eq (car arguments) 'pulling)
	(list 'pulling (list "--rebase"))
      arguments)

    (if (eq (car arguments) 'pushing)
	(list 'pushing (list "-u"))
      arguments)
    )

  (advice-add 'magit-key-mode :filter-args #'magit-key-mode--add-default-options)
  :ensure t
  :bind (("C-x g" . magit-status)))

(use-package forge
  :ensure t)

(use-package ivy-rich
  :ensure t
  :config  (setq ivy-virtual-abbreviate 'full
                 ivy-rich-switch-buffer-align-virtual-buffer t)
  (setq ivy-rich-path-style 'abbrev)
  (ivy-set-display-transformer 'ivy-switch-buffer 'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode))

(use-package copy-as-format
  :ensure t
  :bind ("M-s M-w" . copy-as-format)
  :init
  (setq copy-as-format-default "github"))

(use-package abbrev
  :diminish
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package compile
  :init
  (require 'grep)
  :no-require

  :bind (("C-c c" . compile)
         ("M-O"   . show-compilation))

  :bind (:map compilation-mode-map
              (("z" . delete-window)
               ("RET" . tj-compile-goto-error-same-window)
               ))
  :bind (:map compilation-minor-mode-map
              ("RET" . tj-compile-goto-error-same-window))
  :bind (:map compilation-button-map
              ("RET" . tj-compile-goto-error-same-window))
  :bind (:map grep-mode-map
              ("RET" . tj-compile-goto-error-same-window))

  :preface

  (defun tj-compile-goto-error-same-window ()
    (interactive)
    (let ((display-buffer-overriding-action
           '((display-buffer-reuse-window
              display-buffer-same-window)
             (inhibit-same-window . nil))))
      (call-interactively #'compile-goto-error)))

  (defun show-compilation ()
    (interactive)
    (let ((it
           (catch 'found
             (dolist (buf (buffer-list))
               (when (string-match "\\*compilation\\*" (buffer-name buf))
                 (throw 'found buf))))))
      (if it
          (display-buffer it)
        (call-interactively 'compile))))

  (defun compilation-ansi-color-process-output ()
    (ansi-color-process-output nil)
    (set (make-local-variable 'comint-last-output-start)
         (point-marker)))

  :hook ((compilation-filter . compilation-ansi-color-process-output)))

(use-package dired-toggle
  :ensure t
  :bind ("M-s d" . dired-toggle)
  :preface
  (defun my-dired-toggle-mode-hook ()
    (interactive)
    (setq-local visual-line-fringe-indicators '(nil right-curly-arrow))
    (setq-local word-wrap nil))
  :hook (dired-toggle-mode . my-dired-toggle-mode-hook))

(use-package yasnippet
  :config
  (yas-global-mode))

(use-package auto-yasnippet
  :ensure t
  :after yasnippet
  :bind (("C-c y a" . aya-create)
         ("C-c y e" . aya-expand)
         ("C-c y o" . aya-open-line)))


(use-package git-timemachine
  :ensure t
  :bind (("s-g" . git-timemachine)))

(use-package ag
  :ensure t)

(use-package smart-forward
  :ensure t
  :config
  :bind
  (("M-<up>" . smart-up)
   ("M-<down>" . smart-down)
   ("M-<left>" . smart-backward)
   ("M-<right>" . smart-forward)))

(use-package ibuffer-projectile
  :ensure t
  :config
  (add-hook 'ibuffer-hook
	    (lambda ()
	      (ibuffer-projectile-set-filter-groups)
	      (unless (eq ibuffer-sorting-mode 'alphabetic)
		(ibuffer-do-sort-by-alphabetic)))))

(use-package re-builder
  :bind (:map reb-mode-map
	      ("M-%" . reb-query-replace))
  :config
  (defun reb-query-replace (to-string)
    "Replace current RE from point with `query-replace-regexp'."
    (interactive
     (progn (barf-if-buffer-read-only)
	    (list (query-replace-read-to (reb-target-binding reb-regexp)
					 "Query replace"  t))))
    (with-current-buffer reb-target-buffer
      (query-replace-regexp (reb-target-binding reb-regexp) to-string))))



;; (use-package dockerfile-mode
;;   :ensure t
;;   :mode "Dockerfile[a-zA-Z.-]*\\'")

(use-package edit-indirect
  :ensure t
  :bind (("C-c '" . edit-indirect-region)))

(use-package hcl-mode :ensure t)

(use-package restclient
  :ensure t
  :mode
  ("\\.rest\\'" . restclient-mode)
  :config
  (defun tj-response-loaded-hook ()
    (flycheck-mode -1))
  (add-hook 'restclient-response-loaded-hook 'tj-response-loaded-hook)
  (defun tj-restclient-hook ()
    (setq-local indent-line-function 'js-indent-line))
  (add-hook 'restclient-mode-hook 'tj-restclient-hook))

(use-package osx-clipboard
  :ensure t
  :config
  (osx-clipboard-mode))

(use-package ediff
  :ensure t
  :bind (("M-s = b" . ediff-buffers)
	 ("M-s = B" . ediff-buffers3)
	 ("M-s = c" . compare-windows)
	 ("M-s = =" . ediff-files)
	 ("M-s = f" . ediff-files)
	 ("M-s = F" . ediff-files3)
	 ("M-s = r" . ediff-revision)
	 ("M-s = p" . ediff-patch-file)
	 ("M-s = P" . ediff-patch-buffer)
	 ("M-s = l" . ediff-regions-linewise)
	 ("M-s = w" . ediff-regions-wordwise))
  :init
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

(use-package git-link
  :ensure t
  :bind ("M-s G" . git-link)
  :commands (git-link git-link-commit git-link-homepage))

(use-package git-timemachine
  :ensure t
  :commands git-timemachine)

(use-package gitattributes-mode
  :ensure t
  :defer 5)

(use-package gitconfig-mode
  :ensure t
  :defer 5)

(use-package github-pullrequest
  :ensure t
  :commands (github-pullrequest-new
	     github-pullrequest-checkout))

(use-package gitignore-mode
  :ensure t
  :defer 5)

(use-package gitpatch
  :ensure t
  :commands gitpatch-mail)

(use-package google-this
  :ensure t
  :bind (("M-s /" . google-this-search)))

(use-package goto-last-change
  :ensure t
  :bind ("C-x C-/" . goto-last-change))

(use-package ialign
  :ensure t
  :bind ("M-s [" . ialign-interactive-align))

(use-package operate-on-number
  :ensure t
  :bind ("M-s '" . operate-on-number-at-point))

(use-package shift-number
  :ensure t
  :bind (("M-s +" . shift-number-up)
	 ("M-s -" . shift-number-down)))

(use-package diminish
  :ensure t
  :demand t)

(use-package ivy
  :ensure t
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq projectile-completion-system 'ivy)
  (bind-keys :map minibuffer-local-map
             ((kbd "C-r") #'counsel-minibuffer-history))
  :bind
  (("C-c C-r" . ivy-resume)
   ("M-R" . ivy-resume)
   ("C-x b" . ivy-switch-buffer)
   ("C-x B" . ivy-switch-buffer-other-window))
  ("M-x".  counsel-M-x)
  ;; ("C-x C-f" . counsel-find-file)
  ("<f1> f" . counsel-describe-function)
  ("<f1> v" . counsel-describe-variable)
  ("<f1> l" . counsel-find-library)
  ("<f2> i" . counsel-info-lookup-symbol)
  ("<f2> u" . counsel-unicode-char)
  ("C-c g" . counsel-git)
  ("C-c j" . counsel-git-grep)
  ("C-c a" . counsel-ag)
  ("C-x l" . counsel-locate))


(use-package swiper
  :ensure t
  :diminish
  :after ivy
  :bind (:map swiper-map
	      ("M-y" . yank)
	      ("M-%" . swiper-query-replace)
	      ("C-'" . isearch-forward-regexp)
	      ("M-h" . swiper-avy)
	      ("M-c" . swiper-mc))
  :commands swiper-from-isearch
  :init
  (bind-keys :map isearch-mode-map ("C-i" . swiper-from-isearch)))

(use-package company-tern
  :ensure t
   :defer t
  :init
  (with-eval-after-load 'company
    (add-to-list 'company-backends 'company-tern)))

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package web-mode
  :ensure t
  :config

  (add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))

  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-attr-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-closing t)
  (setq web-mode-enable-auto-expanding t)
  (setq web-mode-enable-auto-opening t)
  (setq web-mode-enable-auto-pairing t)
  (add-to-list 'web-mode-comment-formats '("jsx" . "//"))
  (add-to-list 'web-mode-comment-formats '("javascript" . "//"))
  (setq web-mode-tag-auto-close-style 2)
  (setq web-mode-content-types-alist
	'(("jsx" . "\\.js[x]?\\'")))
  (setq web-mode-engines-alist
	'(("reactjs" . "\\.js$")
          ("go" . "\\.html$")))
  (with-eval-after-load 'web-mode
    (add-to-list 'web-mode-indentation-params '("lineup-args" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-concats" . nil))
    (add-to-list 'web-mode-indentation-params '("lineup-calls" . nil)))
  (setq tj--javascript-common-imenu-regex-list
	'(("Controller" "[. \t]controller([ \t]*['\"]\\([^'\"]+\\)" 1)
	  ("Module" "[. \t]module( *['\"]\\([a-zA-Z0-9_.]+\\)['\"], *\\[" 1)
	  ("Function" "function[ \t]+\\([a-zA-Z0-9_$.]+\\)[ \t]*(" 1)
	  ("Class" "class[ \t]+\\([a-zA-Z_.]+\\)" 1)
	  ("Constant" "const[ \t]+\\([a-zA-Z_.]+\\)" 1)
	  ("Function" "^[ \t]*\\([a-zA-Z0-9_$.]+\\)[ \t]*=[ \t]*function[ \t]*(" 1)
	  ))
  (defun tj--js-imenu-make-index ()
    (save-excursion
      (imenu--generic-function tj--javascript-common-imenu-regex-list)))
  (defun tj-web-mode-hook nil
    (subword-mode)
    (tern-mode)

    (setq comment-start "//"
	  comment-end	"")
    (setq-local imenu-create-index-function 'tj-js-imenu-make-index))
  (add-hook 'web-mode-hook #'tj-web-mode-hook)


  (defun tj-tml-insert-open-and-close-tag ()
    "Generates an open and close HTML snippet using the current word."
    (interactive)
    (let ((inserting-new-tag nil))
      (if (looking-back "[-A-Za-z0-9:_]")
	  (progn (set-mark-command nil)
		 (while (looking-back "[-A-Za-z0-9:_]")
		   (backward-char)))
	(setq inserting-new-tag t)
	(set-mark-command nil)
	(insert "p")
	(exchange-point-and-mark))
      (let ((tag (buffer-substring (region-beginning) (region-end))))
	(delete-char (string-width tag))
	(cond ((string-match "\\`[bh]r\\'" tag)
	       (insert (concat "<" tag ">")))
	      ((string-match (concat "\\`\\(?:img\\|meta\\|link\\|"
				     "input\\|base\\|area\\|col\\|"
				     "frame\\|param\\)\\'")
			     tag)
	       (yas-expand-snippet (concat "<" tag " $1>$0")))
	      (t
	       (yas-expand-snippet
		(if inserting-new-tag
		    (concat "<${1:"
			    tag
			    "}>$0</${1:"
			    "$(and (string-match \"[-A-Za-z0-9:_]+\" yas-text) "
			    "(match-string 0 yas-text))}>")
		  (concat "<"
			  tag
			  "$1>$0</"
			  tag
			  ">"))))))))
  (defun tj-rb-insert-or-toggle-erb-tag ()
    "Insert an ERb tag if the point isn't currently in one, or toggle the type."
    (interactive)
    (let ((action))
      (if (looking-at "[\s\t\n]*<%")
	  (setq action 'insert)
	(save-excursion
	  (let ((regex (concat "\\`<%.*%>\\'")))
	    (while (or (not (region-active-p))
		       (not (or (and (= (point-min) (region-beginning))
				     (= (point-max) (region-end)))
				(string-match regex (buffer-substring-no-properties
						     (region-beginning)
						     (region-end))))))
	      (let ((expand-region-fast-keys-enabled))
		(er/expand-region 1)))
	    (let ((matched (buffer-substring-no-properties (region-beginning)
							   (region-end))))
	      (if (string-match regex matched)
		  (progn (goto-char (+ (if (< (point) (mark)) (point) (mark)) 2))
			 (cond ((looking-at "=")
				(delete-char 1))
			       ((looking-at "#")
				(delete-char 1)
				(insert "="))
			       (t
				(insert "#"))))
		(setq action 'insert))))))
      (if (eq action 'insert)
	  (progn (insert "<%=  %>")
		 (backward-char 3)))))
  :mode
  ("\\.hbs$" . web-mode)
  ("\\.eex$" . web-mode)
  ("\\.js$" . web-mode)

  :bind
  ("M-." . tern-find-definition)
  ("C-c >" . tj-rb-insert-or-toggle-erb-tag)
  ("C-c <" . tj-tml-insert-open-and-close-tag))

(use-package ag
  :ensure t
  :config
  (defun tj-ag-here (arg)
    (interactive "sSearch string: ")
    (ag-regexp arg default-directory))

  (global-set-key (kbd "C-c C-a") 'ag-regexp))

(use-package company-quickhelp
  :ensure t
  :defer t
  :init (add-hook 'global-company-mode-hook #'company-quickhelp-mode))

(eval-after-load 'company
  '(define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin))

(use-package eldoc
  :ensure t
  :diminish)

;; (use-package go-eldoc
;;   :ensure t
;;   :diminish
;;   :defer t
;;   :init
;;   (add-hook 'go-mode-hook 'go-eldoc-setup))

(use-package godoctor :ensure t)

(use-package go-mode
  :ensure t

  :init

  :bind
  (:map go-mode-map
        ("M-j" . comment-indent-new-line)
        ("<tab>" . company-indent-or-complete-common)
	("M-b" . subword-backward)
	("M-f" . subword-forward)
	("M-d" . subword-kill)
	("C-c C-r" . godoctor-rename)
	("C-c C-c" . godoc-at-point)
	("C-c C-t" . go-test-current-file)
	("C-c g" . godoc)
	;; ("C-c <C-m>" . tj-go-kill-doc)
        ("C-c C-d" . lsp-describe-thing-at-point)
	("M-." . lsp-find-definition)
        ("s-t" . counsel-projectile-find-file)
	("s-." . tj-lsp-find-definition-other-window))
  :config

  (defun tj-lsp-find-definition-other-window ()
    "Split window vertically and use LSP to find the definition of the thing at point."
    (interactive)
    (split-window-horizontally)
    (lsp-find-definition))

  (load "~/dev/src/github.com/stapelberg/expanderr/expanderr.el")
  (setq go-test-verbose t)
  (setq gofmt-command "goimports")
  (setenv "GOPATH" (expand-file-name (concat (getenv "HOME") "/dev")))
  (setenv "GOROOT" "/usr/local/go")
  ;; (setq company-go-show-annotation t)

  ;; (setq flycheck-go-megacheck-disabled-checkers '("staticcheck" "simple" "unused"))

  ;; (use-package company-go
  ;; :ensure t
  ;; :defer t)

  (add-hook 'before-save-hook 'gofmt-before-save nil t)

  (setq tab-width 8)

  (setq-local compilation-read-command nil)

  (defun tj-go-find-file ()
    "Find file under $GOROOT."
    (interactive)
    (find-file "/usr/local/go/src/"))

  (use-package go-add-tags :ensure t)

  (use-package go-errcheck
    :ensure t
    :config
    (defun tj-go-errcheck ()
      (interactive)
      (let ((default-directory (projectile-project-root)))
	(go-errcheck nil nil nil))))
  (add-hook 'before-save-hook 'gofmt-before-save)

  (defun tj-go-kill-doc ()
    "Kill the doc for the thing at point."
    (interactive)
    (let ((funcinfo (go-eldoc--get-funcinfo)))
      (if funcinfo
	  (go-eldoc--format-signature funcinfo)
	(let ((bounds (go-eldoc--bounds-of-go-symbol)))
	  (when bounds
	    (let ((curinfo (go-eldoc--get-cursor-info bounds)))
	      (when curinfo
		(kill-new (format "%s" curinfo))
		(message (format "killed: %s" curinfo)))))))))

  (defun tj-go-hook ()
    (setq imenu-generic-expression
        '(("type" "^[ \t]*type *\\([^ \t\n\r\f]*[ \t]*\\(struct\\|interface\\)\\)" 1)
          ("func" "^func *\\(.*\\)" 1)))
    (which-function-mode)
    (highlight-symbol-mode)
    (subword-mode)
    (flycheck-mode)
    (electric-indent-mode)
    (electric-pair-mode)
    (selected-minor-mode 1)
    (font-lock-mode -1)
    ;; (setq company-backends '(company-go))
    (go-guru-hl-identifier-mode)
    (if (not (string-match "go" compile-command))
	(set (make-local-variable 'compile-command)
	     "go build -v && go test -v && go vet")))
  (add-hook 'go-mode-hook 'tj-go-hook)

  (use-package go-guru
    :ensure t)

  (use-package gotest :ensure t)

  :hook
  (go-mode . tj-go-hook))


(use-package selected :ensure t)

(use-package winner
  :diminish
  :config
  (winner-mode +1)
  :bind
  (("M-[" . winner-undo)
   ("M-]" . winner-redo)))

(use-package eacl
  :ensure t
  :config
  (setq eacl-grep-program "grep --exclude-dir=.git --exclude-dir=vendor")
  :bind
  (("C-x C-l" . eacl-complete-line)))

(use-package go-gen-test
  :ensure t
  :after go-mode)

(use-package embrace
  :ensure t
  :config
  (setq	 embrace-show-help-p nil)
  :bind
  (("M-s M-e" . embrace-commander)))

(use-package iy-go-to-char
  :ensure t
  :bind
  (("M-s M-j" . iy-go-up-to-char)
   ("M-s M-J" . iy-go-to-char-backward)
   ("M-s M-s" . iy-go-to-or-up-to-continue)
   ("M-s M-S" . iy-go-to-or-up-to-continue-backward)))

(use-package bm
  :ensure t
  :bind (("M-s b" . bm-toggle)
	 ("M-s ." . bm-next)
	 ("M-s ," . bm-previous))
  :commands (bm-repository-load
	     bm-buffer-save
	     bm-buffer-save-all
	     bm-buffer-restore)
  :init
  (add-hook' after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
				 (bm-buffer-save-all)
				 (bm-repository-save))))
(use-package projectile
  :ensure t
  :config

  (setq projectile-indexing-method 'alien)
  (setq projectile-mode-line nil)
   (setq projectile-sort-order 'modification-time)
  ;; (setq projectile-sort-order 'default)

  ;; Fix the issue where the older project name is prepended to 'Find File:'
  ;; prompt after `projectile-switch-project'
  ;; https://github.com/bbatsov/projectile/issues/1067#issuecomment-270656085
  ;; https://github.com/bbatsov/projectile/issues/1067#issuecomment-270686996
  ;; (defun projectile-project-name-old ()
  ;;   "Return project name."
  ;;   (if projectile-project-name
  ;;       projectile-project-name
  ;;     (let ((project-root
  ;;            (condition-case nil
  ;;       	 (projectile-project-root)
  ;;              (error nil))))
  ;;       (if project-root
  ;;           (funcall projectile-project-name-function project-root)
  ;;         "-"))))
  ;; (advice-add 'projectile-project-name :override #'projectile-project-name-old)

  (setq projectile-switch-project-action #'projectile-commander)
  (add-to-list 'projectile-globally-ignored-directories "Godeps/_workspace")
  (add-to-list 'projectile-globally-ignored-directories "vendor")
  ;; (add-to-list 'projectile-globally-ignored-directories "_build")
  (add-to-list 'projectile-globally-ignored-directories "deps")
  (add-to-list 'projectile-globally-ignored-directories "node_modules")

  ;;  (eval-after-load "projectile"
  ;;	'(progn (setq magit-repo-dirs (mapcar (lambda (dir)
  ;;					    (substring dir 0 -1))
  ;;					  (remove-if-not (lambda (project)
  ;;							   (file-directory-p (concat project "/.git/")))
  ;;							 (projectile-relevant-known-projects))))
  ;;	    (setq magit-repo-dirs-depth 1)))

  (def-projectile-commander-method ?e "Open eshell in root directory." (call-interactively 'projectile-run-eshell))
  (def-projectile-commander-method ?! "Run shell command in root directory." (call-interactively 'projectile-run-async-shell-command-in-root))
  (def-projectile-commander-method ?m "Run shell command in root directory." (call-interactively 'projectile-run-async-shell-command-in-root))
  (def-projectile-commander-method ?a "Run ag in the project." (let ((current-prefix-arg 1)) (call-interactively 'projectile-ag)))
  (def-projectile-commander-method ?A "Run ack in the project." (let ((current-prefix-arg 1)) (call-interactively 'ack)))
  (def-projectile-commander-method ?c "Compile project." (call-interactively 'projectile-compile-project))
  (def-projectile-commander-method ?d "Open project root in dired." (call-interactively 'projectile-dired))
  (def-projectile-commander-method ?t "Test project." (call-interactively 'projectile-test-project))
  (def-projectile-commander-method ?G "Open file in git." (call-interactively 'github-browse-file))

  (def-projectile-commander-method ?d "Open project root in dired." (call-interactively 'projectile-dired))
  (def-projectile-commander-method ?u
    "Git fetch."
    (magit-status)
    (if (fboundp 'magit-fetch-from-upstream)
	(call-interactively #'magit-fetch-from-upstream)
      (call-interactively #'magit-fetch-current)))
  (def-projectile-commander-method ?p
    "Git push."
    (magit-status)
    (if (fboundp 'magit-push-current-to-upstream)
	(call-interactively #'magit-push-current-to-upstream)
      (call-interactively #'magit-push-current)))

  :bind
  (("C-c t" . projectile-toggle-between-implementation-and-test)
   ("C-c p p" . projectile-switch-project)
   ("C-c C-p" . projectile-test-project)
   ("M-m" . projectile-commander)
   ("C-c P" . 'projectile-switch-project)))

(global-set-key (kbd "C-c c") #'embrace-commander)

(use-package web-beautify :ensure t)

(use-package whitespace
  :diminish
  :config
  (setq whitespace-line-column nil))

(use-package projectile
  :ensure t
  :init
  ;; (setq projectile-completion-system 'ivy)
  :config
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (projectile-global-mode +1))

(use-package pt
  :ensure t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . er/expand-region))

(use-package elisp-slime-nav
  :ensure t
  :diminish
  :config
  (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
    (add-hook hook #'elisp-slime-nav-mode)))

(use-package paredit
  :ensure t
  :init
  (add-hook 'lisp-interaction-mode-hook #'paredit-mode)
  (add-hook 'emacs-lisp-mode-hook #'paredit-mode)
  (add-hook 'ielm-mode-hook #'paredit-mode)
  (add-hook 'lisp-mode-hook #'paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'paredit-mode)
  :diminish
  :bind (:map paredit-mode-map
              ("M-;" . nil)
              ("M-r" . nil)
              ("M-I" . paredit-splice-sexp)))

(use-package smex :ensure t)

(use-package paren
  :config
  (show-paren-mode +1))

(use-package abbrev
  :config
  (setq save-abbrevs 'silently)
  (setq-default abbrev-mode t))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package tj)

;; saveplace remembers your location in a file when saving files
(require 'saveplace)
(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" savefile-dir))
  ;; activate it for all buffers
  (setq-default save-place t))

(use-package savehist
  :config
  (setq savehist-additional-variables
	;; search entries
	'(search-ring regexp-search-ring)
	;; save every minute
	savehist-autosave-interval 60
	;; keep the home clean
	savehist-file (expand-file-name "savehist" savefile-dir))
  (savehist-mode +1))

;; (use-package recentf
;;   :config
;;   (setq recentf-save-file (expand-file-name "recentf" savefile-dir)
;; 	recentf-max-saved-items 500
;; 	recentf-max-menu-items 15
;; 	;; disable recentf-cleanup on Emacs start, because it can cause
;; 	;; problems with remote files
;; 	recentf-auto-cleanup 'never)
;;   (recentf-mode +1))

(use-package windmove
  :config
  ;; use shift + arrow keys to switch between visible buffers
  (windmove-default-keybindings))

(use-package highlight-symbol
  :ensure t
  :diminish
  :config
  (highlight-symbol-mode)
  :bind
  (("M-p" . highlight-symbol-prev)
   ("M-n" . highlight-symbol-next)))

(use-package diffview
  :commands (diffview-current diffview-region diffview-message))

(use-package f :ensure t)



(use-package dired
  :bind
  (("C-c J" . dired-double-jump)
   ("C-x d" . dired-jump))
  :bind (:map dired-mode-map
	      ("z"     . delete-window)
	      ("e"     . ora-ediff-files)
	      ("l"     . dired-up-directory)
	      ("Y"     . ora-dired-rsync)
	      ("<tab>" . tj-dired-switch-window)
	      ("M-!"   . async-shell-command)
	      ("M-G"))
  :preface
  (defvar mark-files-cache (make-hash-table :test #'equal))

  (defun mark-similar-versions (name)
    (let ((pat name))
      (if (string-match "^\\(.+?\\)-[0-9._-]+$" pat)
	  (setq pat (match-string 1 pat)))
      (or (gethash pat mark-files-cache)
	  (ignore (puthash pat t mark-files-cache)))))

  (defun dired-mark-similar-version ()
    (interactive)
    (setq mark-files-cache (make-hash-table :test #'equal))
    (dired-mark-sexp '(mark-similar-versions name)))

  (defun dired-double-jump (first-dir second-dir)
    (interactive
     (list (read-directory-name "First directory: "
				(expand-file-name "~")
				nil nil "dl/")
	   (read-directory-name "Second directory: "
				(expand-file-name "~")
				nil nil "Archives/")))
    (dired first-dir)
    (dired-other-window second-dir))

  (defun tj-dired-switch-window ()
    (interactive)
    (if (eq major-mode 'sr-mode)
	(call-interactively #'sr-change-window)
      (call-interactively #'other-window)))

  (defun ora-dired-rsync (dest)
    (interactive
     (list
      (expand-file-name
       (read-file-name "Rsync to: " (dired-dwim-target-directory)))))
    (let ((files (dired-get-marked-files
		  nil current-prefix-arg))
	  (tmtxt/rsync-command
	   "rsync -arvz --progress "))
      (dolist (file files)
	(setq tmtxt/rsync-command
	      (concat tmtxt/rsync-command
		      (shell-quote-argument file)
		      " ")))
      (setq tmtxt/rsync-command
	    (concat tmtxt/rsync-command
		    (shell-quote-argument dest)))
      (async-shell-command tmtxt/rsync-command "*rsync*")
      (other-window 1)))

  (defun ora-ediff-files ()
    (interactive)
    (let ((files (dired-get-marked-files))
	  (wnd (current-window-configuration)))
      (if (<= (length files) 2)
	  (let ((file1 (car files))
		(file2 (if (cdr files)
			   (cadr files)
			 (read-file-name
			  "file: "
			  (dired-dwim-target-directory)))))
	    (if (file-newer-than-file-p file1 file2)
		(ediff-files file2 file1)
	      (ediff-files file1 file2))
	    (add-hook 'ediff-after-quit-hook-internal
		      `(lambda ()
			 (setq ediff-after-quit-hook-internal nil)
			 (set-window-configuration ,wnd))))
	(error "no more than 2 files should be marked"))))

  :config
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


  ;; dired - reuse current buffer by pressing 'a'
  (put 'dired-find-alternate-file 'disabled nil)

  ;; always delete and copy recursively
  (setq dired-recursive-deletes 'always)
  (setq dired-recursive-copies 'always)

  ;; if there is a dired buffer displayed in the next window, use its
  ;; current subdir, instead of the current subdir of this dired buffer
  (setq dired-dwim-target t)

  ;; enable some really cool extensions like C-x C-j(dired-jump)
  (require 'dired-x)

  (ignore-errors
    (unbind-key "M-s f" dired-mode-map))

  (defadvice dired-omit-startup (after diminish-dired-omit activate)
    "Make sure to remove \"Omit\" from the modeline."
    (diminish 'dired-omit-mode) dired-mode-map)

  (defadvice dired-next-line (around dired-next-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and	 (not  (eobp)) (not ad-return-value))
      (forward-line)
      (setq ad-return-value(dired-move-to-filename)))
    (when (eobp)
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename))))

  (defadvice dired-previous-line (around dired-previous-line+ activate)
    "Replace current buffer if file is a directory."
    ad-do-it
    (while (and	 (not  (bobp)) (not ad-return-value))
      (forward-line -1)
      (setq ad-return-value(dired-move-to-filename)))
    (when (bobp)
      (call-interactively 'dired-next-line)))

  (defvar dired-omit-regexp-orig (symbol-function 'dired-omit-regexp))

  ;; Omit files that Git would ignore
  (defun dired-omit-regexp ()
    (let ((file (expand-file-name ".git"))
	  parent-dir)
      (while (and (not (file-exists-p file))
		  (progn
		    (setq parent-dir
			  (file-name-directory
			   (directory-file-name
			    (file-name-directory file))))
		    ;; Give up if we are already at the root dir.
		    (not (string= (file-name-directory file)
				  parent-dir))))
	;; Move up to the parent dir and try again.
	(setq file (expand-file-name ".git" parent-dir)))
      ;; If we found a change log in a parent, use that.
      (if (file-exists-p file)
	  (let ((regexp (funcall dired-omit-regexp-orig))
		(omitted-files
		 (shell-command-to-string "git clean -d -x -n")))
	    (if (= 0 (length omitted-files))
		regexp
	      (concat
	       regexp
	       (if (> (length regexp) 0)
		   "\\|" "")
	       "\\("
	       (mapconcat
		#'(lambda (str)
		    (concat
		     "^"
		     (regexp-quote
		      (substring str 13
				 (if (= ?/ (aref str (1- (length str))))
				     (1- (length str))
				   nil)))
		     "$"))
		(split-string omitted-files "\n" t)
		"\\|")
	       "\\)")))
	(funcall dired-omit-regexp-orig)))))

(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
	      ("/" . dired-narrow)))

(use-package dired-ranger
  :bind (:map dired-mode-map
	      ("W" . dired-ranger-copy)
	      ("X" . dired-ranger-move)
	      ("Y" . dired-ranger-paste)))

(use-package docker
  :defer 15
  :diminish
  :config
  (require 'docker-images)
  (require 'docker-containers)
  (require 'docker-volumes)
  (require 'docker-networks))

(use-package sh-mode
  :init
  (setq sh-basic-offset 2)
  (setq sh-basic-indentation 2)
  :mode ("\\.bats$" . sh-mode))

(use-package anzu
  :ensure t
  :diminish
  :bind (("M-%" . anzu-query-replace-regexp)
	 ("C-M-%" . anzu-query-replace))
  :hook
  (prog-mode . anzu-mode))

(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package exec-path-from-shell
  :ensure t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize))

  (when (memq window-system '(mac ns))
    (set-frame-font "Hack 14" nil t)))

(use-package move-text
  :ensure t
  :bind
  (([(meta shift up)] . move-text-up)
   ([(meta shift down)] . move-text-down)))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :config
  (setq whitespace-line-column 80) ;; limit line length
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

(use-package markdown-mode
  :mode
  ("\\.markdown$" . markdown-mode)
  ("\\.md$" . markdown-mode)
  :config
  (defun tj-wrap-with-tags ()
    (interactive)
    (wrap-region-with-tag))

  (defun tj-insert-author-tag ()
    (interactive)
    (insert "<author></author>")
                    (backward-char 9))
  :bind
  (("C-c <C-m>" . tj-insert-author-tag)
   ("C-c C-w" . tj-wrap-with-tags)
   :map markdown-mode-map
   ("C-c <" . tj-tml-insert-open-and-close-tag))
  :ensure t)

(use-package yaml-mode
  :mode
  ("\\.yaml" . yaml-mode)
  :ensure t)

(use-package org
  :bind
  (("C-c o" . tj-org-capture)
   ("M-s c" . org-capture))
  :hook (org-mode . font-lock-mode)
  :config

  (setq org-todo-keywords
        (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
                (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
  (setq org-agenda-files (split-string (shell-command-to-string "find ~/Dropbox/org/*")))
  (setq org-archive-location (expand-file-name "~/Dropbox/org/archive.org::* Archived Tasks"))
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-use-fast-todo-selection t)
  (setq org-src-lang-modes '(
                             ("screen" . sh)
                             ("ocaml" . tuareg)
                             ("elisp" . emacs-lisp)
                             ("lisp" . lisp)
                             ("ditaa" . artist)
                             ("asymptote" . asy)
                             ("cl" . lisp)
                             ("dot" . graphviz-dot)))

  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min)))

  (setq org-capture-templates
        (quote (("t" "Todo" entry (file "~/Dropbox/org/capture.org")
                 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
                ("n" "Note" entry (file "~/Dropbox/org/capture.org")
                 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
                ("j" "Journal" entry (function org-journal-find-location)
                 "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
                ("w" "org-protocol" entry (file "~/git/org/refile.org")
                 "* TODO Review %c\n%U\n" :immediate-finish t))
               ))

  (setq org-refile-targets (quote ((nil :maxlevel . 9)
                                   (org-agenda-files :maxlevel . 9))))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-compact-blocks t)

  (setq org-agenda-custom-commands
        (quote (("d" todo nil)
	        ("c" todo "DONE|DEFERRED|CANCELLED" nil)
	        ("w" todo "WAITING" nil)
	        ("W" agenda "" ((org-agenda-ndays 21)))
	        ("A" agenda ""
	         ((org-agenda-skip-function
	           (lambda nil
		     (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
	          (org-agenda-ndays 1)
	          (org-agenda-overriding-header "Today's Priority #A tasks: ")))
	        ("u" alltodo ""
	         ((org-agenda-skip-function
	           (lambda nil
		     (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
					       (quote regexp) "\n]+>")))
	          (org-agenda-overriding-header "Unscheduled TODO entries: "))))))


  (setq org-default-notes-file (expand-file-name "~/Dropbox/org/capture.org"))

  (setq org-startup-folded nil)

  (defun tj-org-capture ()
    (interactive)
    (find-file org-default-notes-file))

  (defun tj-org-archive-done-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/DONE" 'tree))

  (require 'org-table)

  (defun tj-org-replace-link-by-link-description ()
    "Replace an org link by its description or if empty its address"
    (interactive)
    (if (org-in-regexp org-bracket-link-regexp 1)
        (save-excursion
          (let ((remove (list (match-beginning 0) (match-end 0)))
                (description (if (match-end 3)
                                 (org-match-string-no-properties 3)
                               (org-match-string-no-properties 1))))
            (apply 'delete-region remove)
            (insert description)))))

  (setq org-src-tab-acts-natively t)
  (setq org-default-notes-file (expand-file-name "~/Dropbox/org/capture.org"))
  (setq org-directory (expand-file-name "~/Dropbox/org"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-use-speed-commands t)


  (setq org-todo-keywords
	(quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
		(sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)"))))
  (setq org-agenda-files (split-string (shell-command-to-string "find ~/Dropbox/org/*")))
  (setq org-archive-location (expand-file-name "~/Dropbox/org/archive.org::* Archived Tasks"))
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)
  (setq org-use-fast-todo-selection t)
  (setq org-src-lang-modes '(
			     ("screen" . sh)
			     ("ocaml" . tuareg)
			     ("elisp" . emacs-lisp)
			     ("lisp" . lisp)
			     ("ditaa" . artist)
			     ("asymptote" . asy)
			     ("cl" . lisp)
			     ("dot" . graphviz-dot)))

  (defun org-journal-find-location ()
    ;; Open today's journal, but specify a non-nil prefix argument in order to
    ;; inhibit inserting the heading; org-capture will insert the heading.
    (org-journal-new-entry t)
    ;; Position point on the journal's top-level heading so that org-capture
    ;; will add the new entry as a child entry.
    (goto-char (point-min)))

  (setq org-capture-templates
	(quote (("t" "Todo" entry (file "~/Dropbox/org/capture.org")
		 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
		("n" "Note" entry (file "~/Dropbox/org/capture.org")
		 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
		("j" "Journal" entry (function org-journal-find-location)
		 "* %(format-time-string org-journal-time-format)%^{Title}\n%i%?")
		("w" "org-protocol" entry (file "~/git/org/refile.org")
		 "* TODO Review %c\n%U\n" :immediate-finish t))
	       ))

  (setq org-refile-targets (quote ((nil :maxlevel . 9)
				   (org-agenda-files :maxlevel . 9))))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)
  (setq org-refile-allow-creating-parent-nodes (quote confirm))

  (setq org-agenda-dim-blocked-tasks nil)
  (setq org-agenda-compact-blocks t)

  (setq org-agenda-custom-commands
	(quote (("d" todo nil)
		("c" todo "DONE|DEFERRED|CANCELLED" nil)
		("w" todo "WAITING" nil)
		("W" agenda "" ((org-agenda-ndays 21)))
		("A" agenda ""
		 ((org-agenda-skip-function
		   (lambda nil
		     (org-agenda-skip-entry-if (quote notregexp) "\\=.*\\[#A\\]")))
		  (org-agenda-ndays 1)
		  (org-agenda-overriding-header "Today's Priority #A tasks: ")))
		("u" alltodo ""
		 ((org-agenda-skip-function
		   (lambda nil
		     (org-agenda-skip-entry-if (quote scheduled) (quote deadline)
					       (quote regexp) "\n]+>")))
		  (org-agenda-overriding-header "Unscheduled TODO entries: "))))))

  :config
  (defun tj-org-archive-done-tasks ()
    (interactive)
    (org-map-entries
     (lambda ()
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading)))
     "/DONE" 'tree))

  (require 'org-table)
  :bind
  ("M-s c" . org-capture)
  ("M-s t" . org-todo-list)
  ("M-s m" . org-tags-view))

(use-package org-journal
  :ensure t
  :defer t
  :config
  (setq org-journal-dir "~/Dropbox/journal"))

(use-package org-web-tools
  :ensure t
  :defer t)

(use-package helpful
  :ensure t
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)))

(use-package alert
  :ensure t
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

(use-package ace-link
  :ensure t
  :defer 10
  :config
  (ace-link-setup-default)

  (add-hook 'org-mode-hook
	    #'(lambda () (bind-key "C-c C-o" #'ace-link-org org-mode-map)))
  (add-hook 'gnus-summary-mode-hook
	    #'(lambda () (bind-key "M-o" #'ace-link-gnus gnus-summary-mode-map)))
  (add-hook 'gnus-article-mode-hook
	    #'(lambda () (bind-key "M-o" #'ace-link-gnus gnus-article-mode-map)))
  (add-hook 'ert-results-mode-hook
	    #'(lambda () (bind-key "o" #'ace-link-help ert-results-mode-map)))

  (bind-key "C-c M-o" 'ace-link-addr))

(use-package ace-mc
  :ensure t
  :bind (("<C-m> h"   . ace-mc-add-multiple-cursors)
	 ("<C-m> M-h" . ace-mc-add-single-cursor)))

(use-package multiple-cursors
  :ensure t
  :defer 5
  :after selected
  :preface

  (defun reactivate-mark ()
    (interactive)
    (activate-mark))

  :config
  (add-to-list 'mc/cursor-specific-vars 'iy-go-to-char-start-pos)

  :bind (
         ("<C-m> <C-m>" . mc/edit-lines)
         ("<C-m> C-SPC" . mc/mark-pop)
         ("<C-m> C-e" . mc/edit-ends-of-lines)
         ("<C-m> C-a" . mc/edit-beginnings-of-lines)
         ("<C-m> a" . mc/mark-all-dwim)
         ("<C-m> C-x" . reactivate-mark)
         ("<C-m> C-SPC" . mc/mark-pop)
         ("<C-m> <" . mc/mark-previous-like-this)
         ("<C-m> >" . mc/mark-next-like-this)
         ;; Extra multiple cursors stuff
         ("<C-m> %" . mc/insert-numbers)
         ("C-S-<mouse-1>" . mc/add-cursor-on-click))

  :bind (:map selected-keymap
              ("c"   . mc/edit-lines)
              ("."   . mc/mark-next-like-this)
              ("<"   . mc/unmark-next-like-this)
              ("C->" . mc/skip-to-next-like-this)
              (","   . mc/mark-previous-like-this)
              (">"   . mc/unmark-previous-like-this)
              ("C-<" . mc/skip-to-previous-like-this)
              ("y"   . mc/mark-next-symbol-like-this)
              ("Y"   . mc/mark-previous-symbol-like-this)
              ("w"   . mc/mark-next-word-like-this)
              ("W"   . mc/mark-previous-word-like-this)))

(use-package mc-extras
  :ensure t
  :after multiple-cursors
  :bind (("<C-m> M-C-f" . mc/mark-next-sexps)
         ("<C-m> M-C-b" . mc/mark-previous-sexps)
         ("<C-m> C-d"   . mc/remove-current-cursor)
         ("<C-m> C-k"   . mc/remove-cursors-at-eol)
         ("<C-m> M-d"   . mc/remove-duplicated-cursors)
         ("<C-m> |"     . mc/move-to-column)
         ("<C-m> ~"     . mc/compare-chars)))

(use-package phi-search
  :ensure t
  :bind (:map mc/keymap
              ("C-r" . phi-search-backward)
              ("C-s" . phi-search)))



(use-package ace-jump-mode
  :ensure t
  :defer t)

(use-package browse-url
  :bind
  (("C-c x" . browse-url-at-point)))

(use-package deft
  :ensure t
  :commands deft
  :bind
  (("C-x D" . deft))
  :config
  (setq deft-directory "~/Dropbox/org")
  (setq deft-extensions '("org" "md"))
  (setq deft-default-extension "org")
  (setq deft-text-mode 'org-mode)
  (setq deft-use-filename-as-title t)
  (setq deft-use-filter-string-for-filename t)
  (setq deft-auto-save-interval 0))

(defun isearch-initial-string nil)

(defun isearch-set-initial-string ()
  (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
  (setq isearch-string isearch-initial-string)
  (isearch-search-and-update))

(defun isearch-foward-at-point (&optional regexp-p no-recursive-edit)
  (interactive)
  (if regexp-p (isearch-forward regexp-p no-recursive-edit)
    (let* ((end (progn (skip-syntax-foward "w_") (point)))
	   (begin (progn (skip-syntax-backward "w_") (point))))
      (if (eq begin end)
	  (isearch-forward regexp-p no-recursive-edit)
	(setq isearch-initial-string (buffer-substring begin end))
	(add-hook 'isearch-mode-hook 'isearch-set-initial-string)
	(isearch-forward regexp-p no-recursive-edit)))))

(defun eshell-execute-current-line ()
  "Insert current line at the end of the buffer."
  (interactive)
  (let ((command (buffer-substring
                  (save-excursion
                    (beginning-of-line)
                    (point))
                  (save-excursion
                    (end-of-line)
                    (point)))))
    (eshell--execute-command command t)))

(defun eshell-insert-command (text &optional func)
  "Insert a command at the end of the buffer."
  (interactive)
  (goto-char eshell-last-output-end)
  (insert-and-inherit text)
  (funcall (or func 'eshell-send-input)))

(defun eshell--execute-command (command save-excursion?)
  (let ((body #'(lambda nil
                  (eshell-insert-command command))))
    (if save-excursion?
        (save-excursion
          (funcall body))
      (funcall body))))


(defun eshell/goto (&optional repo)
  "cd to `repo', cloning if necessary."
  (interactive)
  (let ((segment-path (concat "~/dev/segmentio/" repo))
	(dev-path (concat "~/dev/" repo)))
    (if (file-exists-p segment-path)
	(eshell/cd segment-path)
      (if (file-exists-p dev-path)
	  (eshell/cd dev-path)))))

(defun eshell/clear ()
  "Clear eshell's buffer.'"
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

(add-to-list 'completion-styles 'initials t)
(add-to-list 'completion-styles 'subwords t)
(add-to-list 'completion-styles 'substring t)

(use-package company
  :ensure t
  :diminish
  :config

  ;; Ignore go test -c output files
  (add-to-list 'completion-ignored-extensions ".test")
  (setq company-ddabbrev-code-everywhere t)
  (setq company-dabbrev-code-modes t)
  (setq company-dabbrev-code-other-buffers 'all)
  (setq company-dabbrev-ignore-buffers "\\`\\'")
  (setq company-idle-delay 0.1)
  (setq company-echo-delay 0)
  (setq company-tooltip-align-annotations t)
  (setq company-tern-property-marker "")
  (setq company-minimum-prefix-length 5)
  (setq company-abort-manual-when-too-short 5)
  (setq company-dabbrev-downcase nil)
  (setq company-dabbrev-ignore-case t)
  (setq company-dabbrev-other-buffers 'all)
  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it)))

(use-package company-elisp
  :after company
  :config
  (push 'company-elisp company-backends))

(use-package cask-mode
  :ensure t)

(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode))

(use-package zop-to-char
  :ensure t
  :bind (("M-z" . zop-up-to-char)
	 ("M-Z" . zop-to-char)))

(use-package imenu-anywhere
  :ensure t
  :bind (("C-c i" . imenu-anywhere)
	 ("s-i" . imenu-anywhere)))

(use-package flyspell
  :config
  (when (eq system-type 'windows-nt)
    (add-to-list 'exec-path "C:/Program Files (x86)/Aspell/bin/"))
  (setq ispell-program-name "aspell" ; use aspell instead of ispell
	ispell-extra-args '("--sug-mode=ultra"))
  (add-hook 'text-mode-hook #'flyspell-mode)
  (add-hook 'prog-mode-hook #'flyspell-prog-mode))

(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package super-save
  :ensure t
  :diminish
  :config
  (super-save-mode +1))

(use-package crux
  :ensure t
  :bind (("C-c o" . crux-open-with)
	 ("M-o" . crux-smart-open-line)
         ("C-c d" . crux-duplicate-current-line-or-region)
	 ("C-c n" . crux-cleanup-buffer-or-region)
	 ("C-c f" . crux-recentf-find-file)
	 ("C-M-z" . crux-indent-defun)
	 ("C-c u" . crux-view-url)
	 ("C-c e" . crux-eval-and-replace)
	 ("C-c w" . crux-swap-windows)
	 ("C-c D" . crux-delete-file-and-buffer)
	 ("C-c r" . crux-rename-buffer-and-file)
	 ("C-c k" . crux-kill-other-buffers)
	 ("C-c TAB" . crux-indent-rigidly-and-copy-to-clipboard)
	 ("C-c I" . crux-find-user-init-file)
	 ("C-c S" . crux-find-shell-init-file)
	 ("s-r" . crux-recentf-find-file)
	 ("s-j" . crux-top-join-line)
	 ("C-^" . crux-top-join-line)
	 ("s-k" . crux-kill-whole-line)
	 ("C-<backspace>" . crux-kill-line-backwards)
	 ("s-o" . crux-smart-open-line-above)
	 ([remap move-beginning-of-line] . crux-move-beginning-of-line)
	 ([(shift return)] . crux-smart-open-line)
	 ([(control shift return)] . crux-smart-open-line-above)
	 ([remap kill-whole-line] . crux-kill-whole-line)
	 ("C-c s" . crux-ispell-word-then-abbrev)))

(use-package diff-hl
  :ensure t
  :config
  (global-diff-hl-mode +1)
  (add-hook 'dired-mode-hook 'diff-hl-dired-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package which-key
  :ensure t
  :diminish
  :config
  (which-key-mode +1))

;; backups

(setq
 backup-by-copying t
 delete-old-versions t
  kept-new-versions 10
  kept-old-versions 2
  vc-make-backup-files t
  version-control t)

(defun force-backup-of-buffer ()
  (setq buffer-backed-up nil))

(add-hook 'before-save-hook  'force-backup-of-buffer)

(use-package undo-propose
  :ensure t)

(use-package goto-chg
  :ensure t
  :bind
  (("C-." . goto-last-change)
   ("C-," . goto-last-change-reverse)))

(use-package color-moccur
  :ensure t
  :commands (isearch-moccur isearch-all isearch-moccur-all)
  :bind (("M-s O" . moccur)
	 :map isearch-mode-map
	 ("M-o" . isearch-moccur)
	 ("M-O" . isearch-moccur-all)))

(use-package isearch
  :config

  (setq isearch-lazy-highlight 'all-windows)
  (setq isearch-allow-scroll t)
  (setq lazy-highlight-cleanup t)

  (defun zap-to-isearch (rbeg rend)
    "Kill the region between the mark and the closest portion of
  the isearch match string. The behaviour is meant to be analogous
  to zap-to-char; let's call it zap-to-isearch. The deleted region
  does not include the isearch word. This is meant to be bound only
  in isearch mode.
  The point of this function is that oftentimes you want to delete
  some portion of text, one end of which happens to be an active
  isearch word. The observation to make is that if you use isearch
  a lot to move the cursor around (as you should, it is much more
  efficient than using the arrows), it happens a lot that you could
  just delete the active region between the mark and the point, not
  include the isearch word."
    (interactive "r")
    (when (not mark-active)
      (error "Mark is not active"))
    (let* ((isearch-bounds (list isearch-other-end (point)))
           (ismin (apply 'min isearch-bounds))
           (ismax (apply 'max isearch-bounds))
           )
      (if (< (mark) ismin)
          (kill-region (mark) ismin)
        (if (> (mark) ismax)
            (kill-region ismax (mark))
          (error "Internal error in isearch kill function.")))
      (isearch-exit)))

  :bind
  (("C-s" . isearch-forward-regexp)
   ("C-M-s" . isearch-forward)
   ("C-r" . isearch-backward-regexp)
   ("C-M-r" . isearch-backward))
  (:map isearch-mode-map
        ("M-z" . zap-to-isearch)))

(use-package moccur-edit
  :after color-moccur)

(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume))

(use-package ace-window
  :ensure t
  :diminish
  :bind* ("<s-return>" . ace-window))

(use-package swiper
  :ensure t)

(use-package counsel
  :diminish
  :ensure t
  :config

  (defun tj-counsel-ag ()
  (interactive)
  (counsel-ag nil (projectile-project-root)))
  (setq counsel-find-file-at-point t)
  :bind
  (("C-*"     . counsel-org-agenda-headlines)
   ("C-x C-f" . counsel-find-file)
   ("C-x C-g" . find-file-go)
   ("C-h f"   . counsel-describe-function)
   ("C-x r b" . counsel-bookmark)
   ("M-x"     . counsel-M-x)
   ("C-x <C-m>"     . counsel-M-x)
   ("M-s f"     . tj-counsel-ag)
   ("M-s a"     . tj-ag-regexp)
   ("C-c i" . counsel-imenu)
   ("M-y" . counsel-yank-pop)
   ("M-s j" . counsel-dired-jump)
   ("M-s n" . counsel-file-jump))
  :commands counsel-minibuffer-history
  :init
  :config

  (defun find-file-go (arg)
    (interactive "P")
    (let*
        ((pkg (or
               (and arg (read-string "PKG: "))
               (thing-at-point 'filename)))
         (dir (f-join (getenv "GOPATH") "src" pkg)))
      (projectile-find-file-in-directory dir)))

  (defun ag-go (arg)
    (interactive "P")
    (let*
        ((pkg (or
               (and arg (read-string "PKG: "))
               (thing-at-point 'filename)))
         (dir (f-join (getenv "GOPATH") "src" pkg))
         (search (read-string "Search string: ")))
      (ag search dir)))

  ;; (defun projectile-go-pkg (pkg)
  ;;   (interactive "sPKG: ")
  ;;   (let ((dir (f-join (getenv "GOPATH") "src" pkg)))
  ;;     (projectile-find-file-in-directory dir)))
  )

(use-package counsel-projectile
  :ensure t
  :config
  (setq counsel-projectile-remove-current-buffer t)
  (setq counsel-projectile-remove-current-project t)
  (setq counsel--find-file-matcher 'counsel--find-file-matcher)

  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-projectile-find-file . ivy--sort-files-by-date))

  (add-to-list 'ivy-sort-matches-functions-alist
               '(counsel-find-file . ivy--sort-files-by-date))

  :bind
  (("C-\\" . counsel-projectile-find-file)
   ("C-c p f" . counsel-projectile-find-file)))

(use-package github-browse-file
  :ensure t
  :bind
  (("M-s g" . github-browse-file)))

(use-package minibuffer
  :config
  (defun my-minibuffer-setup-hook ()
    (smartparens-mode -1)
    (electric-pair-mode -1)
    (subword-mode)
    (setq gc-cons-threshold most-positive-fixnum))

  (defun my-minibuffer-exit-hook ()
    (electric-pair-mode +1)
    (setq gc-cons-threshold 800000))

  (add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
  (add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook))

(use-package smartparens :ensure t)

(use-package eval-expr
  :ensure t
  :bind ("M-:" . eval-expr)
  :config
  (defun eval-expr-minibuffer-setup ()
    (local-set-key (kbd "<tab>") #'lisp-complete-symbol)
    (set-syntax-table emacs-lisp-mode-syntax-table)
    (paredit-mode)))

(use-package selected
  :diminish selected-minor-mode
  :bind (:map selected-keymap
              ("s-[" . align-code)
              ("s-f" . fill-region)
              ("s-U" . unfill-region)
              ("s-d" . downcase-region)
              ("s-u" . upcase-region)
              ("s-s" . sort-lines))
  :config
  (selected-global-mode 1))

;; temporarily highlight changes from yanking, etc
(use-package volatile-highlights
  :ensure t
  :diminish
  :config
  (volatile-highlights-mode +1))

(when (eq system-type 'windows-nt)
  (setq w32-pass-lwindow-to-system nil)
  (setq w32-lwindow-modifier 'super) ; Left Windows key

  (setq w32-pass-rwindow-to-system nil)
  (setq w32-rwindow-modifier 'super) ; Right Windows key

  (setq w32-pass-apps-to-system nil)
  (setq w32-apps-modifier 'hyper) ; Menu/App key

  (add-to-list 'exec-path "C:/Program Files/Git/bin")
  (add-to-list 'exec-path "C:/Program Files/Git/mingw64/bin")
  (setenv "PATH" (concat "C:/Program Files/Git/bin;" "C:/Program Files/Git/mingw64/bin;" (getenv "PATH")))
  ;; needed for arc-mode
  (add-to-list 'exec-path "C:/Program Files/7-Zip"))

;; config changes made through the customize UI will be stored here
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(defadvice split-window (after move-point-to-new-window activate)
  "Moves the point to the newly created window after splitting."
  (other-window 1))

;; Functions and bindings.


(setq comment-multi-line t)
(setq-default indent-tabs-mode nil)

(use-package eshell
  :commands (eshell eshell-command)
  :preface
  (defvar eshell-isearch-map
    (let ((map (copy-keymap isearch-mode-map)))
      (define-key map [(control ?m)] 'eshell-isearch-return)
      (define-key map [return]	     'eshell-isearch-return)
      (define-key map [(control ?r)] 'eshell-isearch-repeat-backward)
      (define-key map [(control ?s)] 'eshell-isearch-repeat-forward)
      (define-key map [(control ?g)] 'eshell-isearch-abort)
      (define-key map [backspace]    'eshell-isearch-delete-char)
      (define-key map [delete]	     'eshell-isearch-delete-char)
      map)
    "Keymap used in isearch in Eshell.")

  (defun tj-eshell-here ()
    (interactive)
    (eshell (f-dirname (buffer-file-name))))

  (defun eshell-initialize ()
    (defun eshell-spawn-external-command (beg end)
      "Parse and expand any history references in current input."
      (save-excursion
	(goto-char end)
	(when (looking-back "&!" beg)
	  (delete-region (match-beginning 0) (match-end 0))
	  (goto-char beg)
	  (insert "spawn "))))

    (add-hook 'eshell-expand-input-functions 'eshell-spawn-external-command)

    (use-package em-unix
      :defer t
      :config
      (unintern 'eshell/su nil)
      (unintern 'eshell/sudo nil)))

  :init
  (add-hook 'eshell-first-time-mode-hook 'eshell-initialize)
  (require 'em-smart)

  :config
  (defun tj-eshell()
    (interactive)
    (if (projectile-project-p)
	(call-interactively 'projectile-run-eshell)
      (eshell)))

  (setq eshell-where-to-jump 'begin)
  (setq eshell-review-quick-commands nil)
  (setq eshell-smart-space-goes-to-end t)

  (defun tj-eshell-mode-hook ()
    (define-key eshell-mode-map (kbd "M-r") 'counsel-esh-history)
    (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env)))

  (add-hook 'eshell-mode-hook 'tj-eshell-mode-hook)
  :bind
  (("C-x m" . tj-eshell)))


(use-package eshell-bookmark
  :ensure t
  :hook (eshell-mode . eshell-bookmark-setup))

(use-package eshell-up
  :ensure t
  :commands eshell-up)

(use-package eshell-z
  :ensure t
  :after eshell)

(use-package fancy-narrow
  :ensure t
  :bind (("M-s M-n" . fancy-narrow-to-region)
	 ("M-s M-N" . fancy-widen))
  :commands (fancy-narrow-to-region fancy-widen))

(use-package wgrep :ensure t)

(use-package string-edit :ensure t)

(use-package json-snatcher :ensure t)

(use-package wgrep-ag
  :ensure t
  :config (autoload 'wgrep-ag-setup "wgrep-ag")
  :hook (ag-mode-hook . wgrep-ag-setup))

(use-package visual-regexp
  :ensure t
  :bind
  ("M-&" . vr/query-replace)
  ("M-/" . vr/replace))

(use-package avy-zap
  :ensure t
  :bind
  (("M-Z" . avy-zap-up-to-char-dwim)))

(use-package backup-walker
  :ensure t
  :commands backup-walker-start)

;; (use-package centered-cursor-mode
;;   :ensure t
;;   :hook
;;   (prog-mode . centered-cursor-mode)
;;   (text-mode . centered-cursor-mode)
;;   (conf-mode . centered-cursor-mode))

(use-package change-inner
  :ensure t
  :bind (("M-i"	    . change-inner)
	 ("M-o" . change-outer)
	 ("s-i" . copy-inner)
	 ("s-o" . copy-outer)))

(use-package protobuf-mode
  :ensure t
  :mode "\\.proto\\'"
  :commands (protobuf-mode)
  :hook (protobuf-mode
	 . (lambda ()
	     (subword-mode)
	     (electric-pair-mode)
             (tj-protobuf-imenu-configure)
	     (c-add-style "tj-protobuf-style" tj-protobuf-style t)))
  :config

  (setq tj-protobuf-imenu-generic-expression
        '(("Message" "^message *\\([a-zA-Z0-9_]+\\)" 1)
          ("Service" "^service *\\([a-zA-Z0-9_]+\\)" 1)))

  (defun tj-protobuf-imenu-configure ()
    (interactive)
    (setq imenu-generic-expression tj-protobuf-imenu-generic-expression))

  (progn
    (defconst tj-protobuf-style
      '((c-basic-offset . 2)
	(indent-tabs-mode . nil)))))

(use-package js2-mode
  :ensure t
  :init
  (setq js-indent-level 2)
  (setq-default js2-global-externs '("module" "require" "buster" "sinon" "assert" "refute" "setTimeout" "clearTimeout" "setInterval" "clearInterval" "location" "__dirname" "console" "JSON"))
  (setq-default js2-strict-inconsistent-return-warning nil)

  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(javascript-jshint)))

  (setq-default flycheck-disabled-checkers
		(append flycheck-disabled-checkers
			'(json-jsonlist)))


  ;; :mode
  ;; ("\\.js$" . js2-jsx-mode)
  ;; ("\\.js\\'" . js2-jsx-mode)
  ;; ("\\.json\\'" . js2-jsx-mode)

  :interpreter ("node" . js2-jsx-mode)
  :bind
  ("M-j" . comment-indent-new-line)
  ("C-c C-j" . js2-jump-to-definition)
  ("M-." . tern-find-definition)
  :config
  (defun js2-match-async-arrow-function ()
    (when (and (js2-contextual-kwd-p (js2-current-token) "async")
	       (/= (js2-peek-token) js2-FUNCTION)
	       (/= (js2-peek-token) js2-DOT))
      (js2-record-face 'font-lock-keyword-face)
      (js2-get-token)
      t))
  (defun tj-js2-mode-hook ()
    (electric-indent-mode 1)
    (tern-mode)
    (flycheck-mode)
    (subword-mode))
  (add-hook 'js2-mode-hook 'tj-js2-mode-hook))

(use-package bm
  :bind (("C-c b b" . bm-toggle)
         ("C-c b n" . bm-next)
         ("C-c b l" . bm-show-all)
         ("C-c b p" . bm-previous))
  :commands (bm-repository-load
             bm-buffer-save
             bm-buffer-save-all
             bm-buffer-restore)
  :ensure t
  :init
  (add-hook 'after-init-hook 'bm-repository-load)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'after-revert-hook #'bm-buffer-restore)
  (add-hook 'kill-buffer-hook #'bm-buffer-save)
  (add-hook 'after-save-hook #'bm-buffer-save)
  (add-hook 'vc-before-checkin-hook #'bm-buffer-save)
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save))))

(use-package magit-todos
  :ensure t
  :config
  (magit-todos-mode))


(use-package terraform-mode
  :ensure t
  :config
  (add-hook 'terraform-mode-hook 'terraform-format-on-save-mode))

(use-package multifiles
  :ensure t
  :bind
  ("C-!" . mf/mirror-region-in-multifile))

(use-package toggle-quotes
  :ensure t
  :bind
  ( "C-\"" . toggle-quotes))

(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

(use-package highlight-indentation
  :ensure t
  :config
  (set-face-background 'highlight-indentation-face "#e3e3d3")
  (set-face-background 'highlight-indentation-current-column-face "#c3b3b3")
  (add-hook 'yaml-mode-hook 'highlight-indentation-mode)
  (add-hook 'yaml-mode-hook 'highlight-indentation-current-column-mode))

(use-package indent-tools
  :ensure t
  :config
  (add-hook 'yaml-mode-hook 'indent-tools-minor-mode))

(use-package plain-theme
  :ensure t
  :config
  (load-theme 'plain t))

(use-package gist
  :ensure t)

(require 'resmacro)

(global-set-key (kbd "C-x (") 'resmacro-start-macro)

(require 'titlecase)

(use-package unfill
  :ensure t
  :bind
  (("M-Q" . unfill-paragraph)))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR.

  \(fn arg char)"
  'interactive)

(use-package lsp-mode
  :ensure t
  :hook
  (prog-mode . lsp)
  :init
  (setq lsp-auto-guess-root t)
  :config
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "gopls")
                    :major-modes '(go-mode)
                    :server-id 'gopls)))

(use-package lsp-ui
  :ensure t
  :init
  (setq lsp-ui-doc-enable nil
	lsp-ui-doc-include-signature t
	lsp-ui-doc-position 'at-point
	lsp-ui-sideline-enable nil
	lsp-ui-sideline-ignore-duplicate t)
  )

(use-package company-lsp
  :ensure t
  :config
  (add-to-list 'company-backends 'company-lsp))

(use-package vdiff
  :ensure t)

(use-package vdiff-magit
  :ensure t)

(use-package dot-mode
  :ensure t
  :config
  (setq dot-mode-global-mode t)
  (dot-mode))

(use-package iedit
  :ensure t)

(use-package sqlformat
  :ensure t
  :hook
  (sql-mode . sqlformat-on-save-mode))

(use-package github-review
  :ensure t)

(use-package server
  :no-require
  :hook (after-init . server-start))
