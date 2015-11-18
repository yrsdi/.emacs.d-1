(require 'f)
(require 'eshell)

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

(defun bundler--installed? ()
  "`t' if the bundle is available in the working directory."
  (equal (call-process-shell-command "which bundle > /dev/null 2>&1") 0))

(defun bundler--in-bundled-project? ()
  "`t' if the project is bundled."
  (f-traverse-upwards
   (lambda (path)
     (f-exists? (f-join path "Gemfile")))
   default-directory))

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

(defun eshell/run-with-bundler (&rest args)
  "Run the `ARGS' with bundler when in a bundled project."
  (let ((cmd (eshell-flatten-and-stringify args)))
    (if (and (bundler--installed?) (bundler--in-bundled-project?))
        (eshell--execute-command (format "bundle exec %s" cmd) nil)
      (eshell--execute-command cmd nil))))
