(defun t-make-blog-post (title)
  "Make a new blog post with the given `title'."
  (interactive "sTitle: ")
  (let ((default-directory (expand-file-name "~/dev/homesite/"))
        (re "\\(?:^Creating new post: \\(.*\\)$\\|.*\\)"))
    (find-file (concat default-directory (s-trim (replace-regexp-in-string re "\\1" (shell-command-to-string (format "bundle exec rake \"new_post[%s]\"" title))))))))

(provide 'blog)
