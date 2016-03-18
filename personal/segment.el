(defun s-find-file ()
  (interactive)
  (ido-find-file-in-dir "~/dev/src/github.com/segmentio/"))

(defun s-open-repo (repo)
  (interactive "sRepo: ")
  (browse-url (format "https://github.com/segmentio/%s" repo)))
