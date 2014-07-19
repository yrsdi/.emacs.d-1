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
