;;; prag-prog.el --- Tools to help write and edit Prag Prog books. -*- lexical-binding: t -*-

;;; Commentary:
;;

(require 'cl-lib)

;;; Code:

(defvar-local prag-prog-tags-hidden nil "Whether tags are hidden or not.")

(cl-defstruct (prag-prog-tag
               (:constructor prag-prog-tag-create)
               (:copier nil))
  name highlight)

(defvar prag-prog-tags
  (list
   (prag-prog-tag-create :name "filename")
   (prag-prog-tag-create :name "dir")
   (prag-prog-tag-create :name "ed" :highlight 'hi-yellow)
   (prag-prog-tag-create :name "author" :highlight 'hi-blue)))

(defun prag-prog-insert-author-tag ()
    (interactive)
    (insert "<author></author>")
    (backward-char 9))

(defun prag-prog-tag-re (tag)
  "Regexp for TAG."
  (let ((name (prag-prog-tag-name tag)))
    (format "<%s>\\(.\\|\n\\)*?</%s>" name name)))

(defun prag-prog-format-defun-name (string &rest objects)
  "Format OBJECTS with STRING and return as a symbol."
  (intern (apply #'format string objects)))

(cl-dolist (tag prag-prog-tags)
  (eval
   (let* (
          (tag-re (prag-prog-tag-re tag))
          (tag-name (prag-prog-tag-name tag))
          (tag-open (format "<%s>" tag-name))
          (tag-close (format "</%s>" tag-name))
          (tag-close-length (length tag-close))
          (highlight-face (prag-prog-tag-highlight tag))

          (defun-insert (prag-prog-format-defun-name "prag-prog-tag-%s-insert" tag-name))
          (defun-next (prag-prog-format-defun-name "prag-prog-tag-%s-next" tag-name))
          (defun-prev (prag-prog-format-defun-name "prag-prog-tag-%s-prev" tag-name))
          (defun-highlight (prag-prog-format-defun-name "prag-prog-tag-%s-highlight" tag-name)))
     `(progn
        (cl-defun ,defun-next ()
          "Move to the next tag."
          (interactive)
          (search-forward ,tag-open))

        (cl-defun ,defun-prev ()
          "Move to the previous tag."
          (interactive)
          (search-backward ,tag-open))

        (cl-defun ,defun-insert ()
          "Inert the tag."
          (interactive)
          (insert ,tag-open ,tag-close)
          (backward-char ,tag-close-length))

        (cl-defun ,defun-highlight ()
          "Highlight the tag."
          (interactive)
          (highlight-regexp ,tag-re ',highlight-face nil))))))


(defun prag-prog-stat ()
  "Count number of tags."
  (interactive)
  (let ((counts
         (s-join "\n"
                 (let* ((str (buffer-substring-no-properties (point-min) (point-max))))
                   (cl-mapcar (lambda (tag)
                                (format "%s %d"
                                        (prag-prog-tag-name tag)
                                        (s-count-matches (prag-prog-tag-re tag) str)))
                              prag-prog-tags)))))
    (with-temp-buffer
      (insert counts)
      (align-regexp (point-min) (point-max) ".* \\([0-9,]+\\).*" -1)
      (message (buffer-substring-no-properties (point-min) (point-max))))))

(defun prag-prog-tags-toggle-visibility ()
  "Toggle visbility of tags."
  (interactive)
  (save-excursion
    (cl-dolist (tag prag-prog-tags)
      (condition-case nil
          (goto-char (point-min))
        (while (not (eobp))
          (re-search-forward (prag-prog-tag-re tag))
          (let ((beg (match-beginning 0))
                (end (match-end 0)))
            (if prag-prog-tags-hidden
                (remove-from-invisibility-spec :prag-prog)
              (add-to-invisibility-spec :prag-prog)
              (put-text-property beg end :prag-prog t)
              (put-text-property beg end 'invisible :prag-prog))))))
    (setq prag-prog-tags-hidden (not prag-prog-tags-hidden))))

(defun prag-prog-tags-highlight ()
  "Highlight of tags."
  (interactive)
  (save-excursion
    (cl-dolist (tag prag-prog-tags)
      (when (prag-prog-tag-highlight tag)
        (goto-char (point-min))
        (highlight-regexp (prag-prog-tag-re tag) (prag-prog-tag-highlight tag) nil)))))

(defun prag-prog-tags-unhighlight ()
  "Unhighlight of tags."
  (interactive)
  (save-excursion
    (cl-dolist (tag prag-prog-tags)
      (goto-char (point-min))
      (unhighlight-regexp (prag-prog-tag-re tag)))))

(define-minor-mode prag-prog-mode
  "Mode for writing Prag Prog markdown."
  :lighter " prag-prog-mode"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-p") 'prag-prog-tag-ed-prev)
            (define-key map (kbd "M-n") 'prag-prog-tag-ed-next)
            (define-key map (kbd "C-c C-c a") 'prag-prog-tag-author-insert)
            (define-key map (kbd "C-c C-c f") 'prag-prog-tag-filename-insert)
            (define-key map (kbd "C-c C-c d") 'prag-prog-tag-dir-insert)
            map))

(defun prag-prog-mode-setup ()
  "Setup 'prag-prog-mode'."
  (interactive)
  (prag-prog-tags-highlight))

(add-hook 'prag-prog-mode-hook 'prag-prog-mode-setup)

(provide 'prag-prog)

;;; prag-prog.el ends here
