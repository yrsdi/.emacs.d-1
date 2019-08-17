;;; -*- lexical-binding: t -*-


(require 'cl-lib)

(defvar-local prag-prog-tags-hidden nil "Whether tags are hidden or not.")

(cl-defstruct (prag-prog-tag
               (:constructor prag-prog-tag-create)
               (:copier nil))
  name highlight)

(defvar prag-prog-tag-ed
  (prag-prog-tag-create :name "ed" :highlight 'hi-yellow))

(defvar prag-prog-tag-author
  (prag-prog-tag-create :name "author" :highlight 'hi-blue))

(defvar prag-prog-tags
      (list prag-prog-tag-ed
        prag-prog-tag-author))

(defun prag-prog--format (string &rest objects)
  (intern (apply #'format string objects)))

(defun prag-prog-tag-re (tag)
  (let* ((name (prag-prog-tag-name tag)))
    (format "<%s>.*?\\(\n.*\\)*</%s>" name name)))

(cl-dolist (tag prag-prog-tags)
  (eval
   (let* ((tag-re (prag-prog-tag-re tag))
          (tag-name (prag-prog-tag-name tag))
          (high-face (prag-prog-tag-highlight tag))
          (open-tag (format "<%s>" tag-name))
          (defun-next (prag-prog--format "prag-prog-tag-%s-next" tag-name))
          (defun-prev (prag-prog--format "prag-prog-tag-%s-prev" tag-name))
          (defun-high (prag-prog--format "prag-prog-tag-%s-highlight" tag-name)))
     `(progn
        (cl-defun ,defun-next ()
          "Move to the next ,tag-name tag."
          (interactive)
          (search-forward ,open-tag))

        (cl-defun ,defun-prev ()
          "Move to the previous ,tag-name tag."
          (interactive)
          (search-backward ,open-tag))

        (cl-defun ,defun-high ()
          "Highlight ,tag-name tags."
          (interactive)
          (highlight-regexp ,tag-re ',high-face nil))))))

(defun prag-prog-stat ()
  "Count number of tags."
  (interactive)
  (message
   (s-join ", "
           (let* ((str (buffer-substring-no-properties (point-min) (point-max))))
             (cl-mapcar (lambda (tag)
                          (format "%s: %d"
                                  (prag-prog-tag-name tag)
                                  (s-count-matches (prag-prog-tag-re tag) str)))
                        prag-prog-tags)))))


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
      (goto-char (point-min))
      (highlight-regexp (prag-prog-tag-re tag) (prag-prog-tag-highlight tag) nil))))

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
            map))

(defun prag-prog-mode-setup ()
  "Setup prag-prog-mode."
  (interactive)
  (prag-prog-tags-highlight))

(add-hook 'prag-prog-mode-hook 'prag-prog-mode-setup)

(provide 'prag-prog)
