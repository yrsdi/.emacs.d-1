(defun prag-prog-next-ed-tag ()
  "Move to the next ed tag."
  (interactive)
  (search-forward "<ed>"))

(defun prag-prog-prev-ed-tag ()
  "Move to the previous ed tag."
  (interactive)
  (search-backward "<ed>"))

(defun prag-prog-highlight-ed-author-tags ()
  "Highlight ed and author tags."
  (interactive)
  (highlight-regexp "<ed>\\(.*\\)</ed>" 'hi-yellow nil)
  (highlight-regexp "<author>\\(.*\\)</author>" 'hi-blue nil))

(defvar-local prag-prog-tags-hidden nil "Whether tags are hidden or not.")

(defconst prag-prog-ed-re "<ed>\\(.*\\)</ed>")
(defconst prag-prog-author-re "<author>\\(.*\\)</author>")

(defun prag-prog-stat ()
  "Count number of ed, author tags."
  (interactive)
  (let* ((str (buffer-substring-no-properties (point-min) (point-max)))
         (ed-count (s-count-matches prag-prog-ed-re str))
         (author-count (s-count-matches prag-prog-author-re str)))
    (message (format "ed: %d, author: %d" ed-count author-count))))

(defun prag-prog-hide-ed-author-tags ()
  "Toggle visibility of ed, author tags."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (re-search-forward prag-prog-ed-re)
      (let ((beg (match-beginning 0))
            (end (match-end 0)))
        (if prag-prog-tags-hidden
            (remove-from-invisibility-spec :prag-prog)
          (add-to-invisibility-spec :prag-prog)
          (put-text-property beg end :prag-prog t)
          (put-text-property beg end 'invisible :prag-prog)))))
  (setq prag-prog-tags-hidden (not prag-prog-tags-hidden)))

(define-minor-mode prag-prog-mode
  "Mode for writing Prag Prog markdown."
  :lighter " prag-prog-mode"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "M-p") 'prag-prog-prev-ed-tag)
            (define-key map (kbd "M-n") 'prag-prog-next-ed-tag)
            map))

(defun prag-prog-mode-setup ()
  "Setup prag-prog-mode."
  (interactive)
  (prag-prog-highlight-ed-author-tags))

(add-hook 'prag-prog-mode-hook 'prag-prog-mode-setup)

(provide 'prag-prog)
