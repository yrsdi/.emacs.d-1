;;; proced-narrow.el --- Live-narrowing of search results for proced.

;; Copyright (C) 2019 Travis Jeffery

;; Author: Travis Jeffery <tj@travisjeffery.com>
;; Maintainer: Travis Jeffery <tj@travisjeffery.com>
;; URL: https://github.com/travisjeffery/proced-narrow
;; Keywords: processes, proced
;; Created: 15th July 2019
;; Version: 1.0.0
;; Package-requires: ((dash "2.7.0") (proced))

;;; Commentary:

;; This package provdes live filtering of processes in proced buffer.  In general, after calling the
;; respective narrowing function you type a filter string into the minibuffer.  After each change
;; proced-narrow automatically reflects the change in the buffer.  Typing C-g will cancel the
;; narrowing and restore the origininal view, typing RET will exit the live filtering and leave the
;; proced buffer in the narrow state.  To bring it back to the original view, you can call `revert
;; buffer' (usually bound to g).

;;; Code:

(require 'proced)
(require 'dash)

(defgroup proced-narrow ()
  "Live-narrowing of search results for proced."
  :group 'proced-hacks
  :prefix "proced-narrow-")

(defvar proced-narrow-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-g") 'minibuffer-keyboard-quit)
    (define-key map (kbd "RET") 'exit-minibuffer)
    (define-key map (kbd "<return>") 'exit-minibuffer)
    map)
  "Keymap used while `proced-narrow' is reading the pattern.")

(defun proced-narrow ()
  "Narrow a proced buffer to the processes matching a string.

If the string contains spaces, then each word is matched against
the process name separately.  To succeed, all of tthem have to
match but the order does not matter."
  (interactive)
  (proced-narrow--internal 'proced-narrow--string-filter))

(defun proced-narrow--string-filter (filter)
  "Return t if FILTER is non-nil for the current file."
  (let ((words (split-string filter " ")))
    (--all? (save-excursion (search-forward it (line-end-position) t)) words)))

(defun proced-narrow--remove-text-with-property (prop)
  "Delete all text in the current buffer with text property PROP."
  (let ((start (point-min))
        end)
    (unless (get-text-property start prop)
      (setq start (next-single-property-change start prop)))
    (while start
      (setq end (text-property-any start (point-max) prop nil))
      (delete-region start (or end (point-max)))
      (setq start (when end
                    (next-single-property-change start prop))))))

(define-minor-mode proced-narrow-mode
  "Minor mode for indicating when narrowing is in progress."
  :lighter " proced-narrow")

(defun proced-narrow--disable-on-revert ()
  "Disable `proced-narrow-mode' after revert."
  (proced-narrow-mode -1))

(defun proced-narrow--internal (filter-function)
  "Narrow a proced buffer to the processes matching a filter.

The function FILTER-FUNCTION is called on each line: if it
returns non-nil, the line is kept, otherwise it is removed.  The
function takes on argument, which is the current filter string
read from the minibuffer."
  ( let ((proced-narrow-buffer (current-buffer))
         (proced-narrow-filter-function filter-function))
    (unwind-protect
        (progn
          (proced-narrow-mode 1)
          (add-to-invisibility-spec :proced-narrow)
          (read-from-minibuffer "Filter: " nil proced-narrow-map)
          (let ((inhibit-read-only t))
            (proced-narrow--remove-text-with-property :proced-narrow)))
      (with-current-buffer proced-narrow-buffer
        (remove-from-invisibility-spec :proced-narrow)
        (proced-narrow--restore)))))

(defun proced-narrow--restore ()
  "Restore the invisible files of the current buffer."
  (let ((inhibit-read-only t))
    (remove-list-of-text-properties (point-min) (point-max)
                                    '(invisible :proced-narrow))))

(defvar proced-narrow-buffer nil
  "Proced buffer we are currently filtering.")

(defvar proced-narrow--minibuffer-content ""
  "Content of the minibuffer during narrowing.")

(defvar proced-narrow-filter-function 'identity
  "Filter function used to filter the proced view.")

(defun proced-narrow--minibuffer-setup ()
  "Set up the minibuffer for live filtering."
  (when proced-narrow-buffer
    (add-hook 'post-command-hook 'proced-narrow--live-update nil :local)))

(add-hook 'minibuffer-setup-hook 'proced-narrow--minibuffer-setup)

(defun proced-narrow--live-update ()
  "Update the proced buffer based on the contents of the minibuffer."
  (when proced-narrow-buffer
    (let ((current-filter (minibuffer-contents-no-properties)))
      (with-current-buffer proced-narrow-buffer
        (unless (equal current-filter proced-narrow--minibuffer-content)
          (proced-narrow--update current-filter))))))

(defun proced-narrow--update (filter)
  "Make the processes not matching the FILTER invisible."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (proced-narrow--restore)
      (while (not (eobp))
        (unless (funcall proced-narrow-filter-function filter)
          (put-text-property (line-beginning-position) (1+ (line-end-position)) :proced-narrow t)
          (put-text-property (line-beginning-position) (1+ (line-end-position)) 'invisible :proced-narrow))
        (forward-line 1)))))

(provide 'proced-narrow)
;;; proced-narrow.el ends here