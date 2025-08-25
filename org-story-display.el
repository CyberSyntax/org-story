;;; org-story-display.el --- Narrative plot display and navigation -*- lexical-binding: t; -*-
;;; Commentary:
;; Plot buffer creation, clickable segments, and derived mode.
;;; Code:

(require 'org)

(require 'org-story-custom)
(require 'org-story-clean)
(require 'org-story-context)

(defvar-local org-filter-original-buffer nil)
(defvar-local org-filter-selected-ids nil)
(defvar-local org-filter-operator nil)
(defvar-local org-plot-focus-sentence nil)
(defvar-local org-filter-original-file nil)

(defun org-display-filtered-headings-buffer (headings ids operator source-buffer)
  "Display filtered HEADINGS as a narrative plot in a vertically split window."
  (let ((plot-buf (get-buffer-create "*Narrative Plot*"))
        (orig-buf source-buffer))
    (with-current-buffer plot-buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (setq-local org-filter-original-buffer source-buffer)
      (setq-local org-filter-selected-ids ids)
      (setq-local org-filter-operator operator)
      (setq-local org-plot-focus-sentence org-story--last-focus-sentence)
      (setq-local org-filter-original-file
                  (and (buffer-live-p source-buffer)
                       (buffer-file-name source-buffer)))
      (let ((n (length headings))
            (i 0))
        (dolist (entry headings)
          (setq i (1+ i))
          (let* ((raw (car entry))
                 (marker (cdr entry))
                 (clean-text (org-clean-heading-text raw))
                 (start (point)))
            (insert clean-text)
            (add-text-properties start (point)
                                 `(orig-marker ,marker
                                               orig-pos ,(marker-position marker)
                                               mouse-face highlight
                                               help-echo "RET or click: Jump to original heading"))
            ;; Set focus sentence if this marker matches the focus marker
            (when (and org-story--last-focus-marker
                       (= (marker-position marker)
                          (marker-position org-story--last-focus-marker)))
              (setq-local org-plot-focus-sentence clean-text))
            (when (< i n)
              (insert org-story-plot-joiner)))))
      (goto-char (point-min))
      (org-filtered-heading-mode))
    (delete-other-windows)
    (let* ((base (selected-window))
           (frac (max 0.05 (min 0.95 org-story-source-window-fraction)))
           (hsize (max 5 (floor (* (window-total-height base) frac)))))
      (if (eq org-story-plot-window-position 'bottom)
          ;; Source on top (size = hsize), Plot on bottom, focus Plot
          (let ((top (split-window base hsize 'above)))
            (set-window-buffer top orig-buf)
            (set-window-buffer base plot-buf)
            (select-window base))
        ;; Plot on top, Source on bottom (size = hsize), focus Plot
        (let ((bottom (split-window base hsize 'below)))
          (set-window-buffer base plot-buf)
          (set-window-buffer bottom orig-buf)
          (select-window base))))
    plot-buf))

(defvar org-filtered-heading-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'org-filtered-heading-open-original)
    (define-key map [mouse-1] 'org-filtered-heading-open-original)
    (define-key map [mouse-2] 'org-filtered-heading-open-original)
    map)
  "Keymap for `org-filtered-heading-mode'.")

(define-derived-mode org-filtered-heading-mode special-mode "Narrative-Plot"
  "Major mode for viewing the narrative plot.
In this mode, each segment is clickable (via its attached marker) so that RET or a left-click
jumps to the original heading.")

(defun org-filtered-heading-open-original ()
  "Jump to the original heading corresponding to the segment at point.
Uses the text property `orig-marker` attached to the segment."
  (interactive)
  (let* ((marker (get-text-property (point) 'orig-marker))
         (pos    (or (and marker (marker-position marker))
                     (get-text-property (point) 'orig-pos)))
         (buf    (and marker (marker-buffer marker)))
         (file   (or (and (buffer-live-p buf) (buffer-file-name buf))
                     org-filter-original-file)))
    (cond
     ((buffer-live-p buf)
      (pop-to-buffer buf)
      (goto-char (or pos (point)))
      (recenter))
     ((and file pos)
      (pop-to-buffer (find-file-noselect file))
      (goto-char pos)
      (recenter))
     (t
      (message "Original location not available.")))))

(provide 'org-story-display)
;;; org-story-display.el ends here
