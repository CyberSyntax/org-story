;;; org-story-commands.el --- User-facing commands for org-story -*- lexical-binding: t; -*-
;;; Commentary:
;; Interactive commands like org-story-show-plot and key bindings.
;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'seq)

(require 'org-story-core)
(require 'org-story-custom)
(require 'org-story-cache)
(require 'org-story-clean)
(require 'org-story-context)
(require 'org-story-group)
(require 'org-story-filter)
(require 'org-story-display)
(require 'org-story-gptel)
(require 'org-story-backlink)

;; -------------------------------
;; High-level command(s)
;; -------------------------------

;;;###autoload
(defun org-story-show-plot (&optional arg)
  "Interactively select narrative elements and display their narrative plot.
With prefix ARG (C-u), force a fresh scan (ignore cache)."
  (interactive "P")
  (setq org-story--last-focus-sentence
        (or (org-story--heading-line-at-point)
            org-story--last-focus-sentence))
  (setq org-story--last-focus-marker
        (org-story--marker-at-point))
  (let* ((id-links (org-get-id-links-in-buffer-cached arg))
         (grouped (org-story--collect-candidates id-links))
         (plot-buf nil)
         (src-class org-story-backlink-source-class-default))
    (if (null id-links)
        (message "No ID links found in buffer. Try C-u to refresh.")
      (if grouped
          ;; Grouped flow: content filter first; then apply in-degree on headings’ node IDs.
          (let* ((chars (plist-get grouped :characters))
                 (locs  (plist-get grouped :locations))
                 (res   (org-story--read-grouped chars locs)))
            (if res
                (let* ((sel (nth 0 res))    ;; grouped plist
                       (_op (nth 1 res))    ;; 'grouped
                       (constraint (org-story--read-indegree-constraint))
                       ;; 1) content-filter (by link IDs)
                       (heads0 (org-get-filtered-headings-from-buffer-grouped sel))
                       ;; 2) apply in-degree on heading node IDs (backlinks from class)
                       (heads (org-story-filter-headings-by-indegree heads0 constraint src-class)))
                  (if (null heads)
                      (message "No narrative elements remain after in-degree filter.")
                    (setq plot-buf
                          (org-display-filtered-headings-buffer heads sel 'grouped (current-buffer)))))
              (message "No narrative elements selected. Try selecting at least one.")))
        ;; Legacy flow: select titles -> content filter -> in-degree on headings
        (let* ((titles (delete-dups (mapcar (lambda (e) (plist-get e :title)) id-links)))
               (selected-ids nil)
               (first-prompt t)
               (continue t))
          (when (null titles)
            (message "No titles found from ID links."))
          (while (and continue titles)
            (let* ((prompt (if (and first-prompt org-story--last-selected-ids)
                               "Narrative element (RET to reuse previous selection): "
                             "Narrative element (RET to finish): "))
                   (title (completing-read prompt titles nil nil)))
              (if (string= title "")
                  (progn
                    (when (and first-prompt org-story--last-selected-ids)
                      (setq selected-ids org-story--last-selected-ids))
                    (setq continue nil))
                (setq first-prompt nil)
                (let ((matching-ids (mapcar (lambda (entry) (plist-get entry :uuid))
                                            (seq-filter (lambda (entry)
                                                          (string= title (plist-get entry :title)))
                                                        id-links))))
                  (setq selected-ids (append selected-ids matching-ids))
                  (setq titles (remove title titles))))))
          (if selected-ids
              (let* ((operator (if (> (length selected-ids) 1)
                                   (intern (completing-read "Operator (and/or): " '("and" "or") nil t))
                                 'or))
                     (constraint (org-story--read-indegree-constraint))
                     ;; 1) content-filter (by link IDs)
                     (heads0 (org-get-filtered-headings-from-buffer selected-ids operator))
                     ;; 2) apply in-degree on heading node IDs
                     (heads (org-story-filter-headings-by-indegree heads0 constraint src-class)))
                (if (null heads)
                    (message "No narrative elements remain after in-degree filter.")
                  (setq org-story--last-selected-ids selected-ids)
                  (setq plot-buf (org-display-filtered-headings-buffer heads selected-ids operator (current-buffer)))))
            (message "No narrative elements selected. Try picking one or use C-u to refresh.")))))
    (when (and plot-buf (featurep 'gptel))
      (condition-case err
          (org-story--post-plot-dispatch)
        (user-error (message "GPTel dispatch skipped: %s" (error-message-string err)))))))

(defun org-story--read-indegree-constraint ()
  "Prompt for the in-degree constraint; return 'none, 'indeg-pos, or 'indeg-zero."
  (let* ((choices '(("None" . none)
                    (">0 (present in source-class)" . indeg-pos)
                    ("=0 (absent in source-class)"  . indeg-zero)))
         (default-label (pcase org-story-backlink-constraint-default
                          ('indeg-pos  ">0 (present in source-class)")
                          ('indeg-zero "=0 (absent in source-class)")
                          (_           "None")))
         (label (completing-read
                 (format "In-degree filter (default: %s): " default-label)
                 (mapcar #'car choices) nil t nil nil default-label)))
    (cdr (assoc label choices))))

;; -------------------------------
;; Key binding(s)
;; -------------------------------

(defcustom org-story-bind-keys t
  "If non-nil, bind org-story keys globally."
  :type 'boolean
  :group 'org-story)

(when org-story-bind-keys
  (global-set-key (kbd "C-c s p") 'org-story-show-plot))

(provide 'org-story-commands)
;;; org-story-commands.el ends here