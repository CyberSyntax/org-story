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
;; Internal debug helper (safe-call)
;; -------------------------------

(defun org-story--maybe-dbg (fmt &rest args)
  "Call org-story--dbg if available (no-op otherwise)."
  (when (and (boundp 'org-story-backlink-debug)
             org-story-backlink-debug
             (fboundp 'org-story--dbg))
    (apply #'org-story--dbg fmt args)))

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
    ;; Debug: begin + snapshot
    (org-story--maybe-dbg "=== SHOW-PLOT begin ===")
    (org-story--maybe-dbg "SHOW-PLOT: buffer=%s"
                          (or (buffer-file-name) (buffer-name)))
    (org-story--maybe-dbg "SHOW-PLOT: source-class-default=%s" src-class)
    (org-story--maybe-dbg "SHOW-PLOT: id-links=%d | IDs=%S"
                          (length id-links)
                          (mapcar (lambda (e) (plist-get e :uuid)) id-links))
    (if (null id-links)
        (progn
          (org-story--maybe-dbg "SHOW-PLOT: no id-links in buffer")
          (message "No ID links found in buffer. Try C-u to refresh."))
      (if grouped
          ;; Grouped flow: filter by characters/locations first (content), then apply in-degree on headings.
          (let* ((chars (plist-get grouped :characters))
                 (locs  (plist-get grouped :locations))
                 (res   (progn
                          (org-story--maybe-dbg "SHOW-PLOT (grouped): candidates chars=%d locs=%d"
                                                (length chars) (length locs))
                          (org-story--maybe-dbg "SHOW-PLOT (grouped): candidate CHAR UUIDs=%S"
                                                (mapcar (lambda (c) (plist-get c :uuid)) chars))
                          (org-story--maybe-dbg "SHOW-PLOT (grouped): candidate LOC UUIDs=%S"
                                                (mapcar (lambda (c) (plist-get c :uuid)) locs))
                          (org-story--read-grouped chars locs))))
            (if res
                (let* ((sel (nth 0 res))    ;; grouped plist
                       (_op (nth 1 res))    ;; 'grouped
                       (constraint (org-story--read-indegree-constraint)))
                  (org-story--maybe-dbg "SHOW-PLOT (grouped): pre-constraint sel.chars=%S"
                                        (plist-get sel :chars))
                  (org-story--maybe-dbg "SHOW-PLOT (grouped): pre-constraint sel.locs=%S"
                                        (plist-get sel :locs))
                  (org-story--maybe-dbg "SHOW-PLOT (grouped): constraint=%s class=%s"
                                        constraint src-class)
                  ;; First get candidate headings from content selection (no in-degree here):
                  (let* ((heads0 (org-get-filtered-headings-from-buffer-grouped sel))
                         ;; Now apply in-degree constraint to the HEADINGS’ node IDs:
                         (heads (org-story-filter-headings-by-indegree heads0 constraint src-class)))
                    (org-story--maybe-dbg "SHOW-PLOT (grouped): headings content=%d -> after indeg=%d"
                                          (length heads0) (length heads))
                    (if (null heads)
                        (progn
                          (org-story--maybe-dbg "SHOW-PLOT (grouped): result empty after heading indegree")
                          (message "No narrative elements remain after in-degree filter."))
                      (setq plot-buf
                            (org-display-filtered-headings-buffer heads sel 'grouped (current-buffer))))))
              (progn
                (org-story--maybe-dbg "SHOW-PLOT (grouped): user selection canceled/empty")
                (message "No narrative elements selected. Try selecting at least one."))))
        ;; Legacy flow: choose IDs by title → filter content → apply in-degree on headings
        (let* ((titles (delete-dups (mapcar (lambda (e) (plist-get e :title)) id-links)))
               (selected-ids nil)
               (first-prompt t)
               (continue t))
          (when (null titles)
            (org-story--maybe-dbg "SHOW-PLOT (legacy): no titles derived from id-links")
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
          (org-story--maybe-dbg "SHOW-PLOT (legacy): selected-ids=%S" selected-ids)
          (if selected-ids
              (let* ((operator (if (> (length selected-ids) 1)
                                   (intern (completing-read "Operator (and/or): " '("and" "or") nil t))
                                 'or))
                     (constraint (org-story--read-indegree-constraint)))
                (org-story--maybe-dbg "SHOW-PLOT (legacy): operator=%s constraint=%s class=%s"
                                      operator constraint src-class)
                ;; First get candidate headings from content selection (no in-degree here):
                (let* ((heads0 (org-get-filtered-headings-from-buffer selected-ids operator))
                       ;; Now apply in-degree constraint to HEADINGS’ node IDs:
                       (heads (org-story-filter-headings-by-indegree heads0 constraint src-class)))
                  (org-story--maybe-dbg "SHOW-PLOT (legacy): headings content=%d -> after indeg=%d"
                                        (length heads0) (length heads))
                  (if (null heads)
                      (progn
                        (org-story--maybe-dbg "SHOW-PLOT (legacy): result empty after heading indegree")
                        (message "No narrative elements remain after in-degree filter."))
                    (setq org-story--last-selected-ids selected-ids)
                    (setq plot-buf
                          (org-display-filtered-headings-buffer heads selected-ids operator (current-buffer))))))

            (progn
              (org-story--maybe-dbg "SHOW-PLOT (legacy): user selection canceled/empty")
              (message "No narrative elements selected. Try picking one or use C-u to refresh."))))))
    (org-story--maybe-dbg "=== SHOW-PLOT end ===")
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