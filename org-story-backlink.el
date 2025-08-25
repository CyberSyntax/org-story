;;; org-story-backlink.el --- Backlink/in-degree helpers for org-story -*- lexical-binding: t; -*-
;;; Commentary:
;; - Compute in-degree (>0 backlinks) for IDs using Org-roam DB.
;; - Apply source-class predicate on the backlink SOURCE file (via files.title or path).
;; - Robust to DBs that store quoted strings ("id", "\"UUID\"", "\"/path/file.org\"").
;; - Provides heading-level in-degree filter so constraints apply to headings’ node IDs.

;;; Code:

(require 'org)
(require 'org-roam)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'org-story-custom)

;; -------------------------------
;; Helpers
;; -------------------------------

(defun org-story--strip-outer-quotes (s)
  "If S looks like \"...\", return ...; otherwise return S unchanged."
  (if (and (stringp s)
           (>= (length s) 2)
           (eq (aref s 0) ?\")
           (eq (aref s (1- (length s))) ?\"))
      (substring s 1 (1- (length s)))
    s))

(defun org-story--ids-with-quote-variants (ids)
  "Return a vector including each ID and its quoted variant \"ID\".
Handles DB rows stored with outer quotes."
  (vconcat
   (apply #'append
          (mapcar (lambda (id) (list id (format "\"%s\"" id))) ids))))

;; -------------------------------
;; Source-class predicates
;; -------------------------------

(defun org-story--outline-title-match-p (title)
  "Return non-nil if TITLE matches any regexp in `org-story-outline-title-regexps`."
  (let ((rxs (or org-story-outline-title-regexps '())))
    (and (stringp title)
         (cl-some (lambda (rx)
                    (ignore-errors (and (stringp rx) (string-match-p rx title))))
                  rxs))))

(defun org-story--src-class-any-p (_file _title _source-id _row)
  "Accept any source file."
  t)

(defun org-story--src-class-outline-title-p (_file title _source-id _row)
  "Accept sources whose files.title matches outline regexps."
  (org-story--outline-title-match-p title))

(defun org-story--source-class->predicate (class)
  "Resolve CLASS to a predicate of (file title source-id row)."
  (let* ((entry (assq class org-story-backlink-source-classes))
         (pred-spec (plist-get (cdr entry) :predicate)))
    (cond
     ((functionp pred-spec) pred-spec)
     ((and (symbolp pred-spec) (fboundp pred-spec)) (symbol-function pred-spec))
     ((eq class 'any) #'org-story--src-class-any-p)
     (t #'org-story--src-class-any-p))))

;; -------------------------------
;; In-degree on ID set (dest -> source -> file/title)
;; -------------------------------

(defun org-story--ids-indeg-pos-set-by-class (ids &optional class)
  "Return a hash-set of IDS whose in-degree > 0 under source CLASS."
  (let ((class (or class org-story-backlink-source-class-default))
        (set (make-hash-table :test 'equal)))
    (when ids
      (condition-case _
          (let* ((vec  (org-story--ids-with-quote-variants ids))
                 (pred (org-story--source-class->predicate class))
                 (rows (org-roam-db-query
                        [:select [links:dest files:title files:file links:source links:type]
                         :from links
                         :inner-join nodes :on (= links:source nodes:id)
                         :inner-join files :on (= nodes:file files:file)
                         :where links:dest :in $v1]
                        vec)))
            (dolist (r rows)
              ;; r = [dest title file source type]
              (let* ((dest*  (org-story--strip-outer-quotes (nth 0 r)))
                     (title* (org-story--strip-outer-quotes (nth 1 r)))
                     (file*  (org-story--strip-outer-quotes (nth 2 r)))
                     (sid    (nth 3 r))
                     (type*  (org-story--strip-outer-quotes (nth 4 r))))
                (when (and (string= type* "id")
                           (funcall pred file* title* sid r))
                  (puthash dest* t set)))))
        (error (clrhash set))))
    set))

;; -------------------------------
;; Heading-level in-degree filter
;; -------------------------------

(defun org-story--heading-id-from-marker (marker)
  "Return the :ID of the heading at MARKER; nil if missing."
  (when (and (markerp marker) (marker-buffer marker))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (condition-case nil (org-back-to-heading t) (error nil))
        (org-entry-get (point) "ID")))))

(defun org-story-filter-headings-by-indegree (headings constraint &optional class)
  "Filter HEADINGS (list of (HEADING . MARKER)) by in-degree CONSTRAINT under CLASS.
- Extract each heading’s node ID (:ID).
- Compute in-degree for those IDs via `org-story--ids-indeg-pos-set-by-class`.
- Keep headings per CONSTRAINT:
  - 'indeg-pos  → keep if ID present in the positive set
  - 'indeg-zero → keep if ID absent in the positive set
  - 'none      → return HEADINGS unchanged"
  (let* ((class (or class org-story-backlink-source-class-default)))
    (pcase constraint
      ('none headings)
      (_
       (let* ((id-map (make-hash-table :test 'equal))
              (ids '()))
         (dolist (h headings)
           (let* ((mk (cdr h))
                  (nid (org-story--heading-id-from-marker mk)))
             (when (and nid (not (gethash nid id-map)))
               (puthash nid t id-map)
               (push nid ids))))
         (setq ids (nreverse ids))
         (let* ((pos-set (org-story--ids-indeg-pos-set-by-class ids class))
                (kept '()))
           (dolist (h headings)
             (let* ((mk (cdr h))
                    (nid (org-story--heading-id-from-marker mk))
                    (present (and nid (gethash nid pos-set))))
               (when (pcase constraint
                       ('indeg-pos  present)
                       ('indeg-zero (not present))
                       (_ t))
                 (push h kept))))
           (nreverse kept)))))))

;; -------------------------------
;; ID-level filters (kept for compatibility)
;; -------------------------------

(defun org-story-filter-ids-by-backlink-constraint (ids constraint &optional class)
  "Filter IDS by in-degree CONSTRAINT under source CLASS.
CONSTRAINT:
- 'none       : return IDS unchanged
- 'indeg-pos  : keep only IDs with in-degree > 0
- 'indeg-zero : keep only IDs with in-degree = 0"
  (let ((constraint (or constraint org-story-backlink-constraint-default)))
    (pcase constraint
      ('none ids)
      (_
       (let* ((pos-set (org-story--ids-indeg-pos-set-by-class ids class)))
         (cl-remove-if-not
          (lambda (id)
            (pcase constraint
              ('indeg-pos  (gethash id pos-set))
              ('indeg-zero (not (gethash id pos-set)))
              (_ t)))
          ids))))))

(defun org-story-filter-grouped-selection-by-backlink-constraint (sel constraint &optional class)
  "Filter grouped SEL by in-degree CONSTRAINT under source CLASS.
SEL plist: :chars IDS :locs IDS."
  (let* ((chars  (plist-get sel :chars))
         (locs   (plist-get sel :locs))
         (chars* (org-story-filter-ids-by-backlink-constraint chars constraint class))
         (locs*  (org-story-filter-ids-by-backlink-constraint locs  constraint class)))
    (list :chars chars* :locs locs*
          :char-op (or (plist-get sel :char-op) 'or)
          :loc-op  (or (plist-get sel :loc-op)  'or)
          :groups-op (or (plist-get sel :groups-op) 'or))))

(provide 'org-story-backlink)
;;; org-story-backlink.el ends here
