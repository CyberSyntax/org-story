;;; org-story-backlink.el --- Backlink/in-degree helpers for org-story -*- lexical-binding: t; -*-
;;; Commentary:
;; Compute in-degree (>0 backlinks) for selected IDs using Org-roam DB.
;;
;; Workflow (dest → source → file/title):
;; - Query links where links.dest is any of the selected IDs.
;; - For each row, use links.source → nodes → files to get the source file path/title.
;; - Keep only type="id" links and apply a source-class predicate (by title/path).
;; - If predicate passes, mark that dest as having in-degree > 0.
;;
;; Robustness:
;; - DBs storing quoted strings ("id", "\"UUID\"") are normalized.
;;
;; Debug:
;; - Full row-by-row dump in a dedicated buffer; concise summaries in *Messages*.

;;; Code:

(require 'org)
(require 'org-roam)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'org-story-custom)

;; --------------------------------
;; Debug options + helpers
;; --------------------------------

(defcustom org-story-backlink-debug t
  "If non-nil, print debug messages and write a full dump buffer."
  :type 'boolean :group 'org-story)

(defcustom org-story-backlink-debug-echo t
  "If non-nil, also echo concise summaries to *Messages*."
  :type 'boolean :group 'org-story)

(defcustom org-story-backlink-debug-log-buffer "*org-story-debug*"
  "Buffer name for detailed debug logs."
  :type 'string :group 'org-story)

(defcustom org-story-backlink-debug-dump-rows t
  "If non-nil, dump every DB row (no truncation) during in-degree computation."
  :type 'boolean :group 'org-story)

(defun org-story--open-debug-buffer ()
  "Return the debug buffer, cleared at call-time."
  (let ((buf (get-buffer-create org-story-backlink-debug-log-buffer)))
    (with-current-buffer buf
      (setq buffer-read-only nil)
      (erase-buffer)
      (emacs-lisp-mode))
    buf))

(defun org-story--dbg (fmt &rest args)
  "Log to debug buffer; optionally echo to *Messages*."
  (when org-story-backlink-debug
    (let* ((buf (get-buffer-create org-story-backlink-debug-log-buffer))
           (msg (apply #'format fmt args)))
      (with-current-buffer buf
        (goto-char (point-max))
        (insert msg "\n"))
      (when org-story-backlink-debug-echo
        (message "%s" msg)))))

(defun org-story--strip-outer-quotes (s)
  "If S looks like \"...\", return ...; otherwise return S unchanged."
  (if (and (stringp s)
           (>= (length s) 2)
           (eq (aref s 0) ?\")
           (eq (aref s (1- (length s))) ?\"))
      (substring s 1 (1- (length s)))
    s))

(defun org-story--ids-with-quote-variants (ids)
  "Return a vector including each ID and its quoted variant \"ID\"."
  (vconcat
   (apply #'append
          (mapcar (lambda (id) (list id (format "\"%s\"" id))) ids))))

;; --------------------------------
;; Source-class predicates
;; --------------------------------

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

;; --------------------------------
;; In-degree: IDs (dest → source → file/title)
;; --------------------------------

(defun org-story--ids-indeg-pos-set-by-class (ids &optional class)
  "Return a hash-set of IDS whose in-degree > 0 under source CLASS."
  (let ((class (or class org-story-backlink-source-class-default))
        (set (make-hash-table :test 'equal)))
    (when org-story-backlink-debug
      (org-story--open-debug-buffer)
      (org-story--dbg "=== org-story indeg begin ===")
      (org-story--dbg "CLASS=%s; REGEXPS=%S" class org-story-outline-title-regexps)
      (org-story--dbg "INPUT IDS (%d): %S" (length ids) ids))
    (when ids
      (condition-case err
          (let* ((vec  (org-story--ids-with-quote-variants ids))
                 (pred (org-story--source-class->predicate class))
                 (rows (org-roam-db-query
                        [:select [links:dest files:title files:file links:source links:type]
                         :from links
                         :inner-join nodes :on (= links:source nodes:id)
                         :inner-join files :on (= nodes:file files:file)
                         :where links:dest :in $v1]
                        vec)))
            (org-story--dbg "DB-ROWS=%d (dest variants=%d)" (length rows) (length vec))
            (let ((kept 0) (row-idx 0))
              (when (and org-story-backlink-debug org-story-backlink-debug-dump-rows)
                (org-story--dbg "--- rows dump (no truncation) ---"))
              (dolist (r rows)
                (setq row-idx (1+ row-idx))
                ;; r = [dest title file source type]
                (let* ((dest-raw  (nth 0 r))
                       (title-raw (nth 1 r))
                       (file-raw  (nth 2 r))
                       (source-id (nth 3 r))
                       (type-raw  (nth 4 r))
                       (dest*  (org-story--strip-outer-quotes dest-raw))
                       (title* (org-story--strip-outer-quotes title-raw))
                       (file*  (org-story--strip-outer-quotes file-raw))
                       (type*  (org-story--strip-outer-quotes type-raw))
                       (type-ok (string= type* "id"))
                       (class-ok (and type-ok (funcall pred file* title* source-id r)))
                       (keep (and type-ok class-ok)))
                  (when (and org-story-backlink-debug org-story-backlink-debug-dump-rows)
                    (org-story--dbg "#%d: dest=%s | type=%s | src-id=%s | src-file=%s | src-title=%s | TYPE_OK=%s | CLASS_OK=%s | KEEP=%s"
                                    row-idx dest* type* source-id file* title*
                                    (if type-ok "1" "0")
                                    (if class-ok "1" "0")
                                    (if keep "1" "0")))
                  (when keep
                    (puthash dest* t set)
                    (cl-incf kept))))
              (org-story--dbg "KEPT_ROWS=%d | UNIQUE_DESTS=%d"
                              kept (hash-table-count set))
              (when (> (hash-table-count set) 0)
                (let (dlist)
                  (maphash (lambda (k _v) (push k dlist)) set)
                  (org-story--dbg "DESTS_KEPT (no truncation, %d): %S"
                                  (length dlist) (nreverse dlist))))))
        (error
         (org-story--dbg "DB ERROR: %s" (error-message-string err))
         (clrhash set))))
    (when org-story-backlink-debug
      (org-story--dbg "=== org-story indeg end ==="))
    set))

;; --------------------------------
;; Heading-level in-degree filter (FIX: apply constraint to headings, not link IDs)
;; --------------------------------

(defun org-story--heading-id-from-marker (marker)
  "Return the :ID of the heading at MARKER; nil if missing."
  (when (and (markerp marker) (marker-buffer marker))
    (with-current-buffer (marker-buffer marker)
      (save-excursion
        (goto-char marker)
        (condition-case _ (org-back-to-heading t) (error nil))
        (org-entry-get (point) "ID")))))

(defun org-story-filter-headings-by-indegree (headings constraint &optional class)
  "Filter HEADINGS (list of (HEADING . MARKER)) by in-degree CONSTRAINT under CLASS.
- Extract each heading’s node ID (:ID).
- Compute in-degree on those node IDs using `org-story--ids-indeg-pos-set-by-class`.
- Keep headings whose node ID satisfies CONSTRAINT:
  - 'indeg-pos  → ID present in pos-set
  - 'indeg-zero → ID absent in pos-set
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
         (org-story--dbg "HEADINGS indeg-input: %d headings; %d unique node IDs" (length headings) (length ids))
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
           (setq kept (nreverse kept))
           (org-story--dbg "HEADINGS indeg-output: %d -> %d" (length headings) (length kept))
           kept))))))

;; --------------------------------
;; (Old) ID-level filters (kept for compatibility)
;; --------------------------------

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
       (let* ((pos-set (org-story--ids-indeg-pos-set-by-class ids class))
              (out (cl-remove-if-not
                    (lambda (id)
                      (pcase constraint
                        ('indeg-pos  (gethash id pos-set))
                        ('indeg-zero (not (gethash id pos-set)))
                        (_ t)))
                    ids)))
         (org-story--dbg "FILTER(ID): constraint=%s class=%s | input=%d -> output=%d"
                         constraint (or class org-story-backlink-source-class-default)
                         (length ids) (length out))
         out)))))

(defun org-story-filter-grouped-selection-by-backlink-constraint (sel constraint &optional class)
  "Filter grouped SEL by in-degree CONSTRAINT under source CLASS.
SEL plist: :chars IDS :locs IDS :char-op OP :loc-op OP :groups-op OP."
  (let* ((chars (plist-get sel :chars))
         (locs  (plist-get sel :locs))
         (chars* (org-story-filter-ids-by-backlink-constraint chars constraint class))
         (locs*  (org-story-filter-ids-by-backlink-constraint locs  constraint class)))
    (list :chars chars*
          :locs  locs*
          :char-op (or (plist-get sel :char-op) 'or)
          :loc-op  (or (plist-get sel :loc-op)  'or)
          :groups-op (or (plist-get sel :groups-op) 'or))))

(provide 'org-story-backlink)
;;; org-story-backlink.el ends here