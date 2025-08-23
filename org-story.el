;;; org-story.el --- Narrative plot view from Org IDs -*- lexical-binding: t; -*-

;;; Commentary:
;; This package extracts narrative ID links (which may represent characters,
;; locations, scenes, etc.) from the current Org buffer and caches them (refreshing at most once per day).
;; It lets you interactively choose one or more narrative elements and then filters headings
;; to produce a single paragraph "plot" view. In the process, header markers, priority markers,
;; scheduling lines, and inline link syntax are removed and extra whitespace is normalized
;; so that exactly one space appears between each segment.
;;
;; When you invoke the interactive command (bound to "C-c s p"), the current (source) buffer
;; is split vertically. The top window displays the source buffer at about 38.2% of the total height
;; (following the golden ratio) while the bottom window displays the narrative plot (in the buffer
;; "*Narrative Plot*") using the remaining ~61.8% of the height.
;; In the plot window, pressing RET or left-clicking a segment will jump to the corresponding original heading.
;;
;; Usage:
;;   Press "C-c s p" to invoke the interactive command `org-story-show-plot`.
;;
;; Ensure that packages Org, org-element, Org-roam, cl-lib, and seq are available.

;;; Code:
(require 'org)
(require 'org-element)
(require 'org-roam)
(require 'cl-lib)     ;; For cl-every and cl-some
(require 'seq)        ;; For sequence operations
(require 'subr-x)

;; Enable Org element cache for performance.
(setq org-element-use-cache t)

(defgroup org-story nil
  "Customization for org-story plot selection."
  :group 'org)

(defcustom org-story-groups-operator-default 'or
  "Default operator to combine groups when both Characters and Locations are used.
Allowed values: 'and or 'or."
  :type '(choice (const and) (const or))
  :group 'org-story)

;;;----------------------------------------------------------------------
;;; Section 1: CACHING OF ID LINKS
;;;----------------------------------------------------------------------

(defcustom org-roam-id-links-cache-directory nil
  "Directory to store cached Org-roam ID links.
If nil, a default subdirectory is used within `org-roam-directory` (or in `user-emacs-directory`)."
  :type 'directory
  :group 'org-roam)

(unless org-roam-id-links-cache-directory
  (setq org-roam-id-links-cache-directory
        (expand-file-name
         "org-roam-id-links-cache"
         (or (bound-and-true-p org-roam-directory)
             user-emacs-directory))))
(make-directory org-roam-id-links-cache-directory t)

(defun org-get-id-links-in-buffer ()
  "Return a list of plists for each `id` link in the current buffer.
Each element is of the form (:uuid UUID :title TITLE), where TITLE is obtained
by querying the Org-roam database for the given UUID."
  (let ((uuids '()))
    (save-excursion
	(goto-char (point-min))
	(while (re-search-forward org-link-bracket-re nil t)
	  (let ((link (org-element-context)))
	    (when (string= (org-element-property :type link) "id")
	      (push (org-element-property :path link) uuids)))))
    (setq uuids (delete-dups uuids))
    (let* ((uuid-vector (vconcat uuids))
	     (uuid-title-alist
	      (org-roam-db-query
	       [:select [id title]
		:from nodes
		:where id :in $v1]
	       uuid-vector))
	     (uuid-title-hash (make-hash-table :test 'equal)))
	(dolist (pair uuid-title-alist)
	  (puthash (car pair) (cadr pair) uuid-title-hash))
	(mapcar (lambda (uuid)
		  (let ((title (gethash uuid uuid-title-hash "Untitled")))
		    (if (string= title "Untitled")
			`(:uuid ,uuid :title ,(format "Untitled (%s)" uuid))
		      `(:uuid ,uuid :title ,title))))
		uuids))))

(defun org-save-id-links-cache (cache-file result)
  "Save RESULT (a list of ID links) to CACHE-FILE with today's date stamp."
  (with-temp-file cache-file
    (let ((today (format-time-string "%Y-%m-%d")))
	(insert (prin1-to-string (list :date today :result result))))))

(defun org-load-id-links-cache (cache-file)
  "Load cached ID links from CACHE-FILE.
If the saved date matches today, return the cached result; otherwise, return nil."
  (when (file-exists-p cache-file)
    (let* ((data (with-temp-buffer
		     (insert-file-contents cache-file)
		     (read (buffer-string))))
	     (saved-date (plist-get data :date))
	     (result (plist-get data :result))
	     (today (format-time-string "%Y-%m-%d")))
	(if (string= saved-date today)
	    result
	  nil))))

(defun org-get-id-links-in-buffer-cached (&optional force)
  "Return ID links, using a per-file, per-day cache.
If FORCE is non-nil, or the buffer is modified, read from the current
buffer and bypass the cache."
  (let ((bf (buffer-file-name)))
    (cond
     ((or force (not bf))            ; no file or forced -> live parse
      (org-get-id-links-in-buffer))
     ((buffer-modified-p)            ; unsaved edits -> live parse
      (org-get-id-links-in-buffer))
     (t                              ; use the cache, else refresh it
      (let* ((cache-file (expand-file-name
                          (concat (file-name-nondirectory bf) ".id-links.cache")
                          org-roam-id-links-cache-directory))
             (cached-result (org-load-id-links-cache cache-file)))
        (if cached-result
            cached-result
          (let ((result (org-get-id-links-in-buffer)))
            (org-save-id-links-cache cache-file result)
            result)))))))

;;----------------------------------------------------------------------
;; Section 2: Character/Location grouping, frequency, and two-phase UI
;;----------------------------------------------------------------------

(defun org-story--project-root ()
  "Return the directory of the current file, or nil if not visiting a file."
  (when (buffer-file-name)
    (file-name-directory (buffer-file-name))))

(defun org-story--id-frequency-hash ()
  "Return a hash mapping UUID -> frequency of id: links in the current buffer."
  (let ((counts (make-hash-table :test 'equal)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (let ((link (org-element-context)))
          (when (string= (org-element-property :type link) "id")
            (let ((uuid (org-element-property :path link)))
              (puthash uuid (1+ (gethash uuid counts 0)) counts))))))
    counts))

(defun org-story--uuid-file-map (uuids)
  "Return a hash mapping UUID -> absolute file path via Org-roam DB."
  (let ((result (make-hash-table :test 'equal)))
    (when uuids
      (let* ((vec (vconcat uuids))
             (rows (org-roam-db-query
                    [:select [id file] :from nodes :where id :in $v1]
                    vec)))
        (dolist (row rows)
          (puthash (car row) (cadr row) result))))
    result))

(defun org-story--sort-candidates (cands)
  "Sort CANDS (list of plists with :freq and :title) by freq desc, title asc."
  (sort cands
        (lambda (a b)
          (let ((fa (plist-get a :freq))
                (fb (plist-get b :freq)))
            (if (/= fa fb)
                (> fa fb)
              (string-lessp (or (plist-get a :title) "")
                            (or (plist-get b :title) "")))))))

(defun org-story--title->ids (cands)
  "From candidate plists, return (ORDER . TABLE) where TABLE maps title -> list of UUIDs.
ORDER preserves the order of first appearance in CANDS."
  (let ((tbl (make-hash-table :test 'equal))
        (order '()))
    (dolist (c cands)
      (let ((title (or (plist-get c :title) "Untitled"))
            (id (plist-get c :uuid)))
        (unless (gethash title tbl)
          (puthash title '() tbl)
          (push title order))
        (puthash title (append (gethash title tbl) (list id)) tbl)))
    (cons (nreverse order) tbl)))

(defun org-story--collect-candidates (id-links)
  "Return plist (:characters CHARS :locations LOCS) for recognized candidates.
Each candidate is (:uuid ID :title TITLE :freq N), sorted by freq desc then title.
If no recognized dirs are present or both groups empty, return nil (fallback)."
  (let ((root (org-story--project-root)))
    (when root
      (let* ((char-dir (expand-file-name "characters/" root))
             (loc-dir  (expand-file-name "locations/"  root))
             (any-dir-present (or (file-directory-p char-dir)
                                  (file-directory-p loc-dir)))
             (uuids (mapcar (lambda (e) (plist-get e :uuid)) id-links))
             (uuid->title (let ((h (make-hash-table :test 'equal)))
                            (dolist (e id-links)
                              (puthash (plist-get e :uuid)
                                       (plist-get e :title) h))
                            h))
             (uuid->file (org-story--uuid-file-map uuids))
             (counts (org-story--id-frequency-hash))
             (chars '())
             (locs '()))
        (dolist (id uuids)
          (let* ((file (gethash id uuid->file))
                 (title (gethash id uuid->title))
                 (freq (gethash id counts 0)))
            (when (and file (file-name-absolute-p file))
              (let ((afile (file-truename file)))
                (cond
                 ((and (file-directory-p char-dir)
                       (string-prefix-p (file-truename char-dir) afile))
                  (push (list :uuid id :title title :freq freq) chars))
                 ((and (file-directory-p loc-dir)
                       (string-prefix-p (file-truename loc-dir) afile))
                  (push (list :uuid id :title title :freq freq) locs)))))))
        (setq chars (org-story--sort-candidates chars))
        (setq locs (org-story--sort-candidates locs))
        (if (or (not any-dir-present)
                (and (null chars) (null locs)))
            nil
          (list :characters chars :locations locs))))))

(defun org-story--read-grouped (chars locs)
  "Two-phase UI with simple defaults:
- Empty RET at the first prompt of a group selects ALL in that group (OR).
- \"[Skip]\" lets you skip a group.
- No operator prompts; group-internal operator is 'or; between groups is `org-story-groups-operator-default`.
Returns (LIST 'grouped) where LIST is a plist:
  :chars IDS :locs IDS :char-op 'or :loc-op 'or :groups-op OP."
  (let* ((char-tuples (org-story--title->ids chars))
         (loc-tuples  (org-story--title->ids locs))
         (char-titles (car char-tuples))
         (loc-titles  (car loc-tuples))
         (char-map (cdr char-tuples))
         (loc-map  (cdr loc-tuples))
         (all-char-ids (apply #'append (mapcar (lambda (t) (gethash t char-map)) char-titles)))
         (all-loc-ids  (apply #'append (mapcar (lambda (t) (gethash t loc-map))  loc-titles)))
         (char-selected '())
         (loc-selected '()))
    ;; Characters phase
    (when char-titles
      (let ((first t) done choice)
        (while (not done)
          (setq choice (completing-read
                        (if first
                            "Character (RET=ALL, [Skip]=skip, or pick one): "
                          "Character (RET=finish): ")
                        (append char-titles '("ALL" "[Skip]")) nil nil))
          (cond
           ((and first (string= choice "")) (setq char-selected all-char-ids done t)) ; first empty -> ALL
           ((string= choice "") (setq done t))                                        ; finish
           ((string= choice "[Skip]") (setq done t))                                  ; skip group
           ((string= choice "ALL") (setq char-selected all-char-ids done t))
           (t
            (setq first nil)
            (setq char-selected (append char-selected (gethash choice char-map)))
            (setq char-titles (delete choice char-titles)))))))
    ;; Locations phase
    (when loc-titles
      (let ((first t) done choice)
        (while (not done)
          (setq choice (completing-read
                        (if first
                            "Location (RET=ALL, [Skip]=skip, or pick one): "
                          "Location (RET=finish): ")
                        (append loc-titles '("ALL" "[Skip]")) nil nil))
          (cond
           ((and first (string= choice "")) (setq loc-selected all-loc-ids done t))   ; first empty -> ALL
           ((string= choice "") (setq done t))                                        ; finish
           ((string= choice "[Skip]") (setq done t))                                  ; skip group
           ((string= choice "ALL") (setq loc-selected all-loc-ids done t))
           (t
            (setq first nil)
            (setq loc-selected (append loc-selected (gethash choice loc-map)))
            (setq loc-titles (delete choice loc-titles)))))))
    (let ((sel (list :chars (delete-dups char-selected)
                     :locs  (delete-dups loc-selected)
                     :char-op 'or :loc-op 'or
                     :groups-op org-story-groups-operator-default)))
      (if (and (null (plist-get sel :chars))
               (null (plist-get sel :locs)))
          nil
        (list sel 'grouped)))))

;;;----------------------------------------------------------------------
;;; Section 3: EXTRACTION AND CLEANUP OF FILTERED HEADINGS
;;;----------------------------------------------------------------------

(defun org-get-filtered-headings-from-buffer (ids operator)
  "Return a list of cons cells (HEADING-TEXT . MARKER) from the current buffer.
Only headings matching the UUIDs in IDS (tested with OPERATOR—either 'and or 'or) are included.
Each cons cell's cdr is a copy of the marker pointing to the start of the heading."
  (let ((headings '())
	  (id-regexps (mapcar (lambda (id)
				(regexp-quote (format "id:%s" id)))
			      ids)))
    (save-excursion
	(goto-char (point-min))
	(while (re-search-forward org-heading-regexp nil t)
	  (let ((start (line-beginning-position))
		(end (progn (outline-next-heading) (point))))
	    (let* ((heading-text (buffer-substring-no-properties start end))
		   (matches (mapcar (lambda (regexp)
				      (string-match-p regexp heading-text))
				    id-regexps)))
	      (when (or (and (eq operator 'and) (cl-every #'identity matches))
			(and (eq operator 'or)  (cl-some #'identity matches)))
		(push (cons heading-text (copy-marker start)) headings))))))
    (nreverse headings)))

(defun org-clean-heading-text (raw)
  "Clean the heading RAW and return a single line with normalized spacing.
For each line, remove leading asterisks, any priority marker (of the form [#<number>]),
scheduling lines (e.g. 'SCHEDULED: …'), inline links (e.g. [[...][Description]]),
and property drawers (from :PROPERTIES: to :END:).
All leading and trailing whitespace is removed before the cleaned lines
are concatenated with exactly one space between them."
  (let* ((raw (replace-regexp-in-string ":PROPERTIES:\\(?:.\\|\n\\)*?:END:" "" raw))
	   (lines (split-string raw "\n" t))
	   (clean-lines
	    (mapcar (lambda (line)
		      (let ((line (replace-regexp-in-string "^\\*+[ \t]*" "" line)))
			(setq line (replace-regexp-in-string "\\[#[0-9]+\\][ \t]*" "" line))
			(setq line (replace-regexp-in-string "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1" line))
			(setq line (replace-regexp-in-string "SCHEDULED:[^\n]*" "" line))
			(string-trim line)))
		    lines))
	   (paragraph (string-join clean-lines " ")))
    (string-trim (replace-regexp-in-string "[ \t]+" " " paragraph))))

;;;----------------------------------------------------------------------
;;; Section 4: DISPLAYING FILTERED HEADINGS AS A NARRATIVE PLOT
;;;----------------------------------------------------------------------

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
                                               mouse-face highlight
                                               help-echo "RET or click: Jump to original heading"))
            (when (< i n)
              (insert " ")))))
      (goto-char (point-min))
      (org-filtered-heading-mode))
    (delete-other-windows)
    (let* ((total (window-total-height (selected-window)))
           (top-height (max 5 (floor (* total 0.382)))))
      (let ((top-window (split-window (selected-window) top-height 'above)))
        (set-window-buffer top-window orig-buf)))
    (other-window 1)
    (switch-to-buffer plot-buf)
    plot-buf))

(defvar org-filtered-heading-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'org-filtered-heading-open-original)
    (define-key map [mouse-1] 'org-filtered-heading-open-original)
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
  (let ((marker (get-text-property (point) 'orig-marker)))
    (if (and marker 
	       (or (buffer-live-p (marker-buffer marker))
		   (and (marker-buffer marker)
			(buffer-file-name (marker-buffer marker)))))
	  (progn
	    (pop-to-buffer (or (marker-buffer marker)
			       (find-file-noselect (buffer-file-name (marker-buffer marker)))))
	    (goto-char marker)
	    (recenter))
	(message "Original location not available."))))

;;;----------------------------------------------------------------------
;;; Section 5: INTERACTIVE PLOT VIEW COMMAND
;;;----------------------------------------------------------------------

(defvar org-story--last-selected-ids nil
  "Stores the last selected narrative element IDs used in `org-story-show-plot`.")

;;;###autoload
(defun org-story-show-plot (arg)
  "Interactively select narrative elements and display their narrative plot.
With prefix ARG (C-u), force a fresh scan (ignore cache)."
  (interactive "P")
  (let* ((id-links (org-get-id-links-in-buffer-cached arg))
         (grouped (org-story--collect-candidates id-links)))
    (if grouped
        (let* ((chars (plist-get grouped :characters))
               (locs  (plist-get grouped :locations))
               (res   (org-story--read-grouped chars locs)))
          (if res
              (org-filter-headings-by-ids (nth 0 res) (nth 1 res))
            (message "No narrative elements selected.")))
      ;; Fallback: legacy flow
      (let* ((titles (delete-dups (mapcar (lambda (e) (plist-get e :title)) id-links)))
             (selected-ids nil)
             (first-prompt t)
             (continue t))
        (while (and continue titles)
          (let* ((prompt (if (and first-prompt org-story--last-selected-ids)
                             "Narrative element (RET to reuse previous selection): "
                           "Narrative element (RET to finish): "))
                 ;; Allow empty input -> finish/reuse
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
            (let ((operator (if (> (length selected-ids) 1)
                                (intern (completing-read "Operator (and/or): " '("and" "or") nil t))
                              'or)))
              (setq org-story--last-selected-ids selected-ids)
              (org-filter-headings-by-ids selected-ids operator))
          (message "No narrative elements selected."))))))

(defun org-get-filtered-headings-from-buffer-grouped (sel)
  "Filter headings with group-aware semantics.
SEL plist: :chars IDS :locs IDS :char-op OP :loc-op OP :groups-op OP.
- Within a group: OP is 'and or 'or (default 'or).
- Between groups: :groups-op (default `org-story-groups-operator-default`)."
  (let* ((char-ids (plist-get sel :chars))
         (loc-ids  (plist-get sel :locs))
         (char-op  (or (plist-get sel :char-op) 'or))
         (loc-op   (or (plist-get sel :loc-op)  'or))
         (groups-op (or (plist-get sel :groups-op) org-story-groups-operator-default))
         (char-rx (mapcar (lambda (id) (regexp-quote (format "id:%s" id))) char-ids))
         (loc-rx  (mapcar (lambda (id) (regexp-quote (format "id:%s" id)))  loc-ids))
         (headings '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-heading-regexp nil t)
        (let ((start (line-beginning-position))
              (end (progn (outline-next-heading) (point))))
          (let* ((txt (buffer-substring-no-properties start end))
                 (char-matches (mapcar (lambda (rx) (string-match-p rx txt)) char-rx))
                 (loc-matches  (mapcar (lambda (rx) (string-match-p rx txt))  loc-rx))
                 (char-ok (and char-ids
                               (if (eq char-op 'and)
                                   (cl-every #'identity char-matches)
                                 (cl-some  #'identity char-matches))))
                 (loc-ok  (and loc-ids
                               (if (eq loc-op 'and)
                                   (cl-every #'identity loc-matches)
                                 (cl-some  #'identity loc-matches))))
                 (ok (cond
                      ((and char-ids loc-ids)
                       (if (eq groups-op 'and) (and char-ok loc-ok) (or char-ok loc-ok)))
                      (char-ids char-ok)
                      (loc-ids  loc-ok)
                      (t nil))))
            (when ok
              (push (cons txt (copy-marker start)) headings))))))
    (nreverse headings)))

(defun org-filter-headings-by-ids (ids operator)
  "Filter headings by UUIDs (legacy) or a grouped selection plist."
  (let ((headings
         (cond
          ((or (eq operator 'grouped)
               (and (listp ids)
                    (or (memq :chars ids) (memq :locs ids))))
           (org-get-filtered-headings-from-buffer-grouped ids))
          (t
           (org-get-filtered-headings-from-buffer ids operator)))))
    (if headings
        (org-display-filtered-headings-buffer headings ids operator (current-buffer))
      (message "No matching headings found."))))

;;;----------------------------------------------------------------------
;;; Section 6: KEY BINDINGS AND PROVIDE
;;;----------------------------------------------------------------------

(global-set-key (kbd "C-c s p") 'org-story-show-plot)

(provide 'org-story)
;;; org-story.el ends here
