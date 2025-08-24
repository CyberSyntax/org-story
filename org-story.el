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

(eval-when-compile
  (declare-function gptel-mode "gptel")
  (declare-function gptel-send "gptel"))

(defgroup org-story nil
  "org-story + gptel integration."
  :group 'applications)

(defcustom org-story-use-tab-bar t
  "If non-nil, ensure tab-bar-mode is enabled and open gptel in a new tab."
  :type 'boolean
  :group 'org-story)

(defcustom org-story-auto-return-from-gptel t
  "If non-nil, switch back to the previous tab right after gptel-send."
  :type 'boolean
  :group 'org-story)

(defcustom org-story-groups-operator-default 'or
  "Default operator to combine groups when both Characters and Locations are used.
Allowed values: 'and or 'or."
  :type '(choice (const and) (const or))
  :group 'org-story)

(defcustom org-story-default-op "AUTO"
  "Default operation for envelope INPUT."
  :type 'string
  :group 'org-story)

(defcustom org-story-prompts-directory nil
  "Directory containing prompt files.
If nil, defaults to a 'prompts/' directory next to org-story.el."
  :type '(choice (const :tag "Auto (next to org-story.el)" nil)
                 (directory :tag "Directory"))
  :group 'org-story)

(defcustom org-story-template-variables nil
  "Template variables as an alist of (NAME . VALUE).
Used by prompt templating. Example:
  '((\"PROJECT\" . \"MySaga\") (\"FOCUS_KEY\" . \"FOCUS_SENTENCE\"))"
  :type '(alist :key-type string :value-type string)
  :group 'org-story)

(defcustom org-story-template-max-subst-passes 6
  "Maximum fixpoint passes for variable substitution."
  :type 'integer
  :group 'org-story)

(defcustom org-story-template-max-include-depth 8
  "Maximum include recursion depth for prompt compilation."
  :type 'integer
  :group 'org-story)

(defcustom org-story-always-prompt-template nil
  "If non-nil, always show prompt picker even when only one template exists."
  :type 'boolean
  :group 'org-story)

(defcustom org-story-plot-joiner " "
  "Separator between headings in the Narrative Plot buffer (e.g., \" \" or \"\\n\")."
  :type 'string
  :group 'org-story)

(defcustom org-story-include-focus-meta 'never
  "Whether to include FOCUS_META line in the INPUT block.
- 'never: never include.
- 'auto: include only if Focus is not uniquely found in Plot or disambiguation likely needed.
- 'always: always include."
  :type '(choice (const never) (const auto) (const always))
  :group 'org-story)

(defcustom org-story-append-reply-directive t
  "If non-nil, append a reply directive block to the user envelope
so the model outputs exactly one template block (no meta/echo)."
  :type 'boolean :group 'org-story)

(defcustom org-story-append-format-tail t
  "If non-nil, append a strict format template tail to the user envelope.
This strongly anchors the exact output shape via recency."
  :type 'boolean :group 'org-story)

;; Envelope constants
(defconst org-story-envelope-header
  "Narrative Causal Edit Protocol (Quoted-Identifiers; Plot/Input Envelope)"
  "Header for the envelope.")

(defconst org-story-envelope-begin-plot
  "::BEGIN PLOT"
  "Begin plot marker.")

(defconst org-story-envelope-end-plot
  "::END PLOT"
  "End plot marker.")

(defconst org-story-envelope-begin-input
  "::BEGIN INPUT"
  "Begin input marker.")

(defconst org-story-envelope-end-input
  "::END INPUT"
  "End input marker.")

(defconst org-story--reply-directive
  "::BEGIN REPLY
OUTPUT: exactly one block from [COMPRESS|SPLIT|ADD|IMPROVE].
NO_ECHO: do not echo PLOT or INPUT.
NO_META: do not print FOCUS_META or any field labels.
FORMAT: start with '[' and end with ']'; no commentary.
::END REPLY")

(defconst org-story--format-tail
"::BEGIN FORMAT
You must output exactly one bracketed block. Start with '[' and end with ']'. No commentary.
Do not echo PLOT or INPUT. Do not print FOCUS_META. Do not use pronouns (he/she/they/it/this/that/these/those) in Sentence/Focus’ lines; use named actors only.
Choose exactly one of the four blocks below and follow field names/order exactly.

[COMPRESS
Focus: \"…\"
Keep: \"…\" | new
Drop: \"…\"
Prev (SG): \"…\"
Sentence (Keep/new): {one action-only; outcome-free; names}
Antecedents (CG): ← \"…\"; \"…\"
Consequents (CG): → \"…\"; \"…\"
Links: update {causes|enables|resolves|foreshadows|mirrors|parallels}; replace/remove contradicts
Density: assert Δδ ≥ 0
Justify: {REDUNDANCY_ELIMINATION|SUBSUMPTION|NECESSITY_UPGRADE|PAYOFF_TIGHTEN}
]

[SPLIT
Focus: \"…\"
Focus’ (trimmed): {one action-only; outcome-free; names}
Compensatory New (print - NewN for K chosen by rule: K* = argmax_{k∈{1..min(3,CLI)}} S(k) s.t. Δδ(k)≥0; allow k>3 only if S(k)−S(k−1)≥2 and Δδ(k)>Δδ(k−1); ties→smallest k):
- New1
  Prev (SG): \"…\"
  Sentence: {one action-only; outcome-free; names}
  Antecedents (CG): ← \"…\"; \"…\"
  Consequents (CG): → \"…\"; \"…\"
  FocusLink: Focus —{causes|enables|resolves|foreshadows|mirrors|parallels}→ New1 | New1 → Focus
- NewN
  Prev (SG): \"…\"
  Sentence: {one action-only; outcome-free; names}
  Antecedents (CG): ← \"…\"; \"…\"
  Consequents (CG): → \"…\"; \"…\"
  FocusLink: …
Density: assert Δδ ≥ 0; preserve or resolve hooks
Justify: {ATOMICITY_FIX|CAUSAL_EXPLICITATION|PAYOFF_STAGING|GLOBAL_WEAVE|LOSS_COMPENSATION}
]

[ADD
Focus: \"…\" | <provisional if focus sentence is new>
Prev (SG): \"…\"
NewID: new
Sentence: {one action-only; outcome-free; names}
Antecedents (CG): ← \"…\"; \"…\"
Consequents (CG): → \"…\"; \"…\"
Prev→New (CG): {causes|enables|mirrors|parallels}
New→(implicit Next) (CG): {causes|enables|resolves|foreshadows}
FocusLink: Focus —{causes|enables|resolves|foreshadows|mirrors|parallels}→ New | New → Focus
Threads/Promises: resolves \"…\" or foreshadows \"…\"
Density: assert Δδ ≥ 0
Justify: {RESOLVE|FORESHADOW|NECESSITY_UPGRADE|ECHO_BINDING}
]

[IMPROVE
Focus: \"…\"
Sentence (replacement): {one action-only; outcome-free; names; sharpened verb/parameters}
Antecedents (CG): ← \"…\"; \"…\"
Consequents (CG): → \"…\"; \"…\"
Links: add/retag {causes|enables|resolves|foreshadows|mirrors|parallels}; eliminate contradicts
Loss-Compensation New (if implicit content removed):
- New1
  Prev (SG): \"…\"
  Sentence: {one action-only; outcome-free; names}
  Antecedents (CG): ← \"…\"
  Consequents (CG): → \"…\"
Density: assert Δδ ≥ 0; no hook loss
Justify: {PRECISION|PAYOFF_TIGHTEN|FORESHADOW_TIGHTEN|LOSS_COMPENSATION}
]
::END FORMAT")

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
	    (let* ((block-text (buffer-substring-no-properties start end))
		   (matches (mapcar (lambda (regexp)
				      (string-match-p regexp block-text))
				    id-regexps)))
	      (when (or (and (eq operator 'and) (cl-every #'identity matches))
			(and (eq operator 'or)  (cl-some #'identity matches)))
                (let ((line-text (save-excursion 
                                   (goto-char start) 
                                   (org-get-heading t t t t))))
                  (push (cons line-text (copy-marker start)) headings)))))))
    (nreverse headings)))

(defun org-clean-heading-text (raw)
  "Clean heading RAW to a single plain line.
- Remove stars, priorities, SCHEDULED:, property drawers.
- Strip Org links: [[…][Desc]] -> Desc; [[…]] -> removed.
- Normalize whitespace to single spaces."
  (let* ((raw (replace-regexp-in-string ":PROPERTIES:\\(?:.\\|\n\\)*?:END:" "" raw))
         (lines (split-string raw "\n" t))
         (clean-lines
          (mapcar (lambda (line)
                    (let ((line (replace-regexp-in-string "^\\*+[ \t]*" "" line)))
                      (setq line (replace-regexp-in-string "\\[#[A-Z0-9]+\\][ \t]*" "" line))
                      ;; [[link][Desc]] -> Desc
                      (setq line (replace-regexp-in-string "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1" line))
                      ;; [[link]] -> delete
                      (setq line (replace-regexp-in-string "\\[\\[[^]]+\\]\\]" "" line))
                      (setq line (replace-regexp-in-string "SCHEDULED:[^\n]*" "" line))
                      (string-trim line)))
                  lines))
         (paragraph (string-join clean-lines " ")))
    (string-trim (replace-regexp-in-string "[ \t]+" " " paragraph))))

(defun org-story--ensure-plain (s)
  "Strip Org links and normalize spaces in S."
  (let ((s (or s "")))
    (setq s (replace-regexp-in-string "\\[\\[.*?\\]\\[\\(.*?\\)\\]\\]" "\\1" s))
    (setq s (replace-regexp-in-string "\\[\\[[^]]+\\]\\]" "" s))
    (string-trim (replace-regexp-in-string "[ \t]+" " " s))))

;;;----------------------------------------------------------------------
;;; Section 4: DISPLAYING FILTERED HEADINGS AS A NARRATIVE PLOT
;;;----------------------------------------------------------------------

(defvar org-story--last-focus-marker nil
  "Marker of the heading used as Focus at call-site.")

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

(defvar org-story--last-focus-sentence nil
  "Cleaned heading line captured at org-story-show-plot call-site.")

(defun org-story--heading-line-at-point ()
  "Return cleaned heading at point or nearest above; nil if none."
  (save-restriction
    (widen)
    (save-excursion
      (condition-case nil
          (progn
            (unless (org-at-heading-p)
              (org-back-to-heading t))
            (let ((h (org-get-heading t t t t)))
              (when (and h (stringp h) (not (string-empty-p h)))
                (string-trim (org-clean-heading-text h)))))
        (error nil)))))

(defun org-story--marker-at-point ()
  "Return a marker at the current heading position, or nil if none."
  (save-excursion
    (condition-case nil
        (progn
          (unless (org-at-heading-p)
            (org-back-to-heading t))
          (copy-marker (line-beginning-position)))
      (error nil))))

;;;###autoload
(defun org-story-show-plot (&optional arg)
  "Interactively select narrative elements and display their narrative plot.
With prefix ARG (C-u), force a fresh scan (ignore cache)."
  (interactive "P")
  (setq org-story--last-focus-sentence
        (or (org-story--heading-line-at-point)
            org-story--last-focus-sentence))
  (setq org-story--last-focus-marker
        (org-story--marker-at-point))  ; Capture marker at call-site
  (let* ((id-links (org-get-id-links-in-buffer-cached arg))
         (grouped (org-story--collect-candidates id-links))
         (plot-buf nil))
    (if (null id-links)
        (message "No ID links found in buffer. Try C-u to refresh.")
      (if grouped
          (let* ((chars (plist-get grouped :characters))
                 (locs  (plist-get grouped :locations))
                 (res   (org-story--read-grouped chars locs)))
            (if res
                (setq plot-buf (org-filter-headings-by-ids (nth 0 res) (nth 1 res)))
              (message "No narrative elements selected. Try selecting at least one.")))
        ;; Fallback: legacy flow
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
                (setq plot-buf (org-filter-headings-by-ids selected-ids operator)))
            (message "No narrative elements selected. Try picking one or use C-u to refresh.")))))
    (when (and plot-buf (featurep 'gptel))
      (condition-case err
          (org-story--post-plot-dispatch)
        (user-error (message "GPTel dispatch skipped: %s" (error-message-string err)))))))

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
          (let* ((block-text (buffer-substring-no-properties start end))
                 (char-matches (mapcar (lambda (rx) (string-match-p rx block-text)) char-rx))
                 (loc-matches  (mapcar (lambda (rx) (string-match-p rx block-text))  loc-rx))
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
              (let ((line-text (save-excursion 
                                 (goto-char start) 
                                 (org-get-heading t t t t))))
                (push (cons line-text (copy-marker start)) headings)))))))
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
      (progn
        (message "No matching headings found. Check IDs or try C-u to refresh cache.")
        nil))))

;;;----------------------------------------------------------------------
;;; Section 6: GPTEL INTEGRATION AND ENVELOPE
;;;----------------------------------------------------------------------

;; -------------------------------
;; Prompt, composition, and payload plumbing (extensible)
;; -------------------------------

(defcustom org-story-prompt-extensions '("poet" "yaml" "yml" "txt")
  "Filename extensions to scan in prompts/ directory, ordered by preference.
Earlier entries have higher precedence when duplicate basenames exist."
  :type '(repeat string)
  :group 'org-story)

(defcustom org-story-message-mode 'user-envelope
  "How to send content to gptel:
- 'user-envelope: System = prompt template, User = serialized data.
- 'system-only:  System = template + serialized data, User = stub."
  :type '(choice (const user-envelope) (const system-only))
  :group 'org-story)

(defcustom org-story-system-only-user-stub "."
  "User stub string when `org-story-message-mode' is 'system-only'.
Some providers reject empty user content; keep at least one visible char."
  :type 'string
  :group 'org-story)

(defcustom org-story-serialization-format 'envelope
  "Serialization for narrative data:
- 'envelope: use org-story-compose-envelope (BEGIN/END markers).
- 'yaml:     minimal YAML: plot + input mapping.
- 'none:     minimal inline text, no framing."
  :type '(choice (const envelope) (const yaml) (const none))
  :group 'org-story)

(defun org-story--indent (s n)
  "Indent every line in string S by N spaces."
  (let* ((pad (make-string n ?\s))
         (ls (split-string (or s "") "\n")))
    (mapconcat (lambda (l) (concat pad l)) ls "\n")))

(defun org-story-compose-data (ctx &optional prev op)
  "Serialize CTX per `org-story-serialization-format`.
Returns a string for the user message (or to append into system in system-only mode)."
  (let* ((focus (org-story--resolve-focus ctx))
         (prev  (or prev "<omitted>"))
         (op    (or op org-story-default-op))
         (plot  (org-story--plot-raw-text ctx))
         (fmt   (or (and (boundp 'org-story-serialization-format)
                         org-story-serialization-format)
                    'envelope)))
    (pcase fmt
      ('envelope
       ;; Preserve the existing envelope (with ::BEGIN markers and header).
       (org-story-compose-envelope ctx prev op))
      ('yaml
       (format "plot: |-\n%s\n\ninput:\n  focus_sentence: %S\n  prev: %S\n  op: %S\n"
               (org-story--indent plot 2) focus prev op))
      ('none
       plot)
      (_
       (org-story-compose-envelope ctx prev op)))))

(defun org-story--template-vars ()
  "Return default template variable env merged with user vars."
  (let ((defaults `(("ENVELOPE_HEADER" . ,org-story-envelope-header)
                    ("BEGIN_PLOT"     . ,org-story-envelope-begin-plot)
                    ("END_PLOT"       . ,org-story-envelope-end-plot)
                    ("BEGIN_INPUT"    . ,org-story-envelope-begin-input)
                    ("END_INPUT"      . ,org-story-envelope-end-input)
                    ("FOCUS_KEY"      . "FOCUS_SENTENCE")
                    ("FOCUS_META_KEY" . "FOCUS_META")
                    ("PREV_KEY"       . "Prev")
                    ("OP_KEY"         . "Op"))))
    (let ((tbl (copy-sequence defaults)))
      (dolist (kv org-story-template-variables)
        (let ((k (car kv)) (v (cdr kv)))
          (setq tbl (assq-delete-all k tbl))
          (push (cons k v) tbl)))
      (nreverse tbl))))

(defun org-story--subst-vars (text var-alist &optional max-passes)
  "Substitute ${VAR} and {{VAR}} until fixpoint or MAX-PASSES."
  (let* ((passes (or max-passes org-story-template-max-subst-passes))
         (i 0) (changed t))
    (while (and changed (< i passes))
      (setq i (1+ i) changed nil)
      (let ((new text))
        ;; ${VAR}
        (setq new
              (replace-regexp-in-string
               "\\${\\([A-Za-z0-9_]+\\)}"
               (lambda (m)
                 (let* ((var (match-string 1))
                        (val (cdr (assoc var var-alist))))
                   (if val (progn (setq changed t) val) m)))
               new t t))
        ;; {{VAR}} (include 아님)
        (setq new
              (replace-regexp-in-string
               "{{\\([A-Za-z0-9_]+\\)}}"
               (lambda (m)
                 (let* ((var (match-string 1))
                        (val (cdr (assoc var var-alist))))
                   (if val (progn (setq changed t) val) m)))
               new t t))
        (setq text new)))
    text))

(defun org-story--expand-inline-includes (text name->rec visited depth)
  "Expand <<include:NAME>> and {{include:NAME}} anywhere in TEXT. Cycle-safe."
  (let* ((rx "\\(<<include:[[:space:]]*\\([[:alnum:]_.-]+\\)[[:space:]]*>>\\)\\|\\({{include:[[:space:]]*\\([[:alnum:]_.-]+\\)[[:space:]]*}}\\)")
         (max-depth org-story-template-max-include-depth)
         (changed t) (passes 0))
    (while (and changed (< passes max-depth))
      (setq changed nil passes (1+ passes))
      (let ((start 0) (out ""))
        (while (string-match rx text start)
          (let* ((mb (match-beginning 0))
                 (me (match-end 0))
                 (nm (or (match-string 2 text) (match-string 4 text)))
                 (rec (cdr (assoc nm name->rec)))
                 (repl ""))
            (setq out (concat out (substring text start mb)))
            (when (and rec (not (member (plist-get rec :name) visited)))
              (setq repl (org-story--compile-template rec name->rec
                                                       (cons (plist-get rec :name) visited)
                                                       (1+ depth)))
              (setq changed t))
            (setq out (concat out repl))
            (setq start me)))
        (setq text (concat out (substring text start)))))
    text))

(defun org-story--strip-front-matter (s)
  "Remove leading YAML front-matter blocks (--- ... ---) from S."
  (let ((s (or s "")))
    (while (string-match "\\`---[ \t]*\n\\([^\\0]*?\\)\n---[ \t]*\n" s)
      (setq s (substring s (match-end 0))))
    s))

(defun org-story--squeeze-blank-lines (s)
  "Collapse trailing spaces and 3+ consecutive blank lines."
  (let* ((s (replace-regexp-in-string "[ \t]+\n" "\n" (or s ""))))
    (replace-regexp-in-string "\\(?:\n[ \t]*\n\\)\\{2,\\}" "\n\n" s)))

(defun org-story--compile-template (rec name->rec &optional visited depth)
  "Compile REC by: front-matter includes → inline includes → variable subst."
  (let* ((visited (or visited '()))
         (depth   (or depth 0)))
    (if (>= depth org-story-template-max-include-depth)
        (org-story--strip-front-matter (plist-get rec :raw))
      (let* ((pre (mapconcat
                   (lambda (nm)
                     (let ((irec (cdr (assoc nm name->rec))))
                       (if (and irec (not (member (plist-get irec :name) visited)))
                           (org-story--compile-template irec name->rec
                                                        (cons (plist-get irec :name) visited)
                                                        (1+ depth))
                         "")))
                   (or (plist-get rec :includes) '())
                   "\n\n"))
             (raw (org-story--strip-front-matter (plist-get rec :raw)))
             (text (string-trim (if (string-empty-p pre) raw (concat pre "\n\n" raw))))
             (text* (org-story--expand-inline-includes text name->rec visited depth))
             (vars (org-story--template-vars))
             (out  (org-story--subst-vars text* vars org-story-template-max-subst-passes)))
        (org-story--squeeze-blank-lines out)))))

(defun org-story--parse-prompt-record (rec name->rec)
  "Compose final system template by compiling REC (includes + vars)."
  (let* ((compiled (org-story--compile-template rec name->rec nil 0)))
    (list :system-template compiled
          :user-template   nil)))

(defun org-story-compose-payload (ctx prompt-rec name->rec)
  "Compose final (SYSTEM . USER) pair for sending to gptel."
  (let* ((plan  (org-story--parse-prompt-record prompt-rec name->rec))
         (templ (or (plist-get plan :system-template) ""))
         (data  (org-story-compose-data ctx)))
    (if (eq org-story-message-mode 'system-only)
        (cons (string-trim (concat templ "\n\n" data))
              (or org-story-system-only-user-stub "."))
      (cons templ data))))

(defvar org-story--last-prompt-name nil)

(defun org-story--post-plot-dispatch ()
  "Compose payload and send to gptel.
UI hides basenames starting with '_', but compiler sees the full set."
  (interactive)
  (unless (featurep 'gptel)
    (user-error "gptel not loaded"))
  (let* ((ctx (org-story--plot-context))
         (full (org-story--scan-prompts))
         (visible (seq-filter (lambda (kv) (not (string-prefix-p "_" (car kv))))
                              full)))
    (unless visible
      (user-error "Only include parts found; add at least one main prompt"))
    (let* ((display-map
            (mapcar (lambda (kv)
                      (let* ((name (car kv)) (rec (cdr kv))
                             (title (or (plist-get rec :title) name))
                             (ext   (plist-get rec :ext))
                             (disp  (format "%s — (%s.%s)" title name ext)))
                        (cons disp name)))
                    visible))
           (displays (mapcar #'car display-map))
           (initial (or org-story--last-prompt-name (car displays)))
           (choice-disp
            (if (and (= (length displays) 1)
                     (not org-story-always-prompt-template))
                (car displays)
              (completing-read "Prompt: " displays nil t nil nil initial)))
           (choice (cdr (assoc choice-disp display-map)))
           (rec (cdr (assoc choice visible)))
           (pair (org-story-compose-payload ctx rec full))
           (system (car pair))
           (user   (cdr pair)))
      (setq org-story--last-prompt-name choice-disp)
      (org-story-open-gptel-tab user system))))

(defun org-story--prompts-dir ()
  "Return prompts directory.
Uses `org-story-prompts-directory` if set; otherwise prompts/ next to org-story.el."
  (if (and (boundp 'org-story-prompts-directory)
           org-story-prompts-directory
           (file-directory-p org-story-prompts-directory))
      (file-name-as-directory org-story-prompts-directory)
    (let ((el (locate-library "org-story")))
      (unless el
        (user-error "Cannot locate org-story.el; set `org-story-prompts-directory`"))
      (let ((dir (expand-file-name "prompts/" (file-name-directory el))))
        (unless (file-directory-p dir)
          (user-error "Prompts directory not found: %s" dir))
        (file-name-as-directory dir)))))

(defun org-story--plot-context ()
  "Return plist: :plot-buf :src-buf :ids :op."
  (let ((buf (get-buffer "*Narrative Plot*")))
    (unless (buffer-live-p buf)
      (user-error "*Narrative Plot* buffer not found. Run org-story-show-plot first."))
    (with-current-buffer buf
      (list :plot-buf (current-buffer)
            :src-buf (and (boundp 'org-filter-original-buffer) org-filter-original-buffer)
            :ids     (and (boundp 'org-filter-selected-ids)    org-filter-selected-ids)
            :op      (and (boundp 'org-filter-operator)        org-filter-operator)))))

(defun org-story--plot-raw-text (ctx)
  (with-current-buffer (plist-get ctx :plot-buf)
    (string-trim (buffer-substring-no-properties (point-min) (point-max)))))

(defun org-story--resolve-focus (ctx)
  (let ((f (or
            (and (plist-get ctx :plot-buf)
                 (buffer-live-p (plist-get ctx :plot-buf))
                 (with-current-buffer (plist-get ctx :plot-buf)
                   (and (boundp 'org-plot-focus-sentence) org-plot-focus-sentence)))
            org-story--last-focus-sentence
            (user-error "No Focus captured at call-site. Place cursor on a heading."))))
    (org-story--ensure-plain f)))

(defun org-story--quote (s)
  "Quote S for envelope, escaping internal double quotes."
  (let ((s (or s "")))
    (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"")))

(defun org-story--unique-focus-p (plot focus)
  "Return t if FOCUS occurs exactly once in PLOT."
  (let ((pos 0) (cnt 0))
    (while (setq pos (string-match (regexp-quote focus) plot pos))
      (setq cnt (1+ cnt))
      (setq pos (+ pos (length focus))))
    (= cnt 1)))

(defun org-story-compose-envelope (ctx &optional prev op)
  (let* ((focus (org-story--resolve-focus ctx))
         (prev  (or prev "<omitted>"))
         (op    (or op org-story-default-op))
         (plot  (org-story--plot-raw-text ctx))
         (include-meta
          (pcase org-story-include-focus-meta
            ('never nil)
            ('always t)
            (_ (not (org-story--unique-focus-p plot focus))))) ;; 'auto
         (parts (seq-filter
                 #'identity
                 (list
                  org-story-envelope-header
                  org-story-envelope-begin-plot
                  plot
                  org-story-envelope-end-plot
                  org-story-envelope-begin-input
                  (format "FOCUS_SENTENCE: %s" (org-story--quote focus))
                  (when include-meta
                    "FOCUS_META: TIMELINE=?, TIME=?, LOC=?, ACTORS=?")
                  (format "Prev: %s" (if (string= prev "<omitted>")
                                         "<omitted>" (org-story--quote prev)))
                  (format "Op: %s" op)
                  org-story-envelope-end-input
                  (when org-story-append-reply-directive
                    org-story--reply-directive)
                  (when org-story-append-format-tail
                    org-story--format-tail)))))
    (string-join parts "\n")))

(defvar org-story--gptel-counter 0)

(defun org-story--unique-gptel-buffer ()
  (let* ((stamp (format-time-string "%Y%m%d-%H%M%S"))
         (salt (substring (md5 (format "%s-%S-%s"
                                       stamp (current-time)
                                       (cl-incf org-story--gptel-counter)))
                          0 6))
         (name (format "*gptel org-story %s-%s*" stamp salt))
         (buf  (generate-new-buffer name)))
    (with-current-buffer buf
      (if (fboundp 'markdown-mode) (markdown-mode) (text-mode)))
    buf))

(defun org-story--ensure-tab-bar ()
  "Enable tab-bar-mode if `org-story-use-tab-bar' is non-nil.
Return non-nil if we enabled it here (informational; not used)."
  (when org-story-use-tab-bar
    (unless (bound-and-true-p tab-bar-mode)
      (tab-bar-mode 1)
      t)))

(defun org-story-open-gptel-tab (payload system)
  "Open a new tab (if enabled), prepare a gptel buffer, send, and optionally return.
- SYSTEM is set as the gptel system message (buffer-local).
- PAYLOAD is inserted as the user message content (can be empty).
- If `org-story-auto-return-from-gptel' is non-nil, switch back to the previous tab."
  (interactive)
  (org-story--ensure-tab-bar)
  (when (bound-and-true-p tab-bar-mode)
    (tab-bar-new-tab)
    (tab-bar-rename-tab (format "org-story %s" (format-time-string "%H:%M:%S"))))
  (let* ((buf (org-story--unique-gptel-buffer))
         (msg (or (and (stringp payload) payload)
                  (and (eq org-story-message-mode 'system-only)
                       org-story-system-only-user-stub)
                  "")))
    (switch-to-buffer buf)
    (if (fboundp 'markdown-mode) (markdown-mode) (text-mode))
    (when (fboundp 'gptel-mode) (gptel-mode 1))
    (setq-local gptel-system-message system)
    (when (> (length msg) 0)
      (insert msg)
      (goto-char (point-max)))
    (when (fboundp 'gptel-send)
      (gptel-send))
    (when (and org-story-auto-return-from-gptel
               (bound-and-true-p tab-bar-mode))
      (run-at-time 0 nil #'tab-bar-switch-to-recent-tab))))

(defun org-story--scan-prompts ()
  "Scan prompt files and return ((BASENAME . REC) ...).
REC plists: :name :path :ext :raw, optional :title :desc :includes."
  (let* ((dir (or (ignore-errors (org-story--prompts-dir)) default-directory))
         (allowed (mapcar #'downcase org-story-prompt-extensions))
         (files (and (file-directory-p dir)
                     (directory-files dir t "^[^.].*")))
         (best (make-hash-table :test 'equal)))
    (cl-labels
        ((ext-rank (ext)
           (or (cl-position (downcase (or ext "")) allowed :test #'equal)
               most-positive-fixnum))
         (parse-includes-from-fm (fm)
           (let (out)
             (when (string-match "^includes:[ \t]*\\[\\([^]]*\\)\\]" fm)
               (let* ((inside (match-string 1 fm))
                      (parts (split-string inside "," t "[ \t\n]*")))
                 (setq out (mapcar #'string-trim parts))))
             (when (and (null out)
                        (string-match "^includes:[ \t]*\\(?:$\\|\\(.+\\)\\)" fm))
               (let ((pos (match-end 0)))
                 (while (and pos
                             (string-match "^[ \t]*-[ \t]*\\([^#\n]+\\)" fm pos))
                   (push (string-trim (match-string 1 fm)) out)
                   (setq pos (match-end 0)))
                 (setq out (nreverse out))))
             out))
         (extract-meta (raw)
           (let ((title nil) (desc nil) (incs nil))
             (when (string-match "\\`---[ \t]*\n\\([^\\0]*?\\)\n---[ \t]*\n" raw)
               (let ((fm (match-string 1 raw)))
                 (when (string-match "^title:[ \t]*\\(.*\\)$" fm)
                   (setq title (string-trim (match-string 1 fm))))
                 (when (string-match "^description:[ \t]*\\(.*\\)$" fm)
                   (setq desc (string-trim (match-string 1 fm))))
                 (setq incs (parse-includes-from-fm fm))))
             (unless title
               (when (string-match "^[#; ]*Title:[ \t]*\\(.*\\)$" raw)
                 (setq title (string-trim (match-string 1 raw)))))
             (unless desc
               (when (string-match "^[#; ]*Description:[ \t]*\\(.*\\)$" raw)
                 (setq desc (string-trim (match-string 1 raw)))))
             (list :title title :desc desc :includes incs))))
      (dolist (path files)
        (when (and (file-regular-p path)
                   (member (downcase (or (file-name-extension path) "")) allowed))
          (let* ((ext  (downcase (file-name-extension path)))
                 (name (file-name-base path))
                 (rank (ext-rank ext))
                 (cur  (gethash name best))
                 (cur-rank (plist-get cur :_rank)))
            (when (or (null cur) (< rank cur-rank))
              (puthash name (list :name name :path path :ext ext :_rank rank) best)))))
      (let (alist)
        (maphash
         (lambda (name rec)
           (let* ((raw (with-temp-buffer
                         (insert-file-contents (plist-get rec :path))
                         (buffer-string)))
                  (meta (extract-meta raw))
                  (clean (cl-loop for (k v) on rec by #'cddr
                                  unless (eq k :_rank) append (list k v))))
             (setq clean (plist-put clean :raw raw))
             (when (plist-get meta :title)
               (setq clean (plist-put clean :title (plist-get meta :title))))
             (when (plist-get meta :desc)
               (setq clean (plist-put clean :desc (plist-get meta :desc))))
             (when (plist-get meta :includes)
               (setq clean (plist-put clean :includes (plist-get meta :includes))))
             (push (cons name clean) alist)))
         best)
        (nreverse alist)))))

;;;----------------------------------------------------------------------
;;; Section 7: KEY BINDINGS AND PROVIDE
;;;----------------------------------------------------------------------

(global-set-key (kbd "C-c s p") 'org-story-show-plot)

(provide 'org-story)
;;; org-story.el ends here
