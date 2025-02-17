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

;; Enable Org element cache for performance.
(setq org-element-use-cache t)

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
	(expand-file-name "org-roam-id-links-cache" cache-dir)))
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

(defun org-get-id-links-in-buffer-cached ()
  "Return the ID links from the current buffer, using a file cache when possible.
If the buffer is associated with a file, a cache file (named after the file)
is used so that extraction happens at most once per day. Otherwise, call `org-get-id-links-in-buffer`."
  (if (not (buffer-file-name))
      (org-get-id-links-in-buffer)
    (let* ((cache-file (expand-file-name
			(concat (file-name-nondirectory (buffer-file-name))
				".id-links.cache")
			org-roam-id-links-cache-directory))
	   (cached-result (org-load-id-links-cache cache-file)))
      (if cached-result
	  cached-result
	(let ((result (org-get-id-links-in-buffer)))
	  (org-save-id-links-cache cache-file result)
	  result)))))

;;;----------------------------------------------------------------------
;;; Section 2: EXTRACTION AND CLEANUP OF FILTERED HEADINGS
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
scheduling lines (e.g. 'SCHEDULED: …'), and inline links (e.g. [[...][Description]]).
All leading and trailing whitespace is removed before the cleaned lines are concatenated
with exactly one space between them."
  (let* ((lines (split-string raw "\n" t))
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
;;; Section 3: DISPLAYING FILTERED HEADINGS AS A NARRATIVE PLOT
;;;----------------------------------------------------------------------

(defun org-display-filtered-headings-buffer (headings ids operator source-buffer)
  "Display filtered HEADINGS as a narrative plot in a vertically split window.
HEADINGS is a list of cons cells (RAW-HEADING . MARKER). Each heading is cleaned via
`org-clean-heading-text` and concatenated with exactly one space between segments.
Each segment has its original MARKER attached as a text property (so RET or a left-click jumps to it).
After assembling the plot, the current window is split vertically: the top window shows the source buffer,
and the bottom window displays the narrative plot in the buffer \"*Narrative Plot*\".
IDS, OPERATOR, and SOURCE-BUFFER are stored locally for later reuse."
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
    ;; Use the golden ratio for the vertical split.
    (delete-other-windows)
    (let* ((total (window-total-height (selected-window)))
	   (top-height (max 5 (floor (* total 0.382)))))  ; ≈ 38.2% for the source buffer
      (let ((top-window (split-window (selected-window) top-height 'above)))
	(set-window-buffer top-window orig-buf)))
    (other-window 1)
    (switch-to-buffer plot-buf)
    plot-buf)
    (other-window 1))

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
;;; Section 4: INTERACTIVE PLOT VIEW COMMAND
;;;----------------------------------------------------------------------

(defvar org-story--last-selected-ids nil
  "Stores the last selected narrative element IDs used in `org-story-show-plot`.")

;;;###autoload
(defun org-story-show-plot ()
  "Interactively select narrative elements and display their narrative plot.
This command gathers narrative ID links (for characters, places, scenes, etc.) from the current buffer.
For the first prompt, leaving the input blank will reuse the previous selection (if one exists);
otherwise, you enter one or more narrative elements. Subsequent prompts read as 'Narrative element (RET to finish):'.
The corresponding headings are filtered (using an AND/OR operator) and concatenated into a single paragraph.
In the narrative plot view, pressing RET or left-clicking on a segment jumps to its source.
To update the view, simply rerun this command."
  (interactive)
  (let* ((id-links (org-get-id-links-in-buffer-cached))
	 (titles (delete-dups (mapcar (lambda (entry)
					(plist-get entry :title))
				      id-links)))
	 (selected-ids nil)
	 (first-prompt t)
	 (continue t))
    (while (and continue titles)
      (let* ((prompt (if (and first-prompt org-story--last-selected-ids)
			 "Narrative element (RET to reuse previous selection): "
		       "Narrative element (RET to finish): "))
	     (title (completing-read prompt titles nil t)))
	(if (string= title "")
	    (progn
	      (if (and first-prompt org-story--last-selected-ids)
		  (setq selected-ids org-story--last-selected-ids))
	      (setq continue nil))
	  (setq first-prompt nil)
	  (let ((matching-ids (mapcar (lambda (entry)
					(plist-get entry :uuid))
				      (seq-filter (lambda (entry)
						    (string= title (plist-get entry :title)))
						  id-links))))
	    (setq selected-ids (append selected-ids matching-ids))
	    (setq titles (remove title titles))))))
    (if selected-ids
	(let* ((operator (if (> (length selected-ids) 1)
			     (intern (completing-read "Operator (and/or): " '("and" "or") nil t))
			   'or)))
	  (setq org-story--last-selected-ids selected-ids)
	  (org-filter-headings-by-ids selected-ids operator))
      (message "No narrative elements selected."))))

(defun org-filter-headings-by-ids (ids operator)
  "Filter headings by the specific UUIDs (IDS) using OPERATOR (either 'and or 'or).
The matching headings are then displayed as a narrative plot (a single paragraph)
with each segment clickable."
  (let ((headings (org-get-filtered-headings-from-buffer ids operator)))
    (if headings
	(org-display-filter-headings_buffer headings ids operator (current-buffer))
      (message "No matching headings found."))))

(defun org-filter-headings-by-ids (ids operator)
  "Filter headings by the specific UUIDs (IDS) using OPERATOR (either 'and or 'or).
The matching headings are then displayed as a narrative plot (a single paragraph)
with each segment clickable."
  (let ((headings (org-get-filtered-headings-from-buffer ids operator)))
    (if headings
	(org-display-filtered-headings-buffer headings ids operator (current-buffer))
      (message "No matching headings found."))))

;;;----------------------------------------------------------------------
;;; Section 5: KEY BINDINGS AND PROVIDE
;;;----------------------------------------------------------------------

(global-set-key (kbd "C-c s p") 'org-story-show-plot)

(provide 'org-story)
;;; org-story.el ends here
