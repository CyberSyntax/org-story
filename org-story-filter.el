;;; org-story-filter.el --- Filter headings by narrative elements -*- lexical-binding: t; -*-
;;; Commentary:
;; Extract/clean headings matching id sets; grouped and legacy semantics.
;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)
(require 'seq)

(require 'org-story-custom)
(require 'org-story-clean)
(require 'org-story-group)
(require 'org-story-display)

;; -------------------------------
;; Filtering
;; -------------------------------

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

;; -------------------------------
;; Grouped filtering
;; -------------------------------

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

(provide 'org-story-filter)
;;; org-story-filter.el ends here
