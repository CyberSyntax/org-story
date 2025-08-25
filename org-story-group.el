;;; org-story-group.el --- Character/Location grouping and selection UI -*- lexical-binding: t; -*-
;;; Commentary:
;; Group candidates from org-roam (characters/locations) and two-phase selection UI.
;;; Code:

(require 'org)
(require 'org-element)
(require 'org-roam)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'org-story-core)
(require 'org-story-custom)
(require 'org-story-cache)
(require 'org-story-context)

;; -------------------------------
;; Character/Location grouping + UI
;; -------------------------------

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
                       (file-in-directory-p afile (file-truename char-dir)))
                  (push (list :uuid id :title title :freq freq) chars))
                 ((and (file-directory-p loc-dir)
                       (file-in-directory-p afile (file-truename loc-dir)))
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
         (all-char-ids (apply #'append (mapcar (lambda (title) (gethash title char-map)) char-titles)))
         (all-loc-ids  (apply #'append (mapcar (lambda (title) (gethash title loc-map))  loc-titles)))
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

(provide 'org-story-group)
;;; org-story-group.el ends here
