;;; org-story-cache.el --- Caching of Org-roam ID links -*- lexical-binding: t; -*-
;;; Commentary:
;; Per-file, per-day cache of id: links discovered in buffers.
;;; Code:

(require 'org)
(require 'org-element)
(require 'org-roam)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'org-story-core)
(require 'org-story-custom)

;; Initialize cache dir robustly (org-roam-directory might not be a string).
(let* ((base (and (boundp 'org-roam-directory)
                  (stringp org-roam-directory)
                  (file-name-as-directory org-roam-directory)))
       (dir  (or org-roam-id-links-cache-directory
                 (expand-file-name "org-roam-id-links-cache"
                                   (or base user-emacs-directory)))))
  (setq org-roam-id-links-cache-directory dir)
  (when (and (stringp org-roam-id-links-cache-directory)
             (not (file-directory-p org-roam-id-links-cache-directory)))
    (make-directory org-roam-id-links-cache-directory t)))

;; -------------------------------
;; Functions
;; -------------------------------

(defun org-get-id-links-in-buffer ()
  "Return a list of plists (:uuid UUID :title TITLE)."
  (let* ((uuids (delete-dups
                 (org-element-map (org-element-parse-buffer) 'link
                   (lambda (lnk)
                     (when (and (string= (org-element-property :type lnk) "id")
                                (org-element-property :path lnk))
                       (org-element-property :path lnk))))))
         (vec   (vconcat uuids))
         (rows  (ignore-errors
                  (org-roam-db-query
                   [:select [id title] :from nodes :where id :in $v1]
                   vec)))
         (tbl   (let ((h (make-hash-table :test 'equal)))
                  (dolist (r rows) (puthash (car r) (cadr r) h)) h)))
    (mapcar (lambda (u)
              (let ((title (gethash u tbl "Untitled")))
		(list :uuid u
                      :title (if (string= title "Untitled")
				 (format "Untitled (%s)" u)
                               title))))
            uuids)))

(defun org-save-id-links-cache (cache-file result)
  "Save RESULT (a list of ID links) to CACHE-FILE with today's date stamp."
  (with-temp-file cache-file
    (let ((today (format-time-string "%Y-%m-%d")))
    (insert (prin1-to-string (list :date today :result result))))))

(defun org-load-id-links-cache (cache-file)
  "Load cached ID links from CACHE-FILE.
If the saved date matches today, return the cached result; otherwise, return nil."
  (when (file-exists-p cache-file)
    (condition-case nil
        (let* ((data (with-temp-buffer
                       (insert-file-contents cache-file)
                       (read (buffer-string))))
               (saved-date (plist-get data :date))
               (result (plist-get data :result))
               (today (format-time-string "%Y-%m-%d")))
          (and (string= saved-date today) result))
      (error nil))))

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
      (let* ((key (file-truename bf))
             (cache-file (expand-file-name
                          (format "%s-%s.id-links.cache"
                                  (file-name-nondirectory bf)
                                  (substring (md5 key) 0 8))
                          org-roam-id-links-cache-directory))
             (cached-result (org-load-id-links-cache cache-file)))
        (if cached-result
            cached-result
          (let ((result (org-get-id-links-in-buffer)))
            (org-save-id-links-cache cache-file result)
            result)))))))

(provide 'org-story-cache)
;;; org-story-cache.el ends here
