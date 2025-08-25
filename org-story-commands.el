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
