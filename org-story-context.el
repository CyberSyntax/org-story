;;; org-story-context.el --- Shared context/state helpers for org-story -*- lexical-binding: t; -*-
;;; Commentary:
;; Focus/marker/selection state and plot buffer context resolution.
;;; Code:

(require 'org)
(require 'subr-x)

(require 'org-story-custom)
(require 'org-story-clean)

;; -------------------------------
;; Shared runtime state
;; -------------------------------

(defvar org-story--last-focus-marker nil
  "Marker of the heading used as Focus at call-site.")

(defvar org-story--last-selected-ids nil
  "Stores the last selected narrative element IDs used in `org-story-show-plot`.")

(defvar org-story--last-focus-sentence nil
  "Cleaned heading line captured at org-story-show-plot call-site.")

;; -------------------------------
;; Helpers
;; -------------------------------

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

;; -------------------------------
;; Context helpers
;; -------------------------------

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

(provide 'org-story-context)
;;; org-story-context.el ends here
