;;; org-story-clean.el --- Text cleanup utilities for org-story -*- lexical-binding: t; -*-
;;; Commentary:
;; Heading cleaning and plain-text normalization helpers.
;;; Code:

(require 'org)
(require 'org-element)
(require 'subr-x)

(require 'org-story-custom)

;; -------------------------------
;; Helpers
;; -------------------------------

(defun org-story--strip-links-keep-desc (s)
  "Replace [[link][desc]] with desc and remove [[link]] in S, purely by scanning.
Robust with multiple links on the same line; no org-element required."
  (let* ((s (or s "")) (n (length s)) (i 0))
    (with-output-to-string
      (while (< i n)
        (if (and (< (1+ i) n)
                 (eq (aref s i) ?\[)
                 (eq (aref s (1+ i)) ?\[))
            ;; We are at "[["; find the closing "]]" and optional "]["
            (let* ((path-start (+ i 2))
                   (close (string-match "\\]\\]" s path-start)))
              (if (not close)
                  (progn
                    (princ (char-to-string (aref s i)))
                    (setq i (1+ i)))
                (let ((sep (string-match "\\]\\[" s path-start)))
                  (when (and sep (< sep close))
                    ;; Has description: print it
                    (princ (substring s (+ sep 2) close)))
                  ;; Skip the whole link (bare or described)
                  (setq i (+ close 2)))))
          ;; Normal char
          (princ (char-to-string (aref s i)))
          (setq i (1+ i)))))))

(defun org-clean-heading-text (raw)
  "Clean heading RAW to a single plain line.
- Remove stars, priorities, SCHEDULED:, property drawers.
- Strip Org links: [[…][Desc]] -> Desc; [[…]] -> removed.
- Normalize whitespace to single spaces."
  (let* ((s (or raw "")))
    ;; Remove drawers/SCHEDULED defensively (harmless if absent).
    (setq s (replace-regexp-in-string ":PROPERTIES:\\(?:.\\|\n\\)*?:END:" "" s))
    (setq s (replace-regexp-in-string "SCHEDULED:[^\n]*" "" s))
    ;; Linewise cleanup, then join
    (let* ((lines (split-string s "\n" t))
           (clean-lines
            (mapcar (lambda (line)
                      (setq line (replace-regexp-in-string "^\\*+[ \t]*" "" line))           ; stars
                      (setq line (replace-regexp-in-string "\\[#[A-Z0-9]+\\][ \t]*" "" line)) ; priority
                      (setq line (org-story--strip-links-keep-desc line))                    ; links
                      (string-trim line))
                    lines))
           (paragraph (string-join clean-lines " ")))
      (string-trim (replace-regexp-in-string "[ \t]+" " " paragraph)))))

(defun org-story--ensure-plain (s)
  "Strip Org links and normalize spaces in S."
  (let ((s (or s "")))
    (setq s (org-story--strip-links-keep-desc s))
    (string-trim (replace-regexp-in-string "[ \t]+" " " s))))

(provide 'org-story-clean)
;;; org-story-clean.el ends here
