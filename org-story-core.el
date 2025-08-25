;;; org-story-core.el --- Core setup for org-story -*- lexical-binding: t; -*-
;;; Commentary:
;; Core initializations and compile-time declarations used across modules.
;;; Code:

(require 'org)
(require 'org-element)
(require 'cl-lib)

;; Enable Org element cache for performance.
(setq org-element-use-cache t)

(eval-when-compile
  (declare-function gptel-mode "gptel")
  (declare-function gptel-send "gptel"))

(provide 'org-story-core)
;;; org-story-core.el ends here
