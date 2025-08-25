;;; org-story.el --- Narrative plot view from Org IDs -*- lexical-binding: t; -*-

;; Author: CyberSyntax
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (org "9.3") (org-roam "2.0") (gptel "0.5"))
;; Keywords: org, narrative, writing
;; URL: https://github.com/CyberSyntax/org-story.git

;;; Commentary:
;; org-story provides narrative plot visualization for Org-mode documents...

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
(require 'org-story-clean)
(require 'org-story-context)
(require 'org-story-group)
(require 'org-story-filter)
(require 'org-story-display)
(require 'org-story-gptel)
(require 'org-story-backlink)
(require 'org-story-commands)

(provide 'org-story)
;;; org-story.el ends here
