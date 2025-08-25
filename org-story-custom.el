;;; org-story-custom.el --- Customization and constants for org-story -*- lexical-binding: t; -*-
;;; Commentary:
;; All defgroup/defcustoms and envelope constants live here.
;;; Code:

(require 'org)
(require 'cl-lib)
(require 'seq)


;; -------------------------------
;; Customization group and user options
;; -------------------------------

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

(defcustom org-story-source-window-fraction 0.618
  "Fraction of total frame height for the source window when showing the Narrative Plot.
Must be between 0.05 and 0.95."
  :type 'number
  :group 'org-story)

(defcustom org-story-plot-window-position 'top
  "Where to display the Narrative Plot window.
'bottom puts the plot in the bottom window (default). 'top puts it in the top window."
  :type '(choice (const :tag "Plot at bottom" bottom)
                 (const :tag "Plot at top" top))
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

;; -------------------------------
;; Envelope constants
;; -------------------------------

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

;; -------------------------------
;; Caching of ID links
;; -------------------------------

(defcustom org-roam-id-links-cache-directory nil
  "Directory to store cached Org-roam ID links.
If nil, a default subdirectory is used within `org-roam-directory` (or in `user-emacs-directory`)."
  :type 'directory
  :group 'org-roam)

;; -------------------------------
;; Backlink source classification + in-degree constraint
;; -------------------------------

(defcustom org-story-outline-title-regexps '(" — Series Outline$")
  "Regexps tested against org-roam files.title to classify Outline files.
Path is not considered; titles only."
  :type '(repeat string)
  :group 'org-story)

(defcustom org-story-backlink-source-class-default 'outline-series
  "Default backlink source class used when computing in-degree.
Common values:
- 'outline-series: classify sources by `org-story-outline-title-regexps`.
- 'any:           treat any file as a valid source."
  :type '(choice (const outline-series) (const any) (symbol))
  :group 'org-story)

(defcustom org-story-backlink-source-classes
  '((any . (:label "Any file"
           :predicate org-story--src-class-any-p))
    (outline-series . (:label "Series Outline (title regex)"
                      :predicate org-story--src-class-outline-title-p)))
  "Source-class registry mapping class symbol to a plist:
  :label     Human-readable label.
  :predicate A function symbol or function of (file title source-id row) returning non-nil
             if the backlink from that source should be counted."
  :type '(alist :key-type symbol
                :value-type (plist :key-type symbol :value-type sexp))
  :group 'org-story)

(defcustom org-story-backlink-constraint-default 'none
  "Default in-degree constraint asked after operator selection.
- 'none:       no constraint.
- 'indeg-pos:  keep IDs with in-degree > 0 (under the chosen source-class).
- 'indeg-zero: keep IDs with in-degree = 0 (under the chosen source-class)."
  :type '(choice (const none) (const indeg-pos) (const indeg-zero))
  :group 'org-story)

;; -------------------------------
;; Prompt/GPTel serialization options
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

(defcustom org-story-serialization-format 'envelope
  "Serialization for narrative data:
- 'envelope: use org-story-compose-envelope (BEGIN/END markers).
- 'yaml:     minimal YAML: plot + input mapping.
- 'none:     minimal inline text, no framing."
  :type '(choice (const envelope) (const yaml) (const none))
  :group 'org-story)

(defcustom org-story-system-only-user-stub "."
  "User stub string when `org-story-message-mode' is 'system-only'.
Some providers reject empty user content; keep at least one visible char."
  :type 'string
  :group 'org-story)

(provide 'org-story-custom)
;;; org-story-custom.el ends here
