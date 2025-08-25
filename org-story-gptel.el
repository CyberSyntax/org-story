;;; org-story-gptel.el --- Prompt/template/envelope + GPTel integration -*- lexical-binding: t; -*-
;;; Commentary:
;; Prompt scanning/compilation, envelope/data composition, and GPTel dispatch.
;;; Code:

(require 'org)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(require 'org-story-core)
(require 'org-story-custom)
(require 'org-story-clean)
(require 'org-story-context)
(require 'org-story-cache)

(eval-when-compile
  (declare-function markdown-mode "markdown-mode"))

(defvar org-story--last-prompt-name nil)
(defvar org-story--gptel-counter 0)

;; -------------------------------
;; Templating/serialization helpers
;; -------------------------------

(defun org-story--indent (s n)
  "Indent every line in string S by N spaces."
  (let* ((pad (make-string n ?\s))
         (ls (split-string (or s "") "\n")))
    (mapconcat (lambda (l) (concat pad l)) ls "\n")))

(defun org-story--template-vars ()
  "Return default template variable env merged with user vars."
  (let ((defaults `(("ENVELOPE_HEADER" . ,org-story-envelope-header)
                    ("BEGIN_PLOT"     . ,org-story-envelope-begin-plot)
                    ("END_PLOT"       . ,org-story-envelope-end-plot)
                    ("BEGIN_INPUT"    . ,org-story-envelope-begin-input)
                    ("END_INPUT"      . ,org-story-envelope-end-input)
                    ("FOCUS_KEY"      . "FOCUS_SENTENCE")
                    ("FOCUS_META_KEY" . "FOCUS_META")
                    ("PREV_KEY"       . "Prev")
                    ("OP_KEY"         . "Op"))))
    (let ((tbl (copy-sequence defaults)))
      (dolist (kv org-story-template-variables)
        (let ((k (car kv)) (v (cdr kv)))
          (setq tbl (assq-delete-all k tbl))
          (push (cons k v) tbl)))
      (nreverse tbl))))

(defun org-story--subst-vars (text var-alist &optional max-passes)
  "Substitute ${VAR} and {{VAR}} until fixpoint or MAX-PASSES."
  (let* ((passes (or max-passes org-story-template-max-subst-passes))
         (i 0) (changed t))
    (while (and changed (< i passes))
      (setq i (1+ i) changed nil)
      (let ((new text))
        ;; ${VAR}
        (setq new
              (replace-regexp-in-string
               "\\${\\([A-Za-z0-9_]+\\)}"
               (lambda (m)
                 (let* ((var (match-string 1))
                        (val (cdr (assoc var var-alist))))
                   (if val (progn (setq changed t) val) m)))
               new t t))
        ;; {{VAR}} (include 아님)
        (setq new
              (replace-regexp-in-string
               "{{\\([A-Za-z0-9_]+\\)}}"
               (lambda (m)
                 (let* ((var (match-string 1))
                        (val (cdr (assoc var var-alist))))
                   (if val (progn (setq changed t) val) m)))
               new t t))
        (setq text new)))
    text))

(defun org-story--expand-inline-includes (text name->rec visited depth)
  "Expand <<include:NAME>> and {{include:NAME}} anywhere in TEXT. Cycle-safe."
  (let* ((rx "\\(<<include:[[:space:]]*\\([[:alnum:]_.-]+\\)[[:space:]]*>>\\)\\|\\({{include:[[:space:]]*\\([[:alnum:]_.-]+\\)[[:space:]]*}}\\)")
         (max-depth org-story-template-max-include-depth)
         (changed t) (passes 0))
    (while (and changed (< passes max-depth))
      (setq changed nil passes (1+ passes))
      (let ((start 0) (out ""))
        (while (string-match rx text start)
          (let* ((mb (match-beginning 0))
                 (me (match-end 0))
                 (nm (or (match-string 2 text) (match-string 4 text)))
                 (rec (cdr (assoc nm name->rec)))
                 (repl ""))
            (setq out (concat out (substring text start mb)))
            (when (and rec (not (member (plist-get rec :name) visited)))
              (setq repl (org-story--compile-template rec name->rec
                                                       (cons (plist-get rec :name) visited)
                                                       (1+ depth)))
              (setq changed t))
            (setq out (concat out repl))
            (setq start me)))
        (setq text (concat out (substring text start)))))
    text))

(defun org-story--strip-front-matter (s)
  "Remove leading YAML front-matter blocks (--- ... ---) from S."
  (let ((s (or s "")) done)
    (while (and (not done)
                (string-match "\\`---[ \t]*\n" s))
      (let ((start (match-end 0)))
        (if (string-match "\n---[ \t]*\n" s start)
            (setq s (substring s (match-end 0)))
          (setq done t))))
    s))

(defun org-story--squeeze-blank-lines (s)
  "Collapse trailing spaces and 3+ consecutive blank lines."
  (let* ((s (replace-regexp-in-string "[ \t]+\n" "\n" (or s ""))))
    (replace-regexp-in-string "\\(?:\n[ \t]*\n\\)\\{2,\\}" "\n\n" s)))

(defun org-story--compile-template (rec name->rec &optional visited depth)
  "Compile REC by: front-matter includes → inline includes → variable subst."
  (let* ((visited (or visited '()))
         (depth   (or depth 0)))
    (if (>= depth org-story-template-max-include-depth)
        (org-story--strip-front-matter (plist-get rec :raw))
      (let* ((pre (mapconcat
                   (lambda (nm)
                     (let ((irec (cdr (assoc nm name->rec))))
                       (if (and irec (not (member (plist-get irec :name) visited)))
                           (org-story--compile-template irec name->rec
                                                        (cons (plist-get irec :name) visited)
                                                        (1+ depth))
                         "")))
                   (or (plist-get rec :includes) '())
                   "\n\n"))
             (raw (org-story--strip-front-matter (plist-get rec :raw)))
             (text (string-trim (if (string-empty-p pre) raw (concat pre "\n\n" raw))))
             (text* (org-story--expand-inline-includes text name->rec visited depth))
             (vars (org-story--template-vars))
             (out  (org-story--subst-vars text* vars org-story-template-max-subst-passes)))
        (org-story--squeeze-blank-lines out)))))

(defun org-story--parse-prompt-record (rec name->rec)
  "Compose final system template by compiling REC (includes + vars)."
  (let* ((compiled (org-story--compile-template rec name->rec nil 0)))
    (list :system-template compiled
          :user-template   nil)))

(defun org-story-compose-envelope (ctx &optional prev op)
  (let* ((focus (org-story--resolve-focus ctx))
         (prev  (or prev "<omitted>"))
         (op    (or op org-story-default-op))
         (plot  (org-story--plot-raw-text ctx))
         (include-meta
          (pcase org-story-include-focus-meta
            ('never nil)
            ('always t)
            (_ (not (org-story--unique-focus-p plot focus))))) ;; 'auto
         (parts (seq-filter
                 #'identity
                 (list
                  org-story-envelope-header
                  org-story-envelope-begin-plot
                  plot
                  org-story-envelope-end-plot
                  org-story-envelope-begin-input
                  (format "FOCUS_SENTENCE: %s" (org-story--quote focus))
                  (when include-meta
                    "FOCUS_META: TIMELINE=?, TIME=?, LOC=?, ACTORS=?")
                  (format "Prev: %s" (if (string= prev "<omitted>")
                                         "<omitted>" (org-story--quote prev)))
                  (format "Op: %s" op)
                  org-story-envelope-end-input
                  (when org-story-append-reply-directive
                    org-story--reply-directive)
                  (when org-story-append-format-tail
                    org-story--format-tail)))))
    (string-join parts "\n")))

(defun org-story--quote (s)
  "Quote S for envelope, escaping internal double quotes."
  (let ((s (or s "")))
    (concat "\"" (replace-regexp-in-string "\"" "\\\\\"" s) "\"")))

(defun org-story--unique-focus-p (plot focus)
  "Return t if FOCUS occurs exactly once in PLOT."
  (let ((pos 0) (cnt 0))
    (while (setq pos (string-match (regexp-quote focus) plot pos))
      (setq cnt (1+ cnt))
      (setq pos (+ pos (length focus))))
    (= cnt 1)))

(defun org-story-compose-data (ctx &optional prev op)
  "Serialize CTX per `org-story-serialization-format`.
Returns a string for the user message (or to append into system in system-only mode)."
  (let* ((focus (org-story--resolve-focus ctx))
         (prev  (or prev "<omitted>"))
         (op    (or op org-story-default-op))
         (plot  (org-story--plot-raw-text ctx))
         (fmt   (or (and (boundp 'org-story-serialization-format)
                         org-story-serialization-format)
                    'envelope)))
    (pcase fmt
      ('envelope
       ;; Preserve the existing envelope (with ::BEGIN markers and header).
       (org-story-compose-envelope ctx prev op))
      ('yaml
       (format "plot: |-\n%s\n\ninput:\n  focus_sentence: %S\n  prev: %S\n  op: %S\n"
               (org-story--indent plot 2) focus prev op))
      ('none
       plot)
      (_
       (org-story-compose-envelope ctx prev op)))))

(defun org-story-compose-payload (ctx prompt-rec name->rec)
  "Compose final (SYSTEM . USER) pair for sending to gptel."
  (let* ((plan  (org-story--parse-prompt-record prompt-rec name->rec))
         (templ (or (plist-get plan :system-template) ""))
         (data  (org-story-compose-data ctx)))
    (if (eq org-story-message-mode 'system-only)
        (cons (string-trim (concat templ "\n\n" data))
              (or org-story-system-only-user-stub "."))
      (cons templ data))))

;; -------------------------------
;; Prompts scanning/dispatch + context
;; -------------------------------

(defun org-story--prompts-dir ()
  "Return prompts directory.
Uses `org-story-prompts-directory` if set; otherwise prompts/ next to org-story.el."
  (if (and (boundp 'org-story-prompts-directory)
           org-story-prompts-directory
           (file-directory-p org-story-prompts-directory))
      (file-name-as-directory org-story-prompts-directory)
    (let ((el (locate-library "org-story")))
      (unless el
        (user-error "Cannot locate org-story.el; set `org-story-prompts-directory`"))
      (let ((dir (expand-file-name "prompts/" (file-name-directory el))))
        (unless (file-directory-p dir)
          (user-error "Prompts directory not found: %s" dir))
        (file-name-as-directory dir)))))

(defun org-story--scan-prompts ()
  "Scan prompt files and return ((BASENAME . REC) ...).
REC plists: :name :path :ext :raw, optional :title :desc :includes."
  (let* ((dir (or (ignore-errors (org-story--prompts-dir)) default-directory))
         (allowed (mapcar #'downcase org-story-prompt-extensions))
         (files (and (file-directory-p dir)
                     (directory-files dir t "^[^.].*")))
         (best (make-hash-table :test 'equal)))
    (cl-labels
        ((ext-rank (ext)
           (or (cl-position (downcase (or ext "")) allowed :test #'equal)
               most-positive-fixnum))
	 (parse-includes-from-fm (fm)
           (let (out)
             (with-temp-buffer
               (insert fm)
               (goto-char (point-min))
               (cond
                ((re-search-forward "^includes:[ \t]*\\[\\([^]]*\\)\\]" nil t)
                 (setq out (mapcar #'string-trim
                                   (split-string (match-string 1) "," t))))
                ((progn
                   (goto-char (point-min))
                   (re-search-forward "^includes:[ \t]*$" nil t))
                 (let (acc)
                   (while (re-search-forward "^[ \t]*-[ \t]*\\([^#\n]+\\)" nil t)
                     (push (string-trim (match-string 1)) acc))
                   (setq out (nreverse acc))))))
             out))
	 (extract-meta (raw)
           (let ((title nil) (desc nil) (incs nil))
             (when (string-match "\\`---[ \t]*\n\\(\\(?:.\\|\n\\)*?\\)\n---[ \t]*\n" raw)
               (let ((fm (match-string 1 raw)))
                 (with-temp-buffer
                   (insert fm)
                   (goto-char (point-min))
                   (when (re-search-forward "^title:[ \t]*\\(.*\\)$" nil t)
                     (setq title (string-trim (match-string 1))))
                   (goto-char (point-min))
                   (when (re-search-forward "^description:[ \t]*\\(.*\\)$" nil t)
                     (setq desc (string-trim (match-string 1))))
                   (setq incs (parse-includes-from-fm fm)))))
             ;; Fallback: Title:/Description: comments anywhere in file
             (unless title
               (when (string-match "^[#; ]*Title:[ \t]*\\(.*\\)$" raw)
                 (setq title (string-trim (match-string 1 raw)))))
             (unless desc
               (when (string-match "^[#; ]*Description:[ \t]*\\(.*\\)$" raw)
                 (setq desc (string-trim (match-string 1 raw)))))
             (list :title title :desc desc :includes incs))))
      (dolist (path files)
        (when (and (file-regular-p path)
                   (member (downcase (or (file-name-extension path) "")) allowed))
          (let* ((ext  (downcase (file-name-extension path)))
                 (name (file-name-base path))
                 (rank (ext-rank ext))
                 (cur  (gethash name best))
                 (cur-rank (plist-get cur :_rank)))
            (when (or (null cur) (< rank cur-rank))
              (puthash name (list :name name :path path :ext ext :_rank rank) best)))))
      (let (alist)
        (maphash
         (lambda (name rec)
           (let* ((raw (with-temp-buffer
                         (insert-file-contents (plist-get rec :path))
                         (buffer-string)))
                  (meta (extract-meta raw))
                  (clean (cl-loop for (k v) on rec by #'cddr
                                  unless (eq k :_rank) append (list k v))))
             (setq clean (plist-put clean :raw raw))
             (when (plist-get meta :title)
               (setq clean (plist-put clean :title (plist-get meta :title))))
             (when (plist-get meta :desc)
               (setq clean (plist-put clean :desc (plist-get meta :desc))))
             (when (plist-get meta :includes)
               (setq clean (plist-put clean :includes (plist-get meta :includes))))
             (push (cons name clean) alist)))
         best)
        (nreverse alist)))))

(defun org-story--post-plot-dispatch ()
  "Compose payload and send to gptel.
UI hides basenames starting with '_', but compiler sees the full set."
  (interactive)
  (unless (featurep 'gptel)
    (user-error "gptel not loaded"))
  (let* ((ctx (org-story--plot-context))
         (full (org-story--scan-prompts))
         (visible (seq-filter (lambda (kv) (not (string-prefix-p "_" (car kv))))
                              full)))
    (unless visible
      (user-error "Only include parts found; add at least one main prompt"))
    (let* ((display-map
            (mapcar (lambda (kv)
                      (let* ((name (car kv)) (rec (cdr kv))
                             (title (or (plist-get rec :title) name))
                             (ext   (plist-get rec :ext))
                             (disp  (format "%s — (%s.%s)" title name ext)))
                        (cons disp name)))
                    visible))
           (displays (mapcar #'car display-map))
           (initial (or org-story--last-prompt-name (car displays)))
           (choice-disp
            (if (and (= (length displays) 1)
                     (not org-story-always-prompt-template))
                (car displays)
              (completing-read "Prompt: " displays nil t nil nil initial)))
           (choice (cdr (assoc choice-disp display-map)))
           (rec (cdr (assoc choice visible)))
           (pair (org-story-compose-payload ctx rec full))
           (system (car pair))
           (user   (cdr pair)))
      (setq org-story--last-prompt-name choice-disp)
      (org-story-open-gptel-tab user system))))

;; -------------------------------
;; GPTel buffer/tab helpers
;; -------------------------------

(defun org-story--unique-gptel-buffer ()
  (let* ((stamp (format-time-string "%Y%m%d-%H%M%S"))
         (salt (substring (md5 (format "%s-%S-%s"
                                       stamp (current-time)
                                       (cl-incf org-story--gptel-counter)))
                          0 6))
         (name (format "*gptel org-story %s-%s*" stamp salt))
         (buf  (generate-new-buffer name)))
    (with-current-buffer buf
      (if (fboundp 'markdown-mode) (markdown-mode) (text-mode)))
    buf))

(defun org-story--ensure-tab-bar ()
  "Enable tab-bar-mode if `org-story-use-tab-bar' is non-nil.
Return non-nil if we enabled it here (informational; not used)."
  (when org-story-use-tab-bar
    (unless (bound-and-true-p tab-bar-mode)
      (tab-bar-mode 1)
      t)))

(defun org-story-open-gptel-tab (payload system)
  "Open a new tab (if enabled), prepare a gptel buffer, send, and optionally return.
- SYSTEM is set as the gptel system message (buffer-local).
- PAYLOAD is inserted as the user message content (can be empty).
- If `org-story-auto-return-from-gptel' is non-nil, switch back to the previous tab."
  (interactive)
  (org-story--ensure-tab-bar)
  (when (bound-and-true-p tab-bar-mode)
    (tab-bar-new-tab)
    (tab-bar-rename-tab (format "org-story %s" (format-time-string "%H:%M:%S"))))
  (let* ((buf (org-story--unique-gptel-buffer))
         (msg (or (and (stringp payload) payload)
                  (and (eq org-story-message-mode 'system-only)
                       org-story-system-only-user-stub)
                  "")))
    (switch-to-buffer buf)
    (if (fboundp 'markdown-mode) (markdown-mode) (text-mode))
    (when (fboundp 'gptel-mode) (gptel-mode 1))
    (cond
     ((boundp 'gptel-system-prompt)
      (setq-local gptel-system-prompt system))
     ((boundp 'gptel-system-message)
      (setq-local gptel-system-message system))
     (t
      ;; Fallback for very old/new gptel variants; create a buffer-local var.
      (set (make-local-variable (intern "gptel-system-message")) system)))
    (when (> (length msg) 0)
      (insert msg)
      (goto-char (point-max)))
    (when (fboundp 'gptel-send)
      (gptel-send))
    (when (and org-story-auto-return-from-gptel
               (bound-and-true-p tab-bar-mode))
      (run-at-time 0 nil #'tab-bar-switch-to-recent-tab))))

(provide 'org-story-gptel)
;;; org-story-gptel.el ends here
