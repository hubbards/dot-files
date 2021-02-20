;;; sh-hlint.el --- HLint support -*- lexical-binding: t; -*-

;;; Commentary:

;; This library adds support for HLint to Emacs.  It has been tested with
;; version 3.1.6 of HLint and version 27.1 of Emacs on Fedora 33.  See
;; https://github.com/ndmitchell/hlint for more information about HLint.

;; The library includes a Flymake backend for HLint which uses a project level
;; hint file if found.  See the Flymake manual for more information about
;; Flymake.  A long-term goal is to bundle the Flymake related code into a
;; package.

;;; Code:

(require 'map)
(require 'seq)
(require 'flymake)

;; Bind this variable to the hint file name if it is different from the default
;; value.  Usually, this should be a buffer-local binding.
(defvar sh-hlint-hint-file
  ".hlint.yaml"
  "Name of hint file to search for in project root.")

(defun sh-hlint-hint-file (&optional project)
  "Return expected path to hint file in PROJECT or ‘project-current’."
  ;; Pattern for setting optional arguments used in Emacs Lisp Intro.
  (or project (setq project (project-current)))
  (expand-file-name sh-hlint-hint-file
                    (car (project-roots project))))

(defun sh-hlint-flymake (report-fn &rest _args)
  "Flymake backend for HLint with REPORT-FN."
  ;; Check for HLint program.
  (unless (executable-find "hlint")
    (error "Cannot find HLint command"))
  ;; Use a temp file with contents of current buffer.
  (let ((buffer    (current-buffer))
        (hint-file (sh-hlint-hint-file))
        (temp-file (make-temp-file buffer-file-name)))
    (write-region nil nil temp-file)
    ;; Run HLint and report diagnostics.
    (unwind-protect
        (let ((mk-diag (apply-partially 'sh-hlint--mk-diag buffer))
              (ideas   (sh-hlint--command temp-file hint-file)))
          (setq ideas (seq-filter 'sh-hlint--mk-diag-p ideas)
                ideas (seq-map mk-diag ideas))
          (funcall report-fn ideas))
      ;; Clean up temp file.
      (delete-file temp-file))))

;; Add this to ‘haskell-mode-hook’.
(defun sh-hlint-flymake-init ()
  "Initialize Flymake backend for HLint."
  (add-hook 'flymake-diagnostic-functions 'sh-hlint-flymake nil t))

;; See https://github.com/ndmitchell/hlint for details on JSON encoding of
;; ideas.
(defun sh-hlint--command (file &optional hint-file)
  "Invoke HLint on FILE with HINT-FILE if not nil.
An error is thrown if the HLint command cannot be found."
  (unless (executable-find "hlint")
    (error "Cannot find HLint command"))
  (let* ((opt (if (and hint-file (file-exists-p hint-file))
                  (concat "--hint="
                          (shell-quote-argument hint-file)
                          " ")
                ""))
         (cmd (concat "hlint --json "
                      opt
                      (shell-quote-argument file))))
    ;; By default, JSON arrays are represented as Lisp arrays and JSON objects
    ;; are represented as Lisp hash tables.  Use Lisp lists and alists instead
    ;; to avoid the overhead of creating an arrays or hash table.
    (json-parse-string (shell-command-to-string cmd)
                       :array-type  'list
                       :object-type 'alist)))

(defun sh-hlint--mk-diag-p (idea)
  "Predicate to test if a Flymake diagnostic should be made for IDEA."
  (and
   ;; Test if the severity key has one of the expected values.
   (map-contains-key idea 'severity)
   (pcase (map-elt idea 'severity)
     ("Error"      't)
     ("Warning"    't)
     ("Suggestion" 't))
   ;; Test if any of the other expected keys are missing.
   (seq-reduce (lambda (acc key) (and acc (map-contains-key idea key)))
               '(startLine
                 startColumn
                 endLine
                 endColumn
                 hint
                 from
                 to
                 note)
               't)))

(defun sh-hlint--mk-diag (buffer idea)
  "Make Flymake diagnostic for BUFFER and (JSON encoded) IDEA."
  (let ((region (sh-hlint--mk-diag-region buffer idea)))
    (flymake-make-diagnostic buffer
                             (car region)
                             (cdr region)
                             (sh-hlint--mk-diag-type idea)
                             (sh-hlint--mk-diag-text idea))))

(defun sh-hlint--mk-diag-region (buffer idea)
  "Return Flymake diagnostic region of BUFFER for IDEA.
An error is thrown if any of the start or end line or column field is null."
  ;; Check start and end lines and columns are non-null.
  (pcase idea
    ((map ('startLine   :null)) (error "Null start line"))
    ((map ('startColumn :null)) (error "Null start column"))
    ((map ('endLine     :null)) (error "Null end line"))
    ((map ('endColumn   :null)) (error "Null end column")))
  ;; Compute region from start and end lines and columns.
  (map-let (('startLine   l1)
            ('startColumn c1)
            ('endLine     l2)
            ('endColumn   c2))
      idea
    (let ((r1 (flymake-diag-region buffer l1 c1))
          (r2 (flymake-diag-region buffer l2 c2)))
      (cons (min (car r1) (car r2))
            (max (cdr r1) (cdr r2))))))

(defun sh-hlint--mk-diag-type (idea)
  "Map severity of IDEA to Flymake diagnostic symbol.
An error is thrown if the severity field has an unexpected value."
  (pcase (map-elt idea 'severity)
    ("Error"      :error)
    ("Warning"    :warning)
    ("Suggestion" :note)
    (_            (error "Unexpected severity"))))

;; TODO rewrite.
(defun sh-hlint--mk-diag-text (idea)
  "Build Flymake diagnostic text for IDEA.
An error is thrown if either the hint or from field is null."
  ;; Check hint and from are non-null.
  (pcase idea
    ((map ('hint (or :null ""))) (error "Null or empty hint"))
    ((map ('from (or :null ""))) (error "Null or empty from")))
  ;; Build diagnostic text.
  (map-let (hint from to note) idea
    (cl-flet ((padder (s)
                      (mapconcat (lambda (l) (concat "  " l))
                                 (split-string s "\n") "\n")))
      (setq hint (concat "Hint: " hint)
            from (concat "\nFound:\n" (padder from))
            to   (if (or (eq to :null) (string-empty-p to))
                     "\nPerhaps you should remove it."
                   (concat "\nPerhaps:\n" (padder to)))
            note (if (seq-empty-p note)
                     ""
                   (concat "\nNote:\n" (string-join note "\n"))))
      (concat hint from to note))))

;;; Test:

;; TODO move tests into separate file.

(require 'ert)

(ert-deftest sh-hlint--mk-diag-p-test-1 ()
  "Test ‘sh-hlint--mk-diag-p’ with idea containing expected keys and severity."
  (should (sh-hlint--mk-diag-p '((severity    . "Suggestion")
                                 (hint        . :null)
                                 (startLine   . :null)
                                 (startColumn . :null)
                                 (endLine     . :null)
                                 (endColumn   . :null)
                                 (from        . :null)
                                 (to          . :null)
                                 (note)))))

(ert-deftest sh-hlint--mk-diag-p-test-2 ()
  "Test ‘sh-hlint--mk-diag-p’ with idea missing an expected key."
  (should-not (sh-hlint--mk-diag-p '((severity    . "Suggestion")
                                     (startLine   . :null)
                                     (startColumn . :null)
                                     (endLine     . :null)
                                     (endColumn   . :null)
                                     (from        . :null)
                                     (to          . :null)
                                     (note)))))

(ert-deftest sh-hlint--mk-diag-p-test-3 ()
  "Test ‘sh-hlint--mk-diag-p’ with idea containing unexpected severity."
  (should-not (sh-hlint--mk-diag-p '((severity    . "Ignore")
                                     (hint        . :null)
                                     (startLine   . :null)
                                     (startColumn . :null)
                                     (endLine     . :null)
                                     (endColumn   . :null)
                                     (from        . :null)
                                     (to          . :null)
                                     (note)))))

;; Create a temporary buffer using ‘with-temp-buffer’ when testing
;; ‘sh-hlint--mk-diag-region’.  This helps avoid a dependency on (or change to)
;; the state of the environment.  See ERT manual for more information.
(ert-deftest sh-hlint--mk-diag-region-test-1 ()
  "Test ‘sh-hlint--mk-diag-region’ with idea containing non-null start and end
lines and columns."
  (with-temp-buffer
    (insert (make-string 3 ?x))
    (let ((region (sh-hlint--mk-diag-region (current-buffer)
                                            '((startLine   . 1)
                                              (startColumn . 1)
                                              (endLine     . 1)
                                              (endColumn   . 3)))))
      (should (= (car region) 1))
      (should (= (cdr region) 4)))))

(ert-deftest sh-hlint--mk-diag-type-test-1 ()
  "Test ‘sh-hlint--mk-diag-type’ with ideas containing expected severities."
  (should (eq (sh-hlint--mk-diag-type '((severity . "Suggestion"))) :note))
  (should (eq (sh-hlint--mk-diag-type '((severity . "Warning")))    :warning))
  (should (eq (sh-hlint--mk-diag-type '((severity . "Error")))      :error)))

(ert-deftest sh-hlint--mk-diag-type-test-2 ()
  "Test ‘sh-hlint--mk-diag-type’ with ideas containing unexpected severities."
  (should-error (sh-hlint--mk-diag-type '((severity . "Ignore"))))
  (should-error (sh-hlint--mk-diag-type '((severity . :null)))))

;; Check each line of the return value of ‘sh-hlint--mk-diag-text’ individually.
(ert-deftest sh-hlint--mk-diag-text-test-1 ()
  "Test ‘sh-hlint--mk-diag-text’ with idea containing text for hint, from, and
to keys in addition to one note."
  (let ((text (sh-hlint--mk-diag-text '((hint . "Use null")
                                        (from . "length xs == 0")
                                        (to   . "null xs")
                                        (note "increases laziness")))))
    (setq text (split-string text "\n"))
    (should (= (length text) 7))
    (should (string= (pop text) "Hint: Use null"))
    (should (string= (pop text) "Found:"))
    (should (string= (pop text) "  length xs == 0"))
    (should (string= (pop text) "Perhaps:"))
    (should (string= (pop text) "  null xs"))
    (should (string= (pop text) "Note:"))
    (should (string= (pop text) "increases laziness"))))

(ert-deftest sh-hlint--mk-diag-text-test-2 ()
  "Test ‘sh-hlint--mk-diag-text’ with idea containing text for hint and from
keys but an empty string for to key and no note."
  (let ((text (sh-hlint--mk-diag-text '((hint . "Redundant where")
                                        (from . "where")
                                        (to   . "")
                                        (note)))))
    (setq text (split-string text "\n"))
    (should (= (length text) 4))
    (should (string= (pop text) "Hint: Redundant where"))
    (should (string= (pop text) "Found:"))
    (should (string= (pop text) "  where"))
    (should (string= (pop text) "Perhaps you should remove it."))))

;; This test covers a special case where an idea for avoiding a restricted
;; extension has a null to key.
(ert-deftest sh-hlint--mk-diag-text-test-3 ()
  "Test ‘sh-hlint--mk-diag-text’ with idea containing text for hint and from
keys in addition to one note but null to key."
  (let ((text (sh-hlint--mk-diag-text '((hint . "Avoid restricted extensions")
                                        (from . "{-# LANGUAGE GADTs #-}")
                                        (to   . :null)
                                        (note "may break the code")))))
    (setq text (split-string text "\n"))
    (should (= (length text) 6))
    (should (string= (pop text) "Hint: Avoid restricted extensions"))
    (should (string= (pop text) "Found:"))
    (should (string= (pop text) "  {-# LANGUAGE GADTs #-}"))
    (should (string= (pop text) "Perhaps you should remove it."))
    (should (string= (pop text) "Note:"))
    (should (string= (pop text) "may break the code"))))

(provide 'sh-hlint)
;;; sh-hlint.el ends here
