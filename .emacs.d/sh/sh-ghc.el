;;; sh-ghc.el --- GHC support -*- lexical-binding: t; -*-

;;; Commentary:

;; This library adds support for GHC to Emacs.  It has been tested with version
;; 8.6.5 of GHC and version 27.1 of Emacs on Fedora 33.  See
;; https://gitlab.haskell.org/ghc/ghc for more information about GHC.

;; The library includes a Flymake backend for GHC.  See the Flymake manual for
;; more information about Flymake.  A long-term goal is to bundle the Flymake
;; related code into a package.

;;; Code:

(require 'map)
(require 'seq)
(require 'flymake)

;; This variable is automatically buffer-local.
(defvar-local sh-ghc--flymake-proc
  nil
  "GHC process associated with current Flymake check.")

;; See https://gitlab.haskell.org/ghc/ghc for details on JSON encoding of
;; errors.
(defun sh-ghc--flymake-proc (report-fn buffer file)
  "Make GHC process sentinel with REPORT-FN, BUFFER, and FILE."
  (lambda (proc _event)
    ;; The process sentinel is called whenever the process state changes.
    ;; Use ‘process-status’ instead of checking the ‘_event’ argument.
    (if (eq 'exit (process-status proc))
        (unwind-protect
            ;; Only proceed if ‘proc’ was launched by current check.  We must be
            ;; careful about which buffer is current because
            ;; ‘sh-ghc--flymake-proc’ is buffer-local.
            (if (with-current-buffer buffer (eq proc sh-ghc--flymake-proc))
                (with-current-buffer (process-buffer proc)
                  ;; TODO remove debug message.
                  (flymake-log :debug (buffer-string))
                  (goto-char (point-min))
                  (let ((diags nil))
                    (while (not (eobp))
                      (narrow-to-region (line-beginning-position)
                                        (line-end-position))
                      ;; By default, JSON arrays are represented as Lisp arrays
                      ;; and JSON objects are represented as Lisp hash tables.
                      ;; Use Lisp lists and alists instead to avoid the overhead
                      ;; of creating an arrays or hash table.
                      (let ((msg (json-parse-buffer :array-type  'list
                                                    :object-type 'alist)))
                        (if (sh-ghc--mk-diag-p msg)
                            (push (sh-ghc--mk-diag buffer msg) diags)))
                      (widen)
                      (forward-line 1))
                    ;; Report diagnostics.
                    (funcall report-fn diags)))
              (flymake-log :warning "Canceling obsolete check %s" proc))
          ;; Cleanup temporary process buffer and file.
          (kill-buffer (process-buffer proc))
          (delete-file file)))))

(defun sh-ghc-flymake (report-fn &rest _args)
  "Flymake backend for GHC with REPORT-FN.
An error is thrown if GHC command cannot be found."
  ;; TODO support commands for invoking GHC other than Stack.
  (unless (executable-find "stack")
    (error "Cannot find GHC command"))
  ;; Check for process launched by earlier check and kill it if found.
  (if (process-live-p sh-ghc--flymake-proc)
      (kill-process sh-ghc--flymake-proc))
  (let* ((init-buffer (current-buffer))
         ;; Use a temp file with contents of current buffer.  We must be careful
         ;; with how we name the file because GHC checks the file extension.
         (temp-file   (let ((prefix (file-name-sans-extension buffer-file-name))
                            (suffix (file-name-extension buffer-file-name t)))
                        (make-temp-file prefix nil suffix)))
         (proc-name   "sh-ghc-flymake")
         (proc-buffer (generate-new-buffer "*sh-ghc-flymake*"))
         ;; TODO handle imports.
         (proc-cmd    (list "stack"
                            "ghc"
                            "--"
                            "-fdiagnostics-color=never"
                            "-fno-code"
                            "-ddump-json"
                            "-Wall"
                            temp-file))
         (proc-helper (sh-ghc--flymake-proc report-fn init-buffer temp-file)))
    (write-region nil nil temp-file)
    ;; Run GHC in asychronous sub-process.  If Stack is used, then this command
    ;; could take a while to run the first time it is invoked, depending on
    ;; which dependencies are needed.
    (setq sh-ghc--flymake-proc (make-process :name            proc-name
                                             :buffer          proc-buffer
                                             :command         proc-cmd
                                             :connection-type 'pipe
                                             :sentinel        proc-helper))))

;; Add this to ‘haskell-mode-hook’.
(defun sh-ghc-flymake-init ()
  "Initialize Flymake backend for GHC."
  (add-hook 'flymake-diagnostic-functions 'sh-ghc-flymake nil t))

(defun sh-ghc--mk-diag-p (msg)
  "Predicate to test if a Flymake diagnostic should be made for MSG."
  (and
   ;; Test if the severity key has one of the expected values.
   (map-contains-key msg 'severity)
   (pcase (map-elt msg 'severity)
     ("SevError"   't)
     ("SevWarning" 't))
   ;; Test if any of the span keys are missing.
   (map-contains-key msg 'span)
   (map-nested-elt msg '(span startLine))
   (map-nested-elt msg '(span startCol))
   (map-nested-elt msg '(span endLine))
   (map-nested-elt msg '(span endCol))
   ;; Test if any other expected keys are missing.
   (map-contains-key msg 'doc)
   (map-contains-key msg 'reason)))

(defun sh-ghc--mk-diag (buffer msg)
  "Make Flymake diagnostic for BUFFER and (JSON encoded) MSG."
  (let ((region (sh-ghc--mk-diag-region buffer (map-elt msg 'span))))
    (flymake-make-diagnostic buffer
                             (car region)
                             (cdr region)
                             (sh-ghc--mk-diag-type msg)
                             (sh-ghc--mk-diag-text msg))))

(defun sh-ghc--mk-diag-region (buffer span)
  "Return Flymake diagnostic region of BUFFER for SPAN."
  ;; Check start and end lines and columns are non-null.
  (pcase span
    (:null                    (error "Null span"))
    ((map ('startLine :null)) (error "Null start line"))
    ((map ('startCol  :null)) (error "Null start column"))
    ((map ('endLine   :null)) (error "Null end line"))
    ((map ('endCol    :null)) (error "Null end column")))
  ;; Compute region from start and end lines and columns.
  (map-let (('startLine l1)
            ('startCol  c1)
            ('endLine   l2)
            ('endCol    c2))
      span
    (let ((r1 (flymake-diag-region buffer l1 c1))
          (r2 (flymake-diag-region buffer l2 c2)))
      (cons (min (car r1) (car r2))
            (max (cdr r1) (cdr r2))))))

(defun sh-ghc--mk-diag-type (msg)
  "Map severity of MSG to Flymake diagnostic symbol.
An error is thrown if the severity field has an unexpected value."
  (pcase (map-elt msg 'severity)
    ("SevError"   :error)
    ("SevWarning" :warning)
    (_            (error "Missing or unexpected severity"))))

(defun sh-ghc--mk-diag-text (msg)
  "Build Flymake diagnostic text for MSG.
An error is thrown if the doc field is null."
  (map-let (doc reason) msg
    (if (eq doc :null)
        (error "Null doc"))
    (setq doc    (concat "Message:\n" doc)
          reason (if (eq reason :null)
                     ""
                   (format "Reason: %s\n" reason)))
    (concat reason doc)))

;;; Test:

;; TODO move tests into separate file.

(require 'ert)

(ert-deftest sh-ghc--mk-diag-p-test-1 ()
  "Test ‘sh-ghc--mk-diag-p’ with message containing expected keys and severity."
  (should (sh-ghc--mk-diag-p '((span    . ((startLine . :null)
                                           (startCol  . :null)
                                           (endLine   . :null)
                                           (endCol    . :null)))
                               (doc      . :null)
                               (severity . "SevError")
                               (reason   . :null)))))

(ert-deftest sh-ghc--mk-diag-p-test-2 ()
  "Test ‘sh-ghc--mk-diag-p’ with message missing expected key."
  (should-not (sh-ghc--mk-diag-p '((span     . :null)
                                   (doc      . :null)
                                   (severity . "SevError")
                                   (reason   . :null)))))

(ert-deftest sh-ghc--mk-diag-p-test-3 ()
  "Test ‘sh-ghc--mk-diag-p’ with message containing unexpected severity."
  (should-not (sh-ghc--mk-diag-p '((span     . ((startLine . :null)
                                                (startCol  . :null)
                                                (endLine   . :null)
                                                (endCol    . :null)))
                                   (doc      . :null)
                                   (severity . "SevOutput")
                                   (reason   . :null)))))

;; Create a temporary buffer using ‘with-temp-buffer’ when testing
;; ‘sh-ghc--mk-diag-region’.  This helps avoid a dependency on (or change to)
;; the state of the environment.  See ERT manual for more information.
(ert-deftest sh-ghc--mk-diag-region-test-1 ()
  "Test ‘sh-ghc--mk-diag-region’ with message containing non-null start and end
lines and columns."
  (with-temp-buffer
    (insert (make-string 3 ?x))
    (let ((region (sh-ghc--mk-diag-region (current-buffer)
                                          '((startLine . 1)
                                            (startCol  . 1)
                                            (endLine   . 1)
                                            (endCol    . 3)))))
      (should (= (car region) 1))
      (should (= (cdr region) 4)))))

(ert-deftest sh-ghc--mk-diag-type-test-1 ()
  "Test ‘sh-ghc--mk-diag-type’ with message containing expected severities."
  (should (equal (sh-ghc--mk-diag-type '((severity . "SevError")))   :error))
  (should (equal (sh-ghc--mk-diag-type '((severity . "SevWarning"))) :warning)))

(ert-deftest sh-ghc--mk-diag-type-test-2 ()
  "Test ‘sh-ghc--mk-diag-type’ with message containing unexpected severities."
  (should-error (sh-ghc--mk-diag-type '((severity . "SevOutput"))))
  (should-error (sh-ghc--mk-diag-type '((severity . :null)))))

;; Check each line of the return value of ‘sh-ghc--mk-diag-text’ individually.
(ert-deftest sh-ghc--mk-diag-text-test-1 ()
  "Test ‘sh-ghc--mk-diag-text’ with message containing text for doc and reason."
  (let ((text (sh-ghc--mk-diag-text '((doc      . "Defined but not used: ‘xs’")
                                      (severity . "SevWarning")
                                      (reason   . "Opt_WarnUnusedMatches")))))
    (setq text (split-string text "\n"))
    (should (= (length text) 3))
    (should (string= (pop text) "Reason: Opt_WarnUnusedMatches"))
    (should (string= (pop text) "Message:"))
    (should (string= (pop text) "Defined but not used: ‘xs’"))))

(ert-deftest sh-ghc--mk-diag-text-test-2 ()
  "Test ‘sh-ghc--mk-diag-text’ with message missing reason."
  (let ((text (sh-ghc--mk-diag-text '((doc      . "Could not find module Foo")
                                      (severity . "SevError")
                                      (reason   . :null)))))
    (setq text (split-string text "\n"))
    (should (= (length text) 2))
    (should (string= (pop text) "Message:"))
    (should (string= (pop text) "Could not find module Foo"))))

(provide 'sh-ghc)
;;; sh-ghc.el ends here
