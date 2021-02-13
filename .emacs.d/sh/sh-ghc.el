;;; sh-ghc.el --- GHC support -*- lexical-binding: t; -*-

;;; Commentary:

;; This library adds support for GHC to Emacs.  It has been tested with
;; version 8.6.5 of GHC on Fedora 33.  See https://gitlab.haskell.org/ghc/ghc
;; for more information about GHC.

;; The library includes a Flymake backend for GHC.  See the Flymake manual for
;; more information about Flymake.  A long-term goal is to bundle the Flymake
;; related code into a package.

;;; Code:

(require 'map)
(require 'seq)
(require 'flymake)

(defun sh-ghc-flymake (report-fn &rest _args)
  "Flymake backend for GHC with REPORT-FN."
  (unless (or (executable-find "stack")
              (executable-find "ghc"))
    (error "Cannot find stack or ghc"))
  ;; Use a temp file with contents of current buffer.  We must be careful with
  ;; how we name the temp file because GHC checks the file extension.
  (let ((buffer    (current-buffer))
        (temp-file (let ((prefix (file-name-sans-extension buffer-file-name))
                         (suffix (file-name-extension buffer-file-name 't)))
                     (make-temp-file prefix nil suffix))))
    (write-region nil nil temp-file)
    ;; TODO run GHC in asychronous sub-process
    ;; Run GHC and report diagnostics.
    (unwind-protect
        (let ((mk-diag (apply-partially 'sh-ghc--mk-diag buffer))
              (msgs    (sh-ghc--command temp-file)))
          (setq msgs (seq-filter 'sh-ghc--mk-diag-p msgs)
                msgs (seq-map mk-diag msgs))
          (funcall report-fn msgs))
      ;; Clean up temp file.
      (delete-file temp-file))))

;; Add this to `haskell-mode-hook'.
(defun sh-ghc-flymake-init ()
  "Initialize Flymake backend for GHC."
  (add-hook 'flymake-diagnostic-functions 'sh-ghc-flymake nil t))

;; See https://gitlab.haskell.org/ghc/ghc for details on JSON encoding of
;; errors.
(defun sh-ghc--command (file)
  "Invoke GHC on FILE.
An error is thrown if the Stack or GHC command cannot be found."
  (let ((cmd   (let ((prog (cond ((executable-find "stack") "stack ghc -- ")
                                 ((executable-find "ghc")   "ghc ")
                                 (t (error "Cannot find stack or ghc"))))
                     (opts (list "-fdiagnostics-color=never"
                                 "-fno-code"
                                 "-ddump-json"
                                 "-Wall")))
                 (concat prog
                         (string-join opts " ")
                         " "
                         (shell-quote-argument file))))
        ;; By default, JSON arrays are represented as Lisp arrays and JSON
        ;; objects are represented as Lisp hash tables.  Use Lisp lists and
        ;; alists instead to avoid the overhead of creating an arrays or hash
        ;; table.
        (parse (lambda (string)
                 (json-parse-string string
                                    :array-type  'list
                                    :object-type 'alist))))
    (seq-map parse (split-string (shell-command-to-string cmd) "\n" t))))

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
  "Build Flymake diagnostic text for MSG."
  (pcase msg
    ((map ('doc :null))    (error "Null doc"))
    ((map ('reason :null)) (error "Null reason"))
    ((map reason doc)      (format "Reason: %s\nMessage:\n%s" reason doc))))

;;; Test:

;; TODO move tests into separate file

(require 'ert)

;; (ert-deftest sh-ghc--mk-diag-region-test-1 ()
;;   ;; TODO write documentation string
;;   ;; TODO implement
;;   (error "Not implemented"))

;; (ert-deftest sh-ghc--mk-diag-type-test-1 ()
;;   ;; TODO write documentation string
;;   ;; TODO implement
;;   (error "Not implemented"))

;; (ert-deftest sh-ghc--mk-diag-text-test-1 ()
;;   ;; TODO write documentation string
;;   ;; TODO implement
;;   (error "Not implemented"))

;; (ert-deftest sh-ghc--mk-diag-p-test-1 ()
;;   ;; TODO write documentation string
;;   ;; TODO implement
;;   (error "Not implemented"))

(provide 'sh-ghc)
;;; sh-ghc.el ends here
