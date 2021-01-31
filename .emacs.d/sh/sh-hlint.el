;;; sh-hlint.el --- HLint support -*- lexical-binding: t; -*-

;;; Commentary:

;; This library adds support for HLint to Emacs.  It has been tested with
;; version 3.1.6 of HLint on Fedora 33.  See https://github.com/ndmitchell/hlint
;; for more information about HLint.

;; The library includes a Flymake backend for HLint which uses a project level
;; hint file if found.  See the Flymake manual for more information about
;; Flymake.

;; The library should be added to a package archive, eventually.

;;; Code:

(require 'flymake)

;; Bind this variable to the HLint hint file name if it is different from the
;; default value.  Usually, this should be a buffer-local binding.
(defvar sh-hlint-hint-file
  ".hlint.yaml"
  "Name of HLint hint file to search for in project root.")

(defun sh-hlint-hint-file (&optional project)
  "Return expected path to HLint hint file in PROJECT or `project-current'."
  ;; Pattern for setting optional arguments used in Emacs Lisp Intro.
  (or project (setq project (project-current)))
  (expand-file-name sh-hlint-hint-file
                    (car (project-roots project))))

;; See https://github.com/ndmitchell/hlint for details on JSON encoding of
;; ideas.
(defun sh-hlint--command (file &optional hint-file)
  "Invoke HLint on FILE with HINT-FILE if not nil.
An error is thrown if hlint cannot be found."
  (unless (executable-find "hlint")
    (error "Cannot find hlint"))
  (let* ((opt (if (and hint-file (file-exists-p hint-file))
                  (concat "--hint="
                          (shell-quote-argument hint-file)
                          " ")
                ""))
         (cmd (concat "hlint --json "
                      opt
                      (shell-quote-argument file))))
    (json-parse-string (shell-command-to-string cmd)
                       :array-type  'list
                       :object-type 'alist
                       :null-object 'nil)))

(defun sh-hlint--diag-text (idea)
  "Build Flymake diagnostic text for HLint IDEA."
  (cl-flet ((padder (s)
                    (mapconcat (lambda (l) (concat "  " l))
                               (split-string s "\n")
                               "\n")))
    (let ((hint (alist-get 'hint idea))
          (from (alist-get 'from idea))
          (to   (alist-get 'to   idea))
          (note (alist-get 'note idea)))
      (unless hint (error "Idea missing hint"))
      (setq hint (concat "Hint: " hint))
      (setq from (if from (concat "\nFound:\n"   (padder from))           ""))
      ;; TODO if idea is missing to, then use "Perhaps you should remove it"
      (setq to   (if to   (concat "\nPerhaps:\n" (padder to))             ""))
      (setq note (if note (concat "\nNote:\n"    (string-join note "\n")) ""))
      (concat hint from to note))))

(defun sh-hlint--diag-region (buffer idea)
  "Return Flymake diagnostic region of BUFFER for HLint IDEA."
  (if-let ((l1 (alist-get 'startLine   idea))
           (c1 (alist-get 'startColumn idea))
           (l2 (alist-get 'endLine     idea))
           (c2 (alist-get 'endColumn   idea)))
      (let ((r1 (flymake-diag-region buffer l1 c1))
            (r2 (flymake-diag-region buffer l2 c2)))
        (cons (min (car r1) (car r2))
              (max (cdr r1) (cdr r2))))
    (error "Idea missing start or end line or column")))

(defun sh-hlint--diag-type (idea)
  "Map severity of HLint IDEA to Flymake diagnostic symbol."
  (pcase (alist-get 'severity idea)
    ("Error"      :error)
    ("Warning"    :warning)
    ("Suggestion" :note)
    ;; TODO add diagnositc symbol for ignore
    ("Ignore"     :note)
    (_            (error "Missing or unknown severity"))))

(defun sh-hlint--diag (buffer idea)
  "Make Flymake diagnostic for BUFFER and (JSON encoded) HLint IDEA."
  (let ((region (sh-hlint--diag-region buffer idea)))
    (flymake-make-diagnostic buffer
                             (car region)
                             (cdr region)
                             (sh-hlint--diag-type idea)
                             (sh-hlint--diag-text idea))))

(defun sh-hlint-flymake (report-fn &rest _args)
  "Flymake backend for HLint with REPORT-FN."
  ;; Check for HLint program.
  (unless (executable-find "hlint")
    (error "Cannot find hlint"))
  ;; Use a temp file with contents of current buffer.
  (let ((buffer    (current-buffer))
        (hint-file (sh-hlint-hint-file))
        (temp-file (make-temp-file buffer-file-name)))
    (write-region nil nil temp-file)
    ;; Run HLint and report diagnostics.
    (unwind-protect
        (let ((diags (apply-partially 'sh-hlint--diag buffer))
              (ideas (sh-hlint--command temp-file hint-file)))
          (funcall report-fn (seq-map diags ideas)))
      ;; Clean up temp file.
      (delete-file temp-file))))

;; Add this to `haskell-mode-hook'.
(defun sh-hlint-flymake-init ()
  "Initialize Flymake backend for HLint."
  (add-hook 'flymake-diagnostic-functions 'sh-hlint-flymake nil t))

(provide 'sh-hlint)
;;; sh-hlint.el ends here
