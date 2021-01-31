;;; early-init.el --- Initialization file -*- lexical-binding: t; -*-

;;; Commentary:

;; My personal (early) initialization file.

;; A gentle introduction to Emacs Lisp and Emacs initialization files is
;; provided in "An Introduction to Programming in Emacs Lisp", by Bob Chassell.
;; See Emacs and Emacs Lisp manuals for more information.

;;; Code:

;;;; Logging

;; Set the minimum severity to log debugging messages.  May want to remove this
;; at some point.
(setq warning-minimum-log-level :debug)

;;;; Frame

;; Set initial frame parameters.  Setting this early during initialization to
;; avoid maximization animation.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Disable some default UI.
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))

;;; early-init.el ends here
