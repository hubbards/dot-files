;;; early-init.el --- Personal file -*- lexical-binding: t; -*-

;;; Commentary:

;; My personal early initialization file.

;;; Code:

;;;; Frame

;; Set initial frame parameters.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Disable some default UI.
(if (fboundp 'tool-bar-mode)
    (tool-bar-mode 0))
(if (fboundp 'scroll-bar-mode)
    (scroll-bar-mode 0))

;;; early-init.el ends here
