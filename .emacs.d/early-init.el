;;; early-init.el --- My personal early initialization file.

;;; Commentary:

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
