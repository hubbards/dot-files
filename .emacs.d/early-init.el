;;; early-init.el --- My personal early initialization file.

;;; Commentary:

;;; Code:

;;;; Frame
;; Set frame parameters for initial frame.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Disable some UI enabled by default.
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;;; early-init.el ends here
