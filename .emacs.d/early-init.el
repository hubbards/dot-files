;;; early-init.el --- My personal early initialization file.

;;; Commentary:

;;; Code:

;;;; Frame
;; Set frame parameters for initial frame.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Disable some UI enabled by default.
(tool-bar-mode 0)
(scroll-bar-mode 0)
;; (menu-bar-mode 0)

;;; early-init.el ends here
