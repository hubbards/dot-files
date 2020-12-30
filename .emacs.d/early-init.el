;;; early-init.el --- My personal early initialization file.

;;; Commentary:

;;; Code:

;;;; Frame
;; Set initial frame parameters.
(add-to-list 'initial-frame-alist '(fullscreen . maximized))
;; Disable some UI enabled by default.
;; (menu-bar-mode 0)
(tool-bar-mode 0)
(scroll-bar-mode 0)

;;; early-init.el ends here
