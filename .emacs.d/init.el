;;; init.el
;; My Emacs initialization file.  See Emacs and Emacs Lisp manuals for more
;; information.

;;;; Customize
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file t))

;;;; File and buffer
(setq make-backup-files nil)
(global-auto-revert-mode)

;;;; Spelling
;; Use Aspell spell-checker instead of Ispell.  See Aspell manual for more
;; information.
(if (executable-find "aspell")
    (setq ispell-program-name "aspell"))
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;;; Syntax
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)

;;;; Indentation
;; Use spaces instead of tabs for indentation by default.  Note some files
;; should use tabs instead of spaces for indentation, e.g., make files.
(setq-default indent-tabs-mode nil)

;;;; Filling
(setq-default fill-column 80)
(add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;;;; Point, mark, and region
(blink-cursor-mode 0)
(delete-selection-mode)

;;;; Tab line
(global-tab-line-mode)

;;;; Mode line
(column-number-mode)

;;;; Fringe
(setq-default indicate-buffer-boundaries 'left)

;;;; Frame parameters
(if (eq system-type 'darwin)
    (add-to-list 'default-frame-alist '(font . "SF Mono")))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;;;; Graphical display
(if (display-graphic-p)
    (progn
      (menu-bar-mode 0)
      (tool-bar-mode 0)
      (scroll-bar-mode 0)))
(if (display-popup-menus-p)
    (setq use-dialog-box nil))
;; TODO also check GTK+ support
(if (eq system-type 'gnu/linux)
    (setq x-gtk-use-system-tooltips nil))

;;;; Package archives
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives
;;              '("melpa-stable" . "https://stable.melpa.org/packages/") t)
;; (package-initialize)

