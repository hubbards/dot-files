;;; init.el --- My personal initialization file.

;;; Commentary:

;; A gentle introduction to Emacs Lisp and Emacs initialization files
;; is provided in "An Introduction to Programming in Emacs Lisp", by
;; Bob Chassell.  See Emacs and Emacs Lisp manuals for more
;; information.

;;; Code:

;; TODO rewrite package section
;; TODO maybe use flymake instead of flycheck

;; Common lisp (CL) functions and macros.
;; (require 'cl-lib)
;; (require 'cl-macs)

;; Make sure to uninstall any unused packages and remove from this list.
(defvar init--packages
  '(magit
    gitconfig-mode
    gitattributes-mode
    gitignore-mode
    editorconfig
    markdown-mode
    yaml-mode
    elm-mode
    haskell-mode
    flycheck
    company
    hl-todo
    ;; crux
    ;; guru-mode
    which-key)
  "Useful packages that aren't built-in.")

;; This function removes a minor mode indicator from mode line by
;; directly modifying the minor mode association list.  Note that mode
;; line indicators for many minor modes are define by customizable
;; variables (i.e., user options).  Setting these variables might be
;; preferred over invoking this function.
(defun init--hide-minor-mode (minor-mode)
  "Hide minor mode MINOR-MODE on mode line."
  (interactive "xMinor mode to hide:")
  (let ((minor-mode-assoc (assoc minor-mode minor-mode-alist)))
    (if minor-mode-assoc
      (setcar (cdr minor-mode-assoc) nil))))

;;;; Custom
;; Use a separate file for custom settings.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file t))

;;;; Package
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-refresh-contents)
(mapc
 (lambda (package)
   (unless (package-installed-p package)
     (package-install package)))
 init--packages)
(package-initialize)

;;;; Graphical display
(load-theme 'adwaita)
(blink-cursor-mode 0)
(setq use-dialog-box nil)
(if (boundp x-gtk-use-system-tooltips)
    (setq x-gtk-use-system-tooltips nil))
(setq-default
 indicate-buffer-boundaries 'left
 indicate-empty-lines t)

;;;; Tab-line
(global-tab-line-mode)

;;;; Mode-line
(line-number-mode)
(column-number-mode)
;; Hide indicators for common minor modes on mode line.
(setq which-key-lighter nil
      editorconfig-mode-lighter nil
      flyspell-mode-line-string nil
      flycheck-mode-line nil
      company-lighter nil
      eldoc-minor-mode-string nil)

;;;; File and buffer
(setq make-backup-files nil)
(global-auto-revert-mode)

;;;; Key bindings
(global-unset-key "\C-xf")
(global-set-key "\C-x\C-b" 'buffer-menu)
(setq tab-always-indent 'complete)
(delete-selection-mode)
(which-key-mode)
;; Use common user access (CUA) style key bindings.
;; (cua-mode)

;;;; EditorConfig
(editorconfig-mode)

;;;; Indentation
;; Use spaces instead of tabs for indentation by default.  Note some
;; files should use tabs instead of spaces for indentation, e.g., make
;; files.
(setq-default indent-tabs-mode nil
              tab-width 8)

;;;; Filling
;; (setq-default fill-column 80)
;; (add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;;;; White space
;; TODO customize `whitespace-style'
;; (add-hook 'text-mode-hook 'whitespace-mode)
;; (add-hook 'prog-mode-hook 'whitespace-mode)
;; (add-hook 'before-save-hook 'whitespace-cleanup)
(setq require-final-newline t)

;;;; Highlighting
(add-hook 'prog-mode-hook 'hl-todo-mode)

;;;; Spell checking
(require 'ispell)
(when (executable-find "aspell")
  (setq ispell-program-name "aspell")
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")
  (add-to-list 'ispell-extra-args "--ignore=3"))
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;;; Syntax checking
(add-hook 'prog-mode-hook 'flycheck-mode)

;;;; Completion
(add-hook 'text-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'company-mode)

;;;; Markdown
;; Manually add autoloads for Markdown mode.
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;;;; Lisp and Scheme
(if (executable-find "guile")
    (setq scheme-program-name "guile"))
(add-hook 'lisp-mode-hook 'show-paren-mode)
(add-hook 'emacs-lisp-mode-hook 'show-paren-mode)
(add-hook 'scheme-mode-hook 'show-paren-mode)

;;;; Haskell
;; TODO customize completion support, see haskell-mode manual

;;; init.el ends here
