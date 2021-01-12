;;; init.el --- My personal initialization file.

;;; Commentary:

;; A gentle introduction to Emacs Lisp and Emacs initialization files
;; is provided in "An Introduction to Programming in Emacs Lisp", by
;; Bob Chassell.  See Emacs and Emacs Lisp manuals for more
;; information.

;;; Code:

;; TODO rewrite package section
;; TODO customize `whitespace-style'
;; TODO maybe use flymake instead of flycheck
;; TODO maybe use haskell-language-server instead of haskell-mode

;; This function removes a minor mode indicator from mode line by
;; directly modifying the minor mode association list.  Note that mode
;; line indicators for many minor modes are define by customizable
;; variables (i.e., user options).  Setting these variables might be
;; preferred over invoking this function.
(defun init--hide-minor-mode (minor-mode)
  "Hide minor mode MINOR-MODE on mode line."
  (interactive "xMinor mode to hide: ")
  (if-let ((minor-mode-assoc (assoc minor-mode minor-mode-alist)))
      (setcar (cdr minor-mode-assoc) nil)))

;;;; Custom
;; Use a separate file for custom settings.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file t))

;;;; Package
;; Enable additional package archives.
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archive-priorities '("gnu" . 1))
(add-to-list 'package-archive-priorities '("melpa-stable" . 2))
(add-to-list 'package-archive-priorities '("melpa" . 0))
;; Install useful packages that aren't built-in.  Make sure to
;; uninstall any unused packages and remove from this code.
(package-refresh-contents)
(let ((packages '(which-key
                  ;; crux
                  ;; guru-mode
                  flycheck
                  company
                  ;; projectile
                  hl-todo
                  rainbow-delimiters
                  magit
                  gitconfig-mode
                  gitattributes-mode
                  gitignore-mode
                  editorconfig
                  markdown-mode
                  yaml-mode
                  haskell-mode
                  idris-mode
                  ;; lsp-mode
                  ;; lsp-ui
                  ;; lsp-haskell
                  ;; NOTE add other packages here
                  ))
      (helper (lambda (package)
                (unless (package-installed-p package)
                  (package-install package)))))
  (mapc helper packages))
;; Initialize packages.
(package-initialize)

;;;; Graphical display
;; Set theme and faces.
(load-theme 'adwaita)
(cl-flet ((matchp (pattern)
                  (consp (x-list-fonts pattern 'default nil 1))))
  (cond
   ;; Use system default font on GNOME desktop.
   ((boundp 'font-use-system-font) (setq font-use-system-font t))
   ;; Otherwise, check if preferred fonts are installed and use first
   ;; one found (if any).
   ((matchp "SF Mono") (set-face-font 'default "SF Mono"))
   ;; NOTE add cases for other fonts here
   ))
;; Disable some default UI.
(if (fboundp 'blink-cursor-mode)
    (blink-cursor-mode 0))
(if (display-popup-menus-p)
    ;; Use echo area instead of dialog boxes on mouse click.
    (setq use-dialog-box nil))
(if (boundp 'x-gtk-use-system-tooltips)
    ;; Use Emacs tooltips instead of GTK+.
    (setq x-gtk-use-system-tooltips nil))

;;;; Fringe
;; Show buffer boundaries in fringe.
(setq-default indicate-buffer-boundaries 'left
              indicate-empty-lines t)

;;;; Tab-line
(if (fboundp 'global-tab-line-mode)
    (global-tab-line-mode))

;;;; Mode-line
(line-number-mode)
(column-number-mode)
;; Hide indicators for common minor modes on mode line.
(setq which-key-lighter nil
      editorconfig-mode-lighter nil
      flyspell-mode-line-string nil
      flycheck-mode-line nil
      company-lighter nil)

;;;; Files
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
;; Show fill column when editing text or code.
;; (add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
;; (add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;;;; White space
(setq require-final-newline t)
;; Show white space when editing text or code.
;; (add-hook 'text-mode-hook 'whitespace-mode)
;; (add-hook 'prog-mode-hook 'whitespace-mode)
;; Cleanup whitespace before saving.
;; (add-hook 'before-save-hook 'whitespace-cleanup)

;;;; Highlighting
;; Highlight special words in comments when editing code.
(add-hook 'prog-mode-hook 'hl-todo-mode)

;;;; Spell checking
;; Use Aspell spell checking program.
(require 'ispell)
(when (executable-find "aspell")
  (setq ispell-program-name "aspell")
  (add-to-list 'ispell-extra-args "--sug-mode=ultra")
  (add-to-list 'ispell-extra-args "--ignore=3"))
;; Enable on-the-fly spell checking when editing text or code.
(add-hook 'text-mode-hook 'flyspell-mode)
(add-hook 'prog-mode-hook 'flyspell-prog-mode)

;;;; Syntax checking
;; Enable on-the-fly syntax checking when editing code.
(add-hook 'prog-mode-hook 'flycheck-mode)

;;;; Completion
;; Enable completions when editing text or code.
(add-hook 'text-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'company-mode)

;;;; Markdown
;; Add autoloads for markdown (major) mode.
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;;;; Lisp and Scheme
;; Use Guile implementation of Scheme.
(if (executable-find "guile")
    (setq scheme-program-name "guile"))
;; Enable some helpful minor modes when editing Lisp.
(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                scheme-mode-hook
                ;; NOTE add other (independent) lisp mode hooks here
                ))
  (add-hook hook 'show-paren-mode)
  (add-hook hook 'rainbow-delimiters-mode))
;; Only enable ElDoc mode when editing Emacs Lisp.
(global-eldoc-mode 0)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;;;; Haskell
(setq haskell-process-suggest-remove-import-lines t
      haskell-process-auto-import-loaded-modules t
      haskell-process-log t)
(setq-default haskell-doc-show-global-types t)
(add-hook 'haskell-mode-hook 'haskell-doc-mode)
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(add-hook 'haskell-mode-hook
          (lambda ()
            ;; According to Haskell mode manual, this is a work-around
            ;; to enable completion for let-bindings.
            (make-local-variable 'company-backends)
            (push '(company-capf company-dabbrev-code) company-backends)))

;;; init.el ends here
