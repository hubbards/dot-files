;;; init.el --- Initialization file -*- lexical-binding: t; -*-

;;; Commentary:

;; My personal initialization file.

;; A gentle introduction to Emacs Lisp and Emacs initialization files is
;; provided in "An Introduction to Programming in Emacs Lisp", by Bob Chassell.
;; See Emacs and Emacs Lisp manuals for more information.

;;; Code:

;;;; Custom

;; Use a separate file for custom settings.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(if (file-exists-p custom-file)
    (load custom-file t))

;;;; Package

;; TODO rewrite package section.

;; Enable additional package archives.
(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archive-priorities '("gnu"          . 1))
(add-to-list 'package-archive-priorities '("melpa-stable" . 2))
(add-to-list 'package-archive-priorities '("melpa"        . 0))

;; Install useful packages that aren't built-in.  Make sure to uninstall any
;; unused packages and remove from this code.
(package-refresh-contents)
(let ((packages '(dash
                  dash-functional
                  ;; crux
                  ;; guru-mode
                  which-key
                  magit
                  gitconfig-mode
                  gitattributes-mode
                  gitignore-mode
                  ;; projectile
                  ;; flycheck
                  company
                  editorconfig
                  hl-todo
                  rainbow-delimiters
                  markdown-mode
                  yaml-mode
                  haskell-mode
                  idris-mode
                  ;; TODO use language server protocol.
                  ;; lsp-mode
                  ;; lsp-ui
                  ;; lsp-haskell
                  ;; NOTE add other packages here.
                  ))
      (helper (lambda (package)
                (unless (package-installed-p package)
                  (package-install package)))))
  (mapc helper packages))

;; Initialize packages.
(package-initialize)

;; Add personal libraries to load path.
(defvar init--sh-dir (expand-file-name "sh" user-emacs-directory)
  "Directory for personal libraries not in any package archive.")
(if (file-exists-p init--sh-dir)
    (add-to-list 'load-path (directory-file-name init--sh-dir)))

;;;; Graphical display

;; Set theme and faces.
(load-theme 'adwaita)
(cl-flet ((matchp (pattern)
                  (consp (x-list-fonts pattern 'default nil 1))))
  (cond
   ;; Use system default font on GNOME desktop.
   ((boundp 'font-use-system-font) (setq font-use-system-font t))
   ;; Otherwise, check if preferred fonts are installed and use first one found
   ;; (if any).
   ((matchp "SF Mono") (set-face-font 'default "SF Mono"))
   ;; NOTE add cases for other fonts here.
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

;; Don't show startup screen.
(setq inhibit-startup-screen t)

;;;; Fringe

;; Show buffer boundaries in fringe.
(setq-default indicate-buffer-boundaries 'left
              indicate-empty-lines       t)

;;;; Tab-line

(if (fboundp 'global-tab-line-mode)
    ;; Show tab line.
    (global-tab-line-mode))

;;;; Mode-line

;; This function removes a minor mode indicator from mode line by directly
;; modifying the minor mode association list.  Note that mode line indicators
;; for many minor modes are define by customizable variables (i.e., user
;; options).  Setting these variables might be preferred over invoking this
;; function.
(defun init--hide-minor-mode (mode)
  "Hide minor mode MODE on mode line."
  (interactive "xMinor mode to hide: ")
  (if-let ((mode-assoc (assoc mode minor-mode-alist)))
      (setcar (cdr mode-assoc) nil)))

;; Hide indicators for common minor modes on mode line.
(setq flyspell-mode-line-string     ""
      eldoc-minor-mode-string       ""
      which-key-lighter             ""
      company-lighter               ""
      editorconfig-mode-lighter     ""
      haskell-doc-minor-mode-string ""
      ;; NOTE add other user options here.
      )

;; Show point line and column numbers on mode line.
(line-number-mode)
(column-number-mode)

;;;; Files

(setq make-backup-files nil)
(global-auto-revert-mode)

;;;; Key bindings

(setq tab-always-indent 'complete)

(global-unset-key "\C-xf")
(global-set-key "\C-x\C-b" 'buffer-menu)

(delete-selection-mode)
(which-key-mode)

;;;; Formatting

;; Use spaces instead of tabs for indentation by default.  Note some files
;; should use tabs instead of spaces for indentation, e.g., make files.
(setq-default indent-tabs-mode nil
              tab-width        8)

;; Set default fill column.
(setq-default fill-column 80)

;; Show fill column when editing text or code.
(add-hook 'text-mode-hook 'display-fill-column-indicator-mode)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; TODO use white space mode.
;; TODO customize ‘whitespace-style’.
;; Show white space when editing text or code.
;; (add-hook 'text-mode-hook 'whitespace-mode)
;; (add-hook 'prog-mode-hook 'whitespace-mode)

;; Cleanup whitespace before saving and add a final new line by default.
(setq-default require-final-newline t)
(add-hook 'before-save-hook 'whitespace-cleanup)

;; Use EditorConfig rules if found.
(editorconfig-mode)

;;;; Spelling and syntax

;; Use Aspell spell checking program.
(with-eval-after-load 'ispell
  (when (executable-find "aspell")
    (setq ispell-program-name "aspell")
    (add-to-list 'ispell-extra-args "--sug-mode=ultra")
    (add-to-list 'ispell-extra-args "--ignore=3")))

;; Enable on-the-fly spell checking when editing text or code.
(add-hook 'text-mode-hook 'flyspell-mode)
;; (add-hook 'prog-mode-hook 'flyspell-prog-mode)

;; Enable on-the-fly syntax checking when editing code.
(add-hook 'prog-mode-hook 'flymake-mode)

;; Highlight special words in code comments.
(add-hook 'prog-mode-hook 'hl-todo-mode)

;; TODO enable some electric modes.

;;;; Completion

;; Enable completions when editing text or code.
(add-hook 'text-mode-hook 'company-mode)
(add-hook 'prog-mode-hook 'company-mode)

;;;; Markdown

;; Add autoloads for markdown (major) mode.
(add-to-list 'auto-mode-alist '("\\.md\\'"       . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))

;;;; Lisp and Scheme

;; Use Guile implementation of Scheme.
(if (executable-find "guile")
    (setq scheme-program-name "guile"))

;; Enable some helpful minor modes when editing Lisp.
(dolist (hook '(lisp-mode-hook
                emacs-lisp-mode-hook
                scheme-mode-hook
                ;; NOTE add other (independent) Lisp mode hooks here.
                ))
  (add-hook hook 'show-paren-mode)
  (add-hook hook 'rainbow-delimiters-mode)
  (add-hook hook 'electric-pair-local-mode)
  ;; NOTE add other minor modes here.
  )

;; Only enable ElDoc mode when editing Emacs Lisp code.
(global-eldoc-mode 0)
(add-hook 'emacs-lisp-mode-hook 'eldoc-mode)

;; Insert curved quotes automatically.  Lisp symbols should be delimitted by
;; curved quotes in documentation strings and comments.
(add-hook 'emacs-lisp-mode-hook 'electric-quote-local-mode)

;;;; Haskell

;; Disable error/warning overlay if using Flycheck.
;; (setq haskell-process-show-overlays nil)

;; Ignore any Haskell checkers added to the legacy Proc backend of Flymake.
(with-eval-after-load 'flymake
  (add-to-list 'flymake-proc-ignored-file-name-regexps "\\.l?hs\\'")
  (message "Ignoring Haskell checkers in Proc backend"))

;; Show Haskell documentation.
(add-hook 'haskell-mode-hook 'haskell-doc-mode)

;; NOTE need to run ‘haskell-process-load-file’ (bound to C-c C-l) after
;; loading Haskell file.
;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; According to Haskell mode manual, this is a work-around to enable completion
;; for let-bindings.
;; (add-hook 'haskell-mode-hook
;;           (lambda ()
;;             (make-local-variable 'company-backends)
;;             (push '(company-capf company-dabbrev-code) company-backends)))

;; Use HLint and GHC backends for Flymake.
(require 'sh-hlint)
(require 'sh-ghc)
(add-hook 'haskell-mode-hook 'sh-hlint-flymake-init)
(add-hook 'haskell-mode-hook 'sh-ghc-flymake-init)

;;; init.el ends here
