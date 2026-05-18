;; Increase large file warning
(setq large-file-warning-threshold (* 15 1024 1024))

;; Lazy prompting
(fset 'yes-or-no-p 'y-or-n-p)

;; Set up packages
(require 'package)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 0)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 10)))

(require 'recentf)
(require 'use-package)

(cua-mode 1)
(global-set-key [remap list-buffers] 'ibuffer)
(fido-vertical-mode 1)
(setq completion-cycle-threshold 3)
(setq tab-always-indent 'complete)
(setq completions-detailed t)
(context-menu-mode 1)
(use-package org-journal
  :ensure t
  :custom
  (org-journal-dir "~/journal/")
  (org-journal-file-type 'daily)
  (org-journal-date-format "%A, %Y-%m-%d"))
;; Treesit
(setq treesit-language-source-alist
     '((python "https://github.com/tree-sitter/tree-sitter-python")
       (gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript")
       (bash "https://github.com/tree-sitter/tree-sitter-bash")))
(add-to-list 'major-mode-remap-alist '(gdscript-mode . gdscript-ts-mode))
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(use-package no-littering
  :ensure t
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-var-directory))
  (add-to-list 'recentf-exclude
             (recentf-expand-file-name no-littering-etc-directory))
  )

;; Godot
(use-package gdscript-mode
  :hook (
 	 (gdscript-ts-mode . eglot-ensure)
	 (gdscript-mode . eglot-ensure))
  :ensure t)

;; Python
(use-package python
  :hook (
	 (python-ts-mode . eglot-ensure)
	 (python-mode . eglot-ensure)))

(use-package pyvenv
  :ensure t)
