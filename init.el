
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
(require 'winner)
(winner-mode)
(server-start)
(global-tab-line-mode)

;; Make escape stronger
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; No welcome screen - opens directly in scratch buffer
(setq inhibit-startup-message t
      initial-scratch-message "Hello there Mauro\nF7 - notes\nF8/F9 - bookmarks"
      initial-major-mode 'fundamental-mode
      inhibit-splash-screen t)

;; Increase large file warning
(setq large-file-warning-threshold (* 15 1024 1024))

;; Lazy prompting
(fset 'yes-or-no-p 'y-or-n-p)

;; Helpful stuff
(cua-mode 1)
(context-menu-mode 1)
(tab-bar-mode t)
(global-set-key (kbd "M-[") 'tab-bar-history-back)
(global-set-key (kbd "M-]") 'tab-bar-history-forward)

;; Auto completion
(fido-vertical-mode 1)
(setq enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
(setq completion-cycle-threshold 1)                  ; TAB cycles candidates
(setq completions-detailed t)                        ; Show annotations
(setq tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
(setq completion-styles '(flex))
(setq completion-auto-help 'always)                  ; Open completion always; `lazy' another option
(setq completion-auto-select 'second-tab)
(setq completions-format 'one-column)
(setq completions-max-height 20)
(setq completions-group t)
(setq read-file-name-completion-ignore-case t)

;; Recent Files
(use-package recentf
  :init
  (recentf-mode 1)

  :config
  (setq recentf-max-saved-items 200)

  ;; Optional: save the recentf list periodically
  (run-at-time nil (* 5 60) #'recentf-save-list)

  ;; Nice keybinding
  (global-set-key (kbd "C-x C-r") #'recentf-open-files))

;; Remove annoying bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; ibuffer and dired
(global-set-key [remap list-buffers] 'ibuffer)
(setq-default dired-listing-switches "-alh")
(require 'dired-x)
(add-hook 'dired-mode-hook 'hl-line-mode)
(setq dired-dwim-target t)

(use-package org-ql
  :ensure t
  )

;; Org journal
(if (file-exists-p "~/.emacs.d/work.el")
    (setq note-directory (expand-file-name "~/Nextcloud/notes")) ; If work.el exists
  (setq note-directory (expand-file-name "~/Nextcloud/notes")))  ; If work.el does NOT exist

(defun my/jump-to-notes-folder ()
  "Instantly jump to the notes directory using Dired."
  (interactive)
  (dired note-directory))

;; Bind the F9 key globally to jump to your notes
(global-set-key (kbd "<f9>") 'my/jump-to-notes-folder)

(setq org-default-notes-file (concat note-directory "/capture.org"))

;; Fix: Use 'list' instead of a quoted mark so 'concat' can actually run
(setq org-agenda-files (list (concat note-directory "/projects/") 
                             (concat note-directory "/journal/")))

;; 2. Use your exact path inside the capture template
(setq org-capture-templates
      '(("j" "Yearly Journal" entry
         (file+olp+datetree (concat note-directory "/journal.org"))
         "* %U\n%?")))

;; 3. Bind it to your favorite key
(global-set-key (kbd "<f7>") (lambda () (interactive) (org-capture nil "j")))

;; Treesit
(setq treesit-language-source-alist
     '((python "https://github.com/tree-sitter/tree-sitter-python")
       (gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript")
       (bash "https://github.com/tree-sitter/tree-sitter-bash")))
(add-to-list 'major-mode-remap-alist '(gdscript-mode . gdscript-ts-mode))
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))
(require 'tree-sitter)
(require 'tree-sitter-langs)

(use-package no-littering
  :ensure t
  :config
  (no-littering-theme-backups)
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

