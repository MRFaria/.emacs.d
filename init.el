;;; =========================
;;; PACKAGE SYSTEM
;;; =========================
(require 'package)
(setq package-install-upgrade-built-in t)
(setq package-archives
      '(("GNU ELPA"     . "https://elpa.gnu.org/packages/")
        ("MELPA Stable" . "https://stable.melpa.org/packages/")
        ("MELPA"        . "https://melpa.org/packages/"))
      package-archive-priorities
      '(("MELPA Stable" . 0)
        ("GNU ELPA"     . 5)
        ("MELPA"        . 10)))

(package-initialize)

(require 'use-package)


;;; =========================
;;; CORE EMACS BEHAVIOUR
;;; =========================

(require 'recentf)
(require 'winner)

(winner-mode 1)
(recentf-mode 1)
(server-start)
(setq switch-to-buffer-obey-display-actions t)
(setq inhibit-startup-message t
      inhibit-splash-screen t
      initial-major-mode 'fundamental-mode
      initial-scratch-message "Hello there Mauro\nF7 - notes\nF8/F9 - bookmarks")

(setq large-file-warning-threshold (* 15 1024 1024))

(fset 'yes-or-no-p 'y-or-n-p)

(setq visible-bell nil
      ring-bell-function 'ignore)

(add-hook 'text-mode-hook #'visual-line-mode)


;;; Escape = quit minibuffer everywhere
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;;; =========================
;;; UI / NAVIGATION
;;; =========================

(cua-mode 1)
(context-menu-mode 1)

(global-tab-line-mode 1)

(global-set-key (kbd "M-[") 'tab-bar-history-back)
(global-set-key (kbd "M-]") 'tab-bar-history-forward)

(global-set-key [remap list-buffers] 'ibuffer)

(setq dired-listing-switches "-alh"
      dired-dwim-target t)

(require 'dired-x)
(add-hook 'dired-mode-hook #'hl-line-mode)


;;; =========================
;;; COMPLETION / MINIBUFFER
;;; =========================

(fido-vertical-mode 1)

(setq enable-recursive-minibuffers t
      completion-cycle-threshold 1
      completions-detailed t
      tab-always-indent 'complete
      completion-styles '(flex)
      completion-auto-help 'always
      completion-auto-select 'second-tab
      completions-format 'one-column
      completions-max-height 20
      completions-group t
      read-file-name-completion-ignore-case t)

;;; =========================
;;; RECENT FILES
;;; =========================

(setq recentf-max-saved-items 200)

(run-at-time nil (* 5 60) #'recentf-save-list)

(global-set-key (kbd "C-x C-r") #'recentf-open-files)


;;; =========================
;;; NOTES
;;; =========================

(defvar note-directory
  (if (file-exists-p "~/.emacs.d/work.el")
      (expand-file-name "~/Nextcloud/notes")
    (expand-file-name "/home/mauro/Drive/Notes/")))

;;; =========================
;;; ORG + ORG-ROAM (FIXED)
;;; =========================

(use-package org
  :ensure t)
(use-package org-roam
  :ensure t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory note-directory)
  (org-roam-completion-everywhere t)
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert)
         :map org-mode-map
         ("C-M-i"    . completion-at-point))
  :config
  (org-roam-setup))

;;; =========================
;;; ORG MODE (JOURNAL / TASKS)
;;; =========================

(add-hook 'org-mode-hook #'org-indent-mode)

(setq org-agenda-files
      (list (concat note-directory "projects/")
            (concat note-directory "journal/")))

(setq org-capture-templates
      `(
        ("j" "Yearly Journal" entry
         (file+olp+datetree
          ,(concat note-directory "/journal/"
                   (format-time-string "%Y-journal.org"))
          "Journal")
         "* %<%H:%M>\n%?")

        ("e" "Event" entry
         (file+headline
          ,(concat note-directory "/journal/"
                   (format-time-string "%Y-journal.org"))
          "Events")
         "* %?\n%^T")

        ("t" "Task" entry
         (file+headline
          ,(concat note-directory "/journal/"
                   (format-time-string "%Y-journal.org"))
          "Tasks")
         "* TODO %?\nSCHEDULED: %^t")
        ))

(defun my-open-notes ()
  "Open current year's journal."
  (interactive)
  (find-file
   (concat note-directory
           "/journal/"
           (format-time-string "%Y-journal.org"))))

(global-set-key (kbd "<f7>") #'my-open-notes)
(global-set-key (kbd "C-c c") #'org-capture)


;;; =========================
;;; TREESITTER
;;; =========================

(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript")
        (bash "https://github.com/tree-sitter/tree-sitter-bash")))

(add-to-list 'major-mode-remap-alist '(gdscript-mode . gdscript-ts-mode))
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

(use-package tree-sitter :ensure t)
(use-package tree-sitter-langs :ensure t)

;;; =========================
;;; PACKAGING / FILE CLEANUP
;;; =========================

(use-package no-littering
  :ensure t
  :config
  (no-littering-theme-backups)
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))

  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory))


;;; =========================
;;; PROGRAMMING MODES
;;; =========================

(use-package gdscript-mode
  :ensure t
  :hook ((gdscript-mode . eglot-ensure)
         (gdscript-ts-mode . eglot-ensure)))

(use-package python
  :hook ((python-mode . eglot-ensure)
         (python-ts-mode . eglot-ensure)))

(use-package pyvenv
  :ensure t)
