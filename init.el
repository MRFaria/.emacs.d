(package-install 'compat)
(load-file "~/.emacs.d/no-littering.el")
(require 'no-littering)
(no-littering-theme-backups)
(setenv "LANG" "en_US.UTF-8")
;; Set up custom.el file and work startup file
(when (file-exists-p "~/.emacs.d/work.el")
  (load-file "~/.emacs.d/work.el")
  (message "loaded work file"))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; No welcome screen - opens directly in scratch buffer
(setq inhibit-startup-message t
      initial-scratch-message "Hello there Mauro\nF7 - notes\nF8/F9 - bookmarks"
      initial-major-mode 'fundamental-mode
      inhibit-splash-screen t)

;; Remove annoying bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; ibuffer and dired
(global-set-key [remap list-buffers] 'ibuffer)
(setq-default dired-listing-switches "-alh")
(require 'dired-x)
(add-hook 'dired-mode-hook 'hl-line-mode)
(setq dired-dwim-target t)

;; Maximise frame and change active window size
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Tab bar
(tab-bar-mode t)
(tool-bar-mode 0)
(global-set-key (kbd "M-[") 'tab-bar-history-back)
(global-set-key (kbd "M-]") 'tab-bar-history-forward)

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

;; Make sure use package is installed
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'bind-key)



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
  :config
  (setq recentf-auto-cleanup 'never) ;; prevent issues with Tramp
  (setq recentf-max-saved-items 100)
  (setq recentf-max-menu-items 15)
  (recentf-mode t)

  (defun my/recentf-ido-find-file ()
    "Find a recent file using ido."
    (interactive)
    (let ((file (completing-read "Choose recent file: " recentf-list nil t)))
      (when file
        (find-file file))))

  :bind ("C-x r" . my/recentf-ido-find-file))

;; Manage the browser
(use-package eww
  :bind* (("M-m g x" . eww)
          ("M-m g :" . eww-browse-with-external-browser)
          ("M-m g #" . eww-list-histories)
          ("M-m g {" . eww-back-url)
          ("M-m g }" . eww-forward-url))
  :config
  (progn
    (add-hook 'eww-mode-hook 'visual-line-mode)))

;; Useful for remembering chord
(use-package which-key
  :defer t
  :diminish which-key-mode
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :bind* (("M-m ?" . which-key-show-top-level))
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
   "M-m ?" "top level bindings"))

;; Org mode and notes
(if (file-exists-p "~/.emacs.d/work.el")
    (setq note-directory (expand-file-name "C:/Users/FariaMRD/OneDrive - University of Twente/Notes"))
  (setq note-directory (expand-file-name "~/SynologyDrive/notes")))
(setq org-image-actual-width '(1024))
(setq org-startup-with-inline-images t)
(setq org-hide-emphasis-markers t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))
(use-package org-sliced-images
  :ensure t
  :config (org-sliced-images-mode))
(use-package org-download
  :ensure t
  :hook ('dired-mode-hook . 'org-download-enable))
(setq-default org-download-image-dir note-directory)
(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (setq org-journal-prefix-key "C-c j ")
  :config
  (setq org-journal-dir note-directory
        org-journal-date-format "%A, %d %B %Y")
  (setq org-journal-file-type 'yearly)
  :bind
  ("<f7>" . org-journal-new-entry))
(use-package deft
  :ensure t
  :config
  (setq deft-extensions '("txt" "tex" "org"))
  (setq deft-use-filename-as-title t)
  (setq deft-directory note-directory))
;; Markdown mode
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\.markdown\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;; languages
;; Godot
(use-package gdscript-mode
  :hook (
 	 (gdscript-ts-mode . eglot-ensure)
	 (gdscript-mode . eglot-ensure))
  :ensure t)

(use-package python
  :hook (
	 (python-ts-mode . eglot-ensure)
	 (python-mode . eglot-ensure))
  )

(use-package pyvenv
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter

  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python.exe")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python")))))

;; Treesit and languages
(setq treesit-language-source-alist
     '((python "https://github.com/tree-sitter/tree-sitter-python")
       (gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript")
       (bash "https://github.com/tree-sitter/tree-sitter-bash")))

(add-to-list 'major-mode-remap-alist '(gdscript-mode . gdscript-ts-mode))
(add-to-list 'major-mode-remap-alist '(sh-mode . bash-ts-mode))
(add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; Configure bookmarks
(use-package bookmark
  :ensure nil
  :commands (bookmark-set bookmark-jump bookmark-bmenu-list)
  :hook (bookmark-bmenu-mode . hl-line-mode)
  :config
  (setq bookmark-use-annotations nil)
  (setq bookmark-automatically-show-annotations nil)
  (setq bookmark-fringe-mark nil) ; Emacs 29 to hide bookmark fringe icon
  ;; Write changes to the bookmark file as soon as 1 modification is
  ;; made (addition or deletion).  Otherwise Emacs will only save the
  ;; bookmarks when it closes, which may never happen properly
  ;; (e.g. power failure).
  (setq bookmark-save-flag 1)
  :bind
  ("<f8>" . bookmark-jump)
  ("<f9>" . bookmark-set))

;; Make escape key stronger
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(cua-mode t)
(server-start)
