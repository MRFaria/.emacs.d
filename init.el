(cua-mode t)
(server-start)
(package-install 'compat)
(load-file "~/.emacs.d/no-littering.el")
(require 'no-littering)
(no-littering-theme-backups)

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

;; Notes 
(if (file-exists-p "~/.emacs.d/work.el")
    (setq note-directory (expand-file-name "C:/Users/FariaMRD/SynologyDrive/notes"))
  (setq note-directory (expand-file-name "~/SynologyDrive/notes")))

;; Org
(use-package org
  :ensure t
  :config
  (defun my/add-to-agenda-files (keyword)
    "Add files containing 'keyword' to `org-agenda-files` without duplication.
   Ignores backup files (`~`) and auto-save files (`#...#`)."
    (interactive)
    (let ((new-files (seq-filter 
                    (lambda (f) (and (not (string-suffix-p "~" f))
                                     (not (string-match-p "/#.*#$" f))))
                    (directory-files note-directory t keyword))))
      (setq org-agenda-files 
            (delete-dups (append org-agenda-files new-files)))))
  (my/add-to-agenda-files "__journal")

  (setq org-image-actual-width '(1024))
  (setq org-startup-with-inline-images t)
  (setq org-hide-emphasis-markers t)
  )

(use-package org-sliced-images
  :ensure t
  :config (org-sliced-images-mode))

(use-package org-download
  :ensure t
  :hook ('dired-mode-hook . 'org-download-enable)
  :config
  (setq org-download-image-dir (expand-file-name "images" note-directory)))

;; Org journal
(use-package org-journal
  :ensure t
  :defer t
  :init
  ;; Change default prefix key; needs to be set before loading org-journal
  (require 'org-inlinetask)
  (setq org-journal-prefix-key "C-c j ")
  :config
  
  (setq org-journal-file-header
	(format "#+TITLE: Journal-%s\n#+CATEGORY: journal\n#+STARTUP: folded"
		(format-time-string "%Y")))
  (setq org-journal-enable-agenda-integration t)
  (setq org-journal-dir note-directory)
  (setq org-journal-date-format "%A, %d %B %Y")
  (setq org-journal-file-type 'weekly)
  (defun org-journal-file-header-func (time)
    "Custom function to create journal header."
    (concat
     (pcase org-journal-file-type
       (`daily "#+TITLE: Daily Journal\n#+STARTUP: overview")
       (`weekly (format-time-string "#+TITLE: Week-%V-%Y Journal\n#+STARTUP: overview"))
       (`monthly "#+TITLE: Monthly Journal\n#+STARTUP: overview")
       (`yearly "#+TITLE: Yearly Journal\n#+STARTUP: overview"))))
  (setq org-journal-file-header 'org-journal-file-header-func)
  (setq org-journal-file-format "%Y%m%d__Week-%V-%Y__journal.org")
  :bind
  ("<f7>" . org-journal-new-entry))

;; Deft 
(use-package deft
  :ensure t
  :config
  (setq deft-extensions '("txt" "tex" "org"))
  (setq deft-use-filename-as-title t)
  (setq deft-directory note-directory)
  (defun org-open-file-with-emacs (path)
    "Temp replacement function"
    (org-open-file path t)
    )
  )

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\.markdown\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

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

;; Treesit
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

(defun delete-visited-file (buffer-name)
  "Delete the file visited by the buffer named BUFFER-NAME."
  (interactive "bDelete file visited by buffer ")
  (let* ((buffer (get-buffer buffer-name))
         (filename (buffer-file-name buffer)))
    (when buffer
      (when (and filename
                 (file-exists-p filename))
        (delete-file filename))
      (kill-buffer buffer))))

;; gptel
(use-package gptel
  :ensure t
  :config
  (setq gptel-model 'claude-3.7-sonnet
	gptel-backend (gptel-make-gh-copilot "Copilot")
	gptel-curl-file-size-threshold 8000)
  :bind
  ("C-c C-<return>" . gptel-send))

(use-package dired-sidebar
  :ensure t
  :commands (dired-sidebar-toggle-sidebar))
