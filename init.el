;; prevent emacs from putting files everywhere
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
(setq completions-max-height 10)
(setq completions-group t)
;(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

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
  :ensure t
  :defer t
  :diminish which-key-mode
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :bind* (("M-m ?" . which-key-show-top-level))
  :config
  (which-key-mode)
  (which-key-add-key-based-replacements
   "M-m ?" "top level bindings"))

;; Org mode
(setq org-image-actual-width '(1024))
(setq org-startup-with-inline-images t)
(setq org-hide-emphasis-markers t)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-function-global '(org-agenda-skip-entry-if 'todo 'done))

(use-package org-sliced-images
  :ensure t
  :config (org-sliced-images-mode))


(defun jab/denote-add-to-agenda-files (keyword)
  "Add files containing 'keyword' to `org-agenda-files` without duplication.
Ignores backup files (`~`) and auto-save files (`#...#`)."
  (interactive)
  (let ((new-files (seq-filter 
                    (lambda (f) (and (not (string-suffix-p "~" f))
                                     (not (string-match-p "/#.*#$" f))))
                    (directory-files denote-directory t keyword))))
    (setq org-agenda-files 
          (delete-dups (append org-agenda-files new-files)))))
;; Denote
(use-package denote
  :ensure t
  :commands (denote denote-open-or-create)
  :config
  ;; Pick dates, where relevant, with Org's advanced interface:
  ;; Add all Denote files tagged as "project" to org-agenda-files
  (setq denote-date-prompt-use-org-read-date t)

  (defun my/denote--weekly-template ()
    (concat
     (format-time-string "#category: journal %G-%V\n\n" (current-time))
     "* Monday\n\n"
     "* Tuesday\n\n"
     "* Wednesday\n\n"
     "* Thursday\n\n"
     "* Friday\n\n"
     "* Saturday\n\n"
     "* Sunday\n\n"
     "* Notes"))

  (setq denote-templates `((weekly . ,(my/denote--weekly-template))))

  (defun my/denote-weekly ()
    "Find or create a weekly journal entry."
    (interactive)
    (let* ((display-time (format-time-string "%G-%V" (current-time)))
           (title (concat "week-" display-time))
           (pattern (concat ".*--" title))
           (matches (denote-directory-files pattern)))
      (if matches
          (find-file (car matches))
	(denote title '("journal" "weekly") 'org nil nil 'weekly))))
  
  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1)
  (if (file-exists-p "~/.emacs.d/work.el")
      (setq denote-directory (expand-file-name "M:/Documents/Notes"))
    (setq denote-directory (expand-file-name "~/SynologyDrive/notes")))
  (setq denote-file-type 'org) ;; Default file format
  (setq denote-known-keywords '("work" "personal" "ideas"))

  (jab/denote-add-to-agenda-files "__journal")
  
  :bind
  ("<f7>" . my/denote-weekly))

(use-package denote-org :ensure t)
(use-package denote-search :ensure t)

;; Markdown mode
(use-package markdown-mode
  :ensure t
  :mode ("README\\.md\\.markdown\\'" . gfm-mode)
  :init (setq markdown-command "multimarkdown")
  :bind (:map markdown-mode-map
              ("C-c C-e" . markdown-do)))

;; Godot
(use-package gdscript-mode
  :ensure t
  :hook (gdscript-mode . eglot-ensure))

(use-package python)

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

;; Version control
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

;; Make escape key stronger
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(cua-mode t)
