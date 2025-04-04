;; Set up custom.el file
(when (file-exists-p "~/.emacs.d/work.el")
  (load-file "~/.emacs.d/work.el")
  (message "loaded work file"))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; No welcome screen - opens directly in scratch buffer
(setq inhibit-startup-message t
      initial-scratch-message "Hello there Mauro"
      initial-major-mode 'fundamental-mode
      inhibit-splash-screen t)

;; Remove annoying bell
(setq visible-bell nil)
(setq ring-bell-function 'ignore)

;; Cua mode
(cua-mode 1)

;; Use control + arrow to change window
(windmove-default-keybindings 'meta)

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

;; Slurp environment variables from the shell.
;; a.k.a. The Most Asked Question On r/emacs
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize))

;; Fido (icomplete now includes fuzzy matching for M-x and other completions)
(use-package icomplete
  :bind (:map icomplete-minibuffer-map
              ("C-n" . icomplete-forward-completions)
              ("C-p" . icomplete-backward-completions)
              ("C-v" . icomplete-vertical-toggle)
              ("RET" . icomplete-force-complete-and-exit))
  :hook
  (after-init . (lambda ()
                  (fido-mode 1)
                  ;; (icomplete-mode 1)
                  (icomplete-vertical-mode 1)
                  ))
  :config
  (setq tab-always-indent 'complete)  ;; Starts completion with TAB
  (setq completion-cycle-threshold t)
  (setq icomplete-show-matches-on-no-input t)
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-prospects-height 10)
  (setq icomplete-separator " . ")
  (setq icomplete-with-completion-tables t)
  (setq icomplete-in-buffer t)
  (setq icomplete-max-delay-chars 0)
  (setq icomplete-scroll t)
  (advice-add 'completion-at-point
              :after #'minibuffer-hide-completions))

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

  :bind ("C-x f" . my/recentf-ido-find-file))

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

  :bind ("C-x f" . my/recentf-ido-find-file))

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

;; Restart emacs from emacs
(use-package restart-emacs
  :ensure t
  :bind* (("C-x M-c" . restart-emacs)))

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
(use-package org-sliced-images
  :ensure t
  :config (org-sliced-images-mode))

;; Denote
(use-package denote
  :ensure t
  :commands (denote denote-open-or-create)
  :config
  ;; Pick dates, where relevant, with Org's advanced interface:
  (setq denote-date-prompt-use-org-read-date t)
  (defun my/denote--weekly-template ()
    (concat "* Monday"
            "\n\n"
            "* Tuesday"
            "\n\n"
            "* Wednesday"
            "\n\n"
            "* Thursday"
            "\n\n"
            "* Friday"
            "\n\n"
	    "* Saturday"
	    "\n\n"
	    "* Sunday"
	    "\n\n"
            "* Notes"))

  (setq denote-templates `((weekly . ,(my/denote--weekly-template))))

  (defun my/denote-weekly ()
    "Find or create a weekly journal entry."
    (interactive)
    (let* ((display-time (format-time-string "%G-%U" (current-time)))
           (title (concat "week-" display-time))
           (pattern (concat ".*--" title))
           (matches (denote-directory-files pattern)))
      (if matches
          (find-file (car matches))
	(denote title '("journal" "weekly") 'org nil nil 'weekly))))
  
  ;; Automatically rename Denote buffers using the `denote-rename-buffer-format'.
  (denote-rename-buffer-mode 1)
  (setq denote-directory (expand-file-name "~/SynologyDrive/notes"))
  (setq denote-file-type 'org) ;; Default file format
  (setq denote-known-keywords '("work" "personal" "ideas"))
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

;; Treesit and languages
(setq treesit-language-source-alist
     '((python "https://github.com/tree-sitter/tree-sitter-python")
       (gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript")
       (bash "https://github.com/tree-sitter/tree-sitter-bash")))

;; Gogdot
(use-package gdscript-mode
  :ensure t
  :hook (gdscript-mode . eglot-ensure))
(add-to-list 'major-mode-remap-alist
	     '(gdscript-mode . gdscript-ts-mode)
	     '(sh-mode . bash-ts-mode))

;; Make escape key stronger
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

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

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

