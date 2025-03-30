(cua-mode)


;; Set up custom.el file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (and custom-file
           (file-exists-p custom-file))
  (load custom-file nil :nomessage))

;; Bootstrap crafted-emacs in init.el
(load "~/crafted-emacs/modules/crafted-init-config")

(with-eval-after-load 'package
  (setq package-archive-priorities
        '(("gnu" . 15)          ;; Highest priority
          ("nongnu" . 10)       ;; Second priority
          ("melpa" . 5)         ;; Lower than gnu/nongnu
          ("melpa-stable" . 1)))) ;; Lowest priority


(require 'crafted-ide-packages)
(require 'crafted-ui-packages)
(require 'crafted-org-packages)
(require 'crafted-completion-packages)

;; Install the packages listed in the `package-selected-packages' list.
(add-to-list 'package-selected-packages 'ef-themes)
(add-to-list 'package-selected-packages 'gdscript-mode)
(add-to-list 'package-selected-packages 'denote-journal)
(add-to-list 'package-selected-packages 'magit)
(package-install-selected-packages :noconfirm)

;; Load configuration
(require 'recentf)
(require 'crafted-completion-config)

;; IDE and language completion
(require 'gdscript-mode)
(setq treesit-language-source-alist
      '((python "https://github.com/tree-sitter/tree-sitter-python")
        (gdscript "https://github.com/PrestonKnopp/tree-sitter-gdscript")))
(require 'crafted-ide-config)
(crafted-ide-eglot-auto-ensure-all)
(crafted-ide-configure-tree-sitter)

;; Load crafted-startup configuration
(require 'crafted-startup-config)

;; Load crafted-ui configuration
(require 'crafted-ui-config)

;; Org mode config
(require 'crafted-org-config)
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind
  (("C-c n n" . denote)
   ("C-c n r" . denote-rename-file)
   ("C-c n l" . denote-link)
   ("C-c n b" . denote-backlinks)
   ("C-c n d" . denote-sort-dired))
  :config
  (setq denote-directory (expand-file-name "~/Documents/notes/"))
  (denote-rename-buffer-mode 1))

;; Load crafted-updates configuration
(require 'crafted-defaults-config)

;; Load crafted-speedbar configuration
(require 'crafted-speedbar-config)
(customize-set-variable 'speedbar-use-images t)

;; Escape key 
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)


