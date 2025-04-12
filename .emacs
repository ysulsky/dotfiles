;; Performance
(setq gc-cons-threshold (* 384 1024 1024)
      gc-cons-percentage 0.6)

;; Do not load outdated byte code files.
(setq load-prefer-newer t)

;; Default was too low.
;; Increase for better lsp performance.
(setq read-process-output-max (* 3 1024 1024)) ;; 3mb

;; Initialize package sources
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(global-set-key "\C-z" nil)  ; I never want to stop Emacs.
;; call last keyboard C-x e -> f5
(global-set-key [f5] 'kmacro-end-and-call-macro)

;; Additional key bindings, some replaced below.
(global-set-key (kbd "C-c C-t") 'eshell)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "M-C-s") 'query-replace-regexp)
(global-set-key (kbd "C-x g") 'goto-line)

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; Initialize use-package
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'ansi-color)
(require 'use-package)
(require 'xref)
(defvar xref-auto-jump-to-first-definition nil)
(setq use-package-always-ensure t)

(use-package all-the-icons
  :if (display-graphic-p))

;; ;; Add doom-themes
;; (use-package doom-themes
;;   :config
;;   ;; Global settings (defaults)
;;   (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
;;         doom-themes-enable-italic t) ; if nil, italics is universally disabled
;;   ;; Enable custom neotree theme (all-the-icons must be installed!)
;;   (doom-themes-neotree-config)
;;   ;; Corrects (and improves) org-mode's native fontification.
;;   (doom-themes-org-config))

(use-package shfmt)

(defun format-bash-code ()
  (interactive)
  (if (use-region-p)
      (shfmt-region (region-beginning) (region-end))
    (shfmt-buffer)))

(add-hook 'sh-mode-hook
          (lambda ()
            (local-set-key (kbd "ESC q") 'format-bash-code)))

;; M-x package-vc-install https://github.com/jdtsmith/eglot-booster
(use-package eglot-booster
  :after eglot
  :config (eglot-booster-mode))

(defun eglot-ensure-if-valid-project ()
  (let ((current-project (project-current)))
    (if current-project
      (let ((root (project-root current-project)))
        (if (and root (not (equal root (expand-file-name "~/"))))
          ;; If root is valid and not ~, proceed with eglot-ensure
          (progn (message "Starting eglot: " root) (eglot-ensure))
          ;; Otherwise, do nothing (or notify the user)
          (message "No valid project root found or root is ~, not starting eglot.")))
      (message "No current project, not starting eglot."))))

(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(python-mode . ("pyright-langserver" "--stdio")))

  (add-hook 'python-mode-hook #'eglot-ensure-if-valid-project)
  (add-hook 'c-mode-hook #'eglot-ensure-if-valid-project)
  (add-hook 'c++-mode-hook #'eglot-ensure-if-valid-project)

  (defvar eglot-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "a") #'eglot-code-actions)
      (define-key map (kbd "d") #'eglot-find-declaration)
      (define-key map (kbd "g") #'eglot-find-typeDefinition)
      map))
  :bind-keymap ("C-c l" . eglot-keymap))

;; (use-package lsp-mode
;;   :commands (lsp lsp-deferred)
;;   :hook ((python-mode . lsp-deferred)
;;          (c-mode . lsp-deferred)
;;           (c++-mode . lsp-deferred))
;;   :init (setq lsp-keymap-prefix "C-c l")
;;   :config (setq lsp-headerline-breadcrumb-enable nil)
;;   :bind (:map lsp-mode-map
;;          ("ESC q" . lsp-format-region)))

;; (use-package lsp-ui
;;   :commands lsp-ui-mode
;;   :config
;;   (setq lsp-ui-sideline-enable t)
;;   (setq lsp-ui-doc-enable t)
;;   (setq lsp-ui-doc-position 'bottom))

(use-package treesit-auto
  :config (global-treesit-auto-mode))

(use-package vertico
  :config (vertico-mode 1))

(use-package orderless
  :config (setq completion-styles '(orderless)))

;; yaml-mode: Major mode for editing YAML files
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

;; which-key: Displays available keybindings in popup
(use-package which-key
  :config (which-key-mode))

;; magit: Git interface for Emacs
(use-package magit)

(use-package company
  :config
  (setq company-completion-styles '(basic))

  (defun company-add-empty-completion (candidates)
    "Add an empty completion at the beginning of the candidates list."
    (if (null candidates)
        (list (propertize "" 'company-prefix (company-grab-symbol)))
      (cons (propertize "" 'company-prefix (company-grab-symbol)) candidates)))

  (advice-add 'company-calculate-candidates :filter-return #'company-add-empty-completion)
  (setq company-selection-wrap-around t)
  (setq company-global-modes '(not shell-mode eshell-mode term-mode vterm-mode))
  (global-company-mode))

(use-package fzf
  :config
  (setq fzf/args "-x --color bw --print-query --margin=1,0 --no-hscroll"
        fzf/executable "fzf"
        fzf/git-grep-args "-i --line-number %s"
        fzf/grep-command "grep -nriI ''")
  ;; Advise fzf to use display-line-numbers-mode instead of linum-mode
  (defun fzf-with-line-numbers (orig-fun &rest args)
    (let ((display-line-numbers t))
      (apply orig-fun args)))
  (advice-add 'fzf/start :around #'fzf-with-line-numbers))

(use-package ripgrep)

;; rainbow-delimiters: Highlight delimiters such as parentheses, brackets or braces according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package doom-modeline
  :config

  (defun short-system-name ()
    (car (split-string (system-name) "\\.")))

  (doom-modeline-def-segment system-name
    "Displays the system name on the left side of the modeline."
    (propertize (short-system-name) 'face 'doom-modeline-buffer-file))

  ;; Create a custom modeline that includes the system name
  (doom-modeline-def-modeline 'main
    '(eldoc bar workspace-name window-number modals matches follow buffer-info
      remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus
      github debug repl lsp minor-modes input-method indent-info buffer-encoding
      major-mode process vcs check time system-name))

  (doom-modeline-mode 1)

  :custom-face
  (mode-line ((t (:background "#4a4a4a" :foreground "white"))))
  (mode-line-inactive ((t (:background "#353535" :foreground "gray")))))

;; Custom function to close window or exit Emacs
(defun close-this-window ()
  (interactive)
  (if (> (length (frame-list)) 1)
      (delete-frame)
    (save-buffers-kill-emacs)))
(global-set-key (kbd "C-x C-c") 'close-this-window)

;; Whitespace mode configuration
(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)

;; Always display which function we're in.
(which-function-mode 1)

(defun xref-or-ffap ()
  "Go to xref or ffap if on an include line."
  (interactive)
  (if (and (or (eq major-mode 'c-mode)
               (eq major-mode 'c++-mode))
           (save-excursion
             (beginning-of-line)
             (looking-at-p "^#include")))
      (call-interactively #'ffap)
    (call-interactively #'xref-find-definitions)))

(use-package cc-mode
  :config
  (define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file)
  (define-key c-mode-base-map (kbd "M-.") 'xref-or-ffap))

(defun c-mode-hook-common ()
  (c-set-offset 'substatement-open 0)
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  (setq indent-tabs-mode nil))

(use-package projectile
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-root-files '(".git"))
  (setq projectile-project-search-path '(("~/" . 1) ("~/src" . 1)))
  (add-to-list 'projectile-ignored-projects (expand-file-name "~/"))
  :bind
  ("C-x p" . projectile-command-map)
  ("C-x p C-f" . counsel-projectile-find-file)
  ("C-x C-p" . projectile-command-map)
  ("C-x C-p C-f" . counsel-projectile-find-file))


;; C mode hook for indentation
(add-hook 'c-mode-hook 'c-mode-hook-common)
(add-hook 'c++-mode-hook 'c-mode-hook-common)

;; Define .dv files as C++ header files
(add-to-list 'auto-mode-alist '("\\.dv\\'" . c++-mode))

;; Zoom fonts with Ctrl+mouse wheel
(global-set-key [C-mouse-4] 'text-scale-increase)
(global-set-key [C-mouse-5] 'text-scale-decrease)

;; Python configuration: never use tabs
(setq-default indent-tabs-mode nil)
(add-hook 'python-mode-hook
          (lambda ()
            (setq-default indent-tabs-mode nil)
            (setq-default tab-width 4)
            (setq-default python-indent-offset 4)))

;; Additional useful configurations
(setq inhibit-startup-message t)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(tool-bar-mode -1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq visible-bell nil)
(display-time)
(setq scroll-error-top-bottom t)
(setq lisp-indent-offset 2)

(column-number-mode)

(dolist (mode '(term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda ()
                   (display-line-numbers-mode 0))))


;; Make ESC quit prompts
;; (global-set-key (kbd "<escape>") 'keyboard-escape-quit)

;; Set up recent files
(recentf-mode 1)
(setq recentf-max-menu-items 25)
(setq recentf-max-saved-items 25)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)

;; Save history
(savehist-mode 1)
(setq history-length 25)

;; Remember and restore the last cursor location of opened files
(save-place-mode 1)

;; Move customization variables to a separate file and load it
(setq custom-file (locate-user-emacs-file "custom-vars.el"))
(load custom-file 'noerror 'nomessage)

;; Don't warn for following symlinked files
(setq vc-follow-symlinks t)

;; Backup settings
(setq backup-directory-alist '(("." . "~/.backups")))
(setq backup-by-copying t)
(setq delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)

;; Autosave
(setq auto-save-file-name-transforms
    `((".*" ,(concat user-emacs-directory "auto-save/") t)))

;; Lockfile settings
(defun my-make-lock-file-name (filename)
  "Create a lock file name for FILENAME, handling both local and TRAMP paths."
  (if (file-remote-p filename)
      ;; Handle TRAMP remote paths
      (let* ((vec (tramp-dissect-file-name filename))
             (user (tramp-file-name-user vec))
             (host (tramp-file-name-host vec)))
        (tramp-make-tramp-file-name user host (concat user-emacs-directory "lock-files/")))
    ;; For local files, use the default directory
    (concat user-emacs-directory "lock-files/" (file-name-nondirectory filename))))

(setq make-lock-file-name 'my-make-lock-file-name)

;; Enable auto-revert mode globally
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq truncate-lines t)

(let ((default-directory  (concat user-emacs-directory "site-lisp/")))
  (normal-top-level-add-subdirs-to-load-path))

(require 'shell-file)
(shell-file-define-global-keys (current-global-map) "\C-z")
(shell-file-define-minor-mode-keys "\C-z")

;; Allow "y"/"n" answers
(setq use-short-answers t)

;; Shadow everything after ~ the same way that / works.
(defun substitute-tilde-in-paths (orig-fun &rest args)
  "Replace everything up to the last '~' with '~' in the file path."
  (let ((path (car args)))
    (if (string-match "~" path)
        (let ((new-path (replace-regexp-in-string "^.*~" "~" path)))
          (apply orig-fun (list new-path)))
      (apply orig-fun args))))

;; Add the advice to 'substitute-in-file-name'
(advice-add 'substitute-in-file-name :around #'substitute-tilde-in-paths)

;; Do not set a background color in terminal mode, makes copying bad.
(unless window-system (set-face-attribute 'default nil :background "unspecified-bg"))

;; (defun my-prog-mode-hook ()
;;   (local-set-key (kbd "M-/") 'comment-or-uncomment-region))
;;
;; (add-hook 'prog-mode-hook 'my-prog-mode-hook)

(let ((local (concat user-emacs-directory "local.el")))
  (when (file-exists-p local)
    (load local)))
