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

(use-package eglot
  :hook ((python-mode . eglot-ensure)
         (c-mode . eglot-ensure)
         (c++-mode . eglot-ensure))
  :bind (("ESC q" . eglot-format))
  :config
  (setq eglot-confirm-server-initiated-edits nil))

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
  (setq company-show-numbers t)
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

;; rainbow-delimiters: Highlight delimiters such as parentheses, brackets or braces according to their depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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

(use-package cc-mode
  :config
  (define-key c-mode-base-map (kbd "C-c o") 'ff-find-other-file))

(defun c-mode-hook-common ()
  (c-set-offset 'substatement-open 0)
  (setq c-basic-offset 4)
  (setq c-indent-level 4)
  (setq indent-tabs-mode nil))

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
;; (global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

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

;; Lockfile settings
(setq lock-file-name-transforms
      '((".*" "~/.emacs.d/lock-files/" t)))

;; Enable auto-revert mode globally
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq truncate-lines t)

(let ((default-directory  "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'shell-file)
(shell-file-define-global-keys (current-global-map) "\C-z")
(shell-file-define-minor-mode-keys "\C-z")

;; Get rid of that annoying prompt that requires one to type
 ;; in YES and then press the enter key to confirm.
(defun yes-or-no-p (PROMPT)
   (beep)
   (y-or-n-p PROMPT))

(setq fixed-default-directory
  (cond
    ((file-exists-p "~/ai") "~/ai")
    (t nil)))

(when fixed-default-directory
  (add-hook 'find-file-hook
    (lambda () (setq-local default-directory fixed-default-directory)))
  (when (file-exists-p (file-name-concat fixed-default-directory ".git"))
    (global-set-key (kbd "C-x C-f") 'fzf-git-files)))
