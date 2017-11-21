(require 'package)
(package-initialize)

(defun install-package (pkgname)
  (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
  (package-refresh-contents)
  (package-install pkgname))

;; (install-package 'flycheck)
(require 'flycheck)

(let ((default-directory  "~/.emacs.d/site-lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; buildifier
(let ((buildifier-path "~/.local/go/bin/buildifier"))
  (if buildifier-path
      (add-hook 'after-save-hook
              (lambda()
                (if (string-match "BUILD" (file-name-base (buffer-file-name)))
                    (progn
                      (shell-command (concat buildifier-path " " (buffer-file-name)))
                      (find-alternate-file (buffer-file-name))))))))


;(ido-mode 1)

;; paste at cursor not at mouse point
(setq mouse-yank-at-point t)

(setq major-mode 'text-mode)

(autoload 'gnuserv-start "gnuserv-compat"
  "Allow this Emacs process to be a server for client processes."
  t)

(autoload 'd-mode "d-mode")

;(when (null terminal-frame)
;  (require 'gnuserv)
;  ;;(setq gnuserv-frame (selected-frame))
;  (gnuserv-start))

(set-default 'auto-mode-alist
 	     (append '(("\\.sc$" . scheme-mode)
                       ("\\.ml[iylp]?$" . tuareg-mode)
                       ("\\.d$" . d-mode)
                       ("\\.m$" . matlab-mode)) ; more likely than Obj-C
                     auto-mode-alist))
;(add-hook 'caml-mode-hook (lambda () (require 'caml-font)))
;(require 'caml-font)
(setq save-abbrevs t)
;(autoload 'caml-mode "caml" "Major mode for editing Caml code." t)
;(autoload 'run-caml "inf-caml" "Run an inferior Caml process." t)


;(load-library "column-marker")
;(add-hook 'scheme-mode-hook (lambda () (interactive) (column-marker-1 80)))

(if t
    (progn
      (autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
      (autoload 'run-scheme "cmuscheme" "Switch to interactive Scheme buffer." t)
      (defvar scheme-program-name "gosh"))
  (progn
    (add-to-list 'load-path "/usr/share/emacs/site-lisp/slime/")
    (add-to-list 'load-path "~/.emacs.d/site-lisp/slime48/")
    (autoload 'slime "slime"
      "Start an inferior^_superior Lisp and connect to its Swank server."
      t)
    (autoload 'slime-mode "slime"
      "SLIME: The Superior Lisp Interaction Mode for Emacs (minor-mode)."
      t)
    (eval-after-load "slime"
      '(progn
         (slime-setup)
         (setq slime-lisp-implementations
               `((s48 ("scheme48") :init slime48-init-command)
                 ,@slime-lisp-implementations))))
    (autoload 'slime48-init-command "slime48"
      "Return a string to initialize Scheme48 running under SLIME.")
    ;;(setq slime-space-information-p nil)
    (add-hook 'scheme-mode-hook (lambda () (slime-mode +1)))))


(add-hook 'before-save-hook 'delete-trailing-whitespace)
(add-hook 'scheme-mode-hook (lambda () (require 'quack)))

(autoload 'latex-mode "tex-site" "Load Auctex." t)

;(load-library "gosh.el")

;(load "/home/yury/src/lush/etc/lush.el")

;(autoload 'gambit-inferior-mode "gambit" "Hook Gambit mode into cmuscheme.")
;(autoload 'gambit-mode "gambit" "Hook Gambit mode into scheme.")
;(add-hook 'inferior-scheme-mode-hook (function gambit-inferior-mode))
;(add-hook 'scheme-mode-hook (function gambit-mode))
;(setq scheme-program-name "gsi -:d-")

(setq inhibit-startup-message t)
(display-time)
(line-number-mode t)

;; put backups in ~/.backups
(add-to-list 'backup-directory-alist
             (cons "." "~/.backups"))
(setq version-control t)
(setq delete-old-versions t)
(setq kept-new-versions 6)
(setq kept-old-versions 2)

;; (if (equal window-system nil)
;;     (menu-bar-mode nil)
;;   (progn
;;     (setq browse-url-browser-function 'browse-url-mozilla)
;;     (setq browse-url-mozilla-program "opera")))

;; call last keyboard C-x e -> f5
(global-set-key [f5] 'kmacro-end-and-call-macro)

 ;; Get rid of that annoying prompt that requires one to type
 ;; in YES and then press the enter key to confirm.
(defun yes-or-no-p (PROMPT)
   (beep)
   (y-or-n-p PROMPT))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(battery-mode-line-format " [%b%p%%,%d°C]")
 '(case-fold-search t)
 '(column-number-mode t)
 '(current-language-environment "English")
 '(display-battery-mode t)
 '(filladapt-turn-on-mode-hooks (quote (lisp-mode-hook)))
 '(frame-background-mode nil)
 '(global-flycheck-mode t)
 '(global-font-lock-mode t nil (font-lock))
 '(global-undo-tree-mode nil)
 '(gnus-secondary-select-methods
   (quote
    ((nnimap "gmail"
             (nnimap-address "imap.gmail.com")
             (nnimap-server-port 993)
             (nnimap-stream ssl)))))
 '(gnus-secondary-servers (quote ("news.optonline.net" "news.nyu.edu")))
 '(gnus-select-method (quote (nntp "news.optonline.net")))
 '(gnus-treat-display-smileys nil)
 '(home-end-enable t)
 '(indent-tabs-mode nil)
 '(lisp-tag-indentation 2)
 '(mail-host-address "nyu.edu")
 '(menu-bar-mode nil)
 '(message-send-mail-function (quote smtpmail-send-it))
 '(mouse-wheel-mode t nil (mwheel))
 '(package-selected-packages (quote (flycheck)))
 '(quack-default-program "gosh")
 '(quack-fontify-style (quote emacs))
 '(quack-fontify-threesemi-p nil)
 '(quack-pltish-fontify-definition-names-p t)
 '(quack-pltish-fontify-keywords-p t)
 '(quack-pretty-lambda-p nil)
 '(quack-programs
   (quote
    ("scm" "bigloo" "csi" "csi -hygienic" "gosh" "gsi" "gsi ~~/syntax-case.scm -" "guile" "kawa" "mit-scheme" "mred -z" "mzscheme" "mzscheme -M errortrace" "rs" "scheme" "scheme48" "scsh" "sisc" "stklos" "sxi")))
 '(quack-smart-open-paren-p nil)
 '(read-file-name-completion-ignore-case t)
 '(rmail-summary-scroll-between-messages t)
 '(safe-local-variable-values (quote ((eval shell-file-mode t))))
 '(scroll-bar-mode (quote right))
 '(scroll-conservatively 100)
 '(scroll-step 0)
 '(show-paren-mode t)
 '(text-mode-hook (quote (flyspell-mode text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(transient-mark-mode t)
 '(user-mail-address "yury.sulsky@gmail.com"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 100 :width normal :foundry "xos4" :family "Terminus"))))
 '(custom-group-tag-face-1 ((t (:inherit variable-pitch :foreground "pink" :weight bold :height 180 :family "terminus"))) t)
 '(highlight-current-line-face ((t (:background "lightgoldenrodyellow"))))
 '(keywiz-command-face ((t (:inherit (quote variable-pitch) :foreground "Blue" :weight bold :height 1.2 :family "terminus"))))
 '(keywiz-heading-face ((t (:inherit (quote variable-pitch) :weight bold :height 1.5 :family "terminus"))))
 '(region ((((class color) (background dark)) (:background "midnight blue"))))
 '(widget-inactive ((((class grayscale color) (background dark)) (:foreground "dark gray")))))

(setq truncate-lines t)

(set (make-local-variable lisp-indent-function)
             'common-lisp-indent-function)


(add-hook 'c-mode-hook (lambda ()
                         (require 'cc-mode)
                         (c-set-offset 'substatement-open 0)))


;; bind C-c C-t to start a shell (use .emacs_shellname for the shells rc file)
(global-set-key "\C-c\C-t" 'eshell)
(global-set-key "\C-r" 'isearch-backward-regexp)
(global-set-key "\C-s" 'isearch-forward-regexp)
(global-set-key "\M-\C-s" 'query-replace-regexp)
(global-set-key "\C-xg" 'goto-line)
(global-set-key (read-kbd-macro "<M-DEL>") 'backward-kill-word)

(defun close-this-window ()
  (interactive)
  (if multiple-frames
      (delete-frame)
    (save-buffers-kill-emacs)))

(global-set-key "\C-x\C-c" 'close-this-window)


;(t-mouse-mode t)
;(xterm-mouse-mode t)

(require 'whitespace)
(setq whitespace-style '(face empty tabs lines-tail trailing))
(global-whitespace-mode t)


;; git clone git@github.com:ysulsky/shell-file-mode.git
(require 'shell-file)
(global-set-key "\C-z" nil)  ; I never want to stop Emacs.
(shell-file-define-global-keys (current-global-map) "\C-z")
(shell-file-define-minor-mode-keys "\C-z")
(defun shell-file-cd-command (dir)
  (concat "shell-file-cd " dir))


;; compile .emacs
(let* ((init     (file-name-sans-extension user-init-file))
       (init-elc (concat init ".elc"))
       (init-elc-exists-p (file-exists-p init-elc)))
  (when (and init-elc-exists-p (file-newer-than-file-p init user-init-file))
    (delete-file init-elc)
    (setq init-elc-exists-p nil))
  (when (not init-elc-exists-p)
    (byte-compile-file init)))


(provide '.emacs)
;;; .emacs ends here
