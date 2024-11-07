(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
    ["#242730" "#ff665c" "#7bc275" "#FCCE7B" "#51afef" "#C57BDB" "#5cEfFF"
      "#bbc2cf"])
 '(custom-safe-themes
    '("7e377879cbd60c66b88e51fad480b3ab18d60847f31c435f15f5df18bdb18184"
       "013728cb445c73763d13e39c0e3fd52c06eefe3fbd173a766bfd29c6d040f100"
       "1f292969fc19ba45fbc6542ed54e58ab5ad3dbe41b70d8cb2d1f85c22d07e518"
       "81f53ee9ddd3f8559f94c127c9327d578e264c574cda7c6d9daddaec226f87bb"
       "ffafb0e9f63935183713b204c11d22225008559fa62133a69848835f4f4a758c"
       "9f297216c88ca3f47e5f10f8bd884ab24ac5bc9d884f0f23589b0a46a608fe14"
       "88f7ee5594021c60a4a6a1c275614103de8c1435d6d08cc58882f920e0cec65e"
       "d6b934330450d9de1112cbb7617eaf929244d192c4ffb1b9e6b63ad574784aad"
       default))
 '(display-time-default-load-average nil)
 '(display-time-mode t)
 '(eldoc-echo-area-use-multiline-p nil)
 '(shell-file-dir "~/.emacs.d/shell-file-outputs")
 '(exwm-floating-border-color "#1c1f24")
 '(fci-rule-color "#62686E")
 '(highlight-tail-colors
    ((("#2c3636" "#99bb66" "green") . 0)
      (("#293b44" "#46D9FF" "cyan") . 20)))
 '(ispell-dictionary nil)
 '(jdee-db-active-breakpoint-face-colors (cons "#1c1f24" "#51afef"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1c1f24" "#7bc275"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1c1f24" "#484854"))
 '(objed-cursor-color "#ff665c")
 '(package-selected-packages
    '(bash-lsp clang-format company doom-modeline fzf lsp-bash lsp-shell
       magit orderless projectile rainbow-delimiters ripgrep shfmt
       treesit-auto vertico which-key yaml-mode))
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#242730"))
 '(rustic-ansi-faces
    ["#242730" "#ff665c" "#7bc275" "#FCCE7B" "#51afef" "#C57BDB" "#5cEfFF"
      "#bbc2cf"])
 '(safe-local-variable-values '((eval shell-file-mode t)))
 '(scroll-conservatively 100)
 '(shfmt-arguments '("-i" "4"))
 '(vc-annotate-background "#242730")
 '(vc-annotate-color-map
    (list (cons 20 "#7bc275") (cons 40 "#a6c677") (cons 60 "#d1ca79")
      (cons 80 "#FCCE7B") (cons 100 "#f4b96e") (cons 120 "#eda461")
      (cons 140 "#e69055") (cons 160 "#db8981") (cons 180 "#d082ae")
      (cons 200 "#C57BDB") (cons 220 "#d874b0") (cons 240 "#eb6d86")
      (cons 260 "#ff665c") (cons 280 "#d15e59") (cons 300 "#a35758")
      (cons 320 "#754f56") (cons 340 "#62686E") (cons 360 "#62686E")))
 '(vc-annotate-very-old-color nil))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :background "black" :foreground "white" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight medium :height 140 :width normal :foundry "xos4" :family "terminus"))))
 '(doom-modeline-time ((t (:foreground "cyan" :inherit doom-modeline))))
 '(region ((t (:extend t :background "#505050")))))
