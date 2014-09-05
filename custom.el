(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(TeX-view-program-selection
   (quote
    (((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "xpdf")
     (output-html "xdg-open"))))
 '(ansi-color-names-vector
   ["#272822" "#F92672" "#A6E22E" "#E6DB74" "#66D9EF" "#FD5FF0" "#A1EFE4" "#F8F8F2"])
 '(browse-url-browser-function (quote eww-browse-url))
 '(browse-url-generic-program "/Users/grc/bin/conkeror")
 '(canlock-password "7eb9fe24d5c7742671cba67f3eba35dd151d0cdc")
 '(compilation-message-face (quote default))
 '(custom-enabled-themes (quote (grc)))
 '(custom-safe-themes
   (quote
    ("479eba125f9e97a0208b642a99eee1d816fa208fe3a06f73e444504beb0b17f7" "144c32b0a2f17b78943bbb86217758ba6e62d75dd0623799679d2d08a008aa08" "7ed6913f96c43796aa524e9ae506b0a3a50bfca061eed73b66766d14adfa86d1" default)))
 '(fci-rule-color "#49483E")
 '(gnus-ignored-from-addresses (quote ("giles@jujutsu\\.org\\.uk" "giles@pexip\\.com")))
 '(highlight-changes-colors (quote ("#FD5FF0" "#AE81FF")))
 '(highlight-tail-colors
   (quote
    (("#49483E" . 0)
     ("#67930F" . 20)
     ("#349B8D" . 30)
     ("#21889B" . 50)
     ("#968B26" . 60)
     ("#A45E0A" . 70)
     ("#A41F99" . 85)
     ("#49483E" . 100))))
 '(magit-diff-use-overlays nil)
 '(org-agenda-files
   (quote
    ("~/org/home.org" "~/org/calendar.org" "~/org/notes.org")))
 '(org-entities-user
   (quote
    (("space" "\\ " nil " " " " " " " ")
     ("grcbreak" "\\\\" nil "" "" "" ""))))
 '(org-latex-pdf-process
   (quote
    ("lualatex -shell-escape -interaction nonstopmode -output-directory %o %f" "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f" "lualatex -shell-escape -interaction nonstopmode -output-directory %o %f")))
 '(org-latex-to-pdf-process (quote ("LATEX='pdflatex -shell-escape' texi2dvi %f")))
 '(syslog-debug-face
   (quote
    ((t :background unspecified :foreground "#A1EFE4" :weight bold))))
 '(syslog-error-face
   (quote
    ((t :background unspecified :foreground "#F92672" :weight bold))))
 '(syslog-hour-face (quote ((t :background unspecified :foreground "#A6E22E"))))
 '(syslog-info-face
   (quote
    ((t :background unspecified :foreground "#66D9EF" :weight bold))))
 '(syslog-ip-face (quote ((t :background unspecified :foreground "#E6DB74"))))
 '(syslog-su-face (quote ((t :background unspecified :foreground "#FD5FF0"))))
 '(syslog-warn-face
   (quote
    ((t :background unspecified :foreground "#FD971F" :weight bold))))
 '(weechat-color-list
   (quote
    (unspecified "#272822" "#49483E" "#A20C41" "#F92672" "#67930F" "#A6E22E" "#968B26" "#E6DB74" "#21889B" "#66D9EF" "#A41F99" "#FD5FF0" "#349B8D" "#A1EFE4" "#F8F8F2" "#F8F8F0"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(erc-input-face ((t (:foreground "yellow"))))
 '(error ((t (:foreground "red1" :weight bold))))
 '(eshell-prompt ((t (:foreground "chartreuse" :weight normal))))
 '(font-lock-function-name-face ((t (:foreground "aquamarine"))))
 '(org-todo ((t (:foreground "gold" :weight bold))))
 '(region ((t (:background "CadetBlue4")))))
