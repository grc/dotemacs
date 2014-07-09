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
 '(browse-url-browser-function (quote eww-browse-url))
 '(browse-url-generic-program "/Users/grc/bin/conkeror")
 '(canlock-password "7eb9fe24d5c7742671cba67f3eba35dd151d0cdc")
 '(gnus-ignored-from-addresses (quote ("giles@jujutsu\\.org\\.uk" "giles@pexip\\.com")))
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
 '(send-mail-function (quote smtpmail-send-it)))
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
