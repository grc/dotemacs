(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("2878517f049b28342d7a360fd3f4b227086c4be8f8409f32e0f234d129cee925" "2a3ffb7775b2fe3643b179f2046493891b0d1153e57ec74bbe69580b951699ca" "0fe9f7a04e7a00ad99ecacc875c8ccb4153204e29d3e57e9669691e6ed8340ce" "2d1fe7c9007a5b76cea4395b0fc664d0c1cfd34bb4f1860300347cdad67fb2f9" "a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(doom-modeline-mode t)
 '(helm-completing-read-handlers-alist
   '((describe-function . helm-completing-read-symbols)
     (describe-variable . helm-completing-read-symbols)
     (describe-symbol . helm-completing-read-symbols)
     (debug-on-entry . helm-completing-read-symbols)
     (find-function . helm-completing-read-symbols)
     (disassemble . helm-completing-read-symbols)
     (trace-function . helm-completing-read-symbols)
     (trace-function-foreground . helm-completing-read-symbols)
     (trace-function-background . helm-completing-read-symbols)
     (find-tag . helm-completing-read-default-find-tag)
     (org-capture . helm-org-completing-read-tags)
     (org-set-tags . helm-org-completing-read-tags)
     (ffap-alternate-file)
     (tmm-menubar)
     (find-file)
     (find-file-at-point . helm-completing-read-sync-default-handler)
     (ffap . helm-completing-read-sync-default-handler)
     (execute-extended-command)
     (bbdb-create)))
 '(inhibit-splash-screen t)
 '(inhibit-startup-screen t)
 '(org-agenda-files
   '("/Users/grc/org/devnotes.org" "/Users/grc/org/devnotes2.org" "/Users/grc/org/home.org" "/Users/grc/org/musings.org" "/Users/grc/org/notes.org"))
 '(org-html-head-include-default-style nil)
 '(org-html-head-include-scripts nil)
 '(package-selected-packages
   '(fancy-narrow all-the-icons-dired all-the-icons-gnus doom-themes doom-modeline all-the-icons exec-path-from-shell org-web-tools jinja2-mode wgrep logview blacken smart-mode-line powerline artbollocks-mode flycheck graphviz-dot-mode org2blog helm-projectile sudo-edit helm-ebdb blimp yaml-mode org-gcal yasnippet-classic-snippets bbdb define-word slime f3 dash go-mode shell-pop spacemacs-theme cider google-this which-key pp-c-l dired-narrow use-package tc spotlight smartparens slack skewer-mode restclient projectile pdf-tools ox-reveal origami org-plus-contrib magit helpful helm-dash haskell-mode gnuplot crux auctex))
 '(python-shell-completion-native-enable nil)
 '(safe-local-variable-values
   '((grc-meta . "fubar")
     (org-beamer-outline-frame-title . "Topics")
     (eval setq-local org-html-postamble nil)
     (eval setq-local lob
           (concat site-root "lob.inc"))
     (eval setq-local site-root
           (locate-dominating-file default-directory ".dir-locals.el"))
     (eval setq-local project-path
           (locate-dominating-file default-directory ".dir-locals.el"))
     (eval setq-local project-path
           (locate-dominating-file defualt-directory ".dir-locals.el"))
     (eval message "Project directory set to `%s'." project-path)
     (eval setq-local project-path
           (file-name-directory
            (let
                ((d
                  (dir-locals-find-file ".")))
              (if
                  (stringp d)
                  d
                (car d)))))
     (eval message "Project directory set to `%s'." my-project-path)
     (eval set
           (make-local-variable 'my-project-path)
           (file-name-directory
            (let
                ((d
                  (dir-locals-find-file ".")))
              (if
                  (stringp d)
                  d
                (car d)))))
     (site-root
      (eval nth 1
            (s-split " "
                     (pwd))))
     (site-root eval
                (nth 1
                     (s-split " "
                              (pwd))))
     (site-root nth 1
                (s-split " "
                         (pwd)))
     (org-confirm-babel-evaluate))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-comment-face ((t (:foreground "burlywood"))))
 '(font-lock-function-name-face ((t (:foreground "deep sky blue"))))
 '(org-todo ((t (:foreground "OrangeRed1" :weight bold))))
 '(slack-message-output-header ((t (:foreground "#FFA000" :underline nil :weight normal :height 1.0)))))
