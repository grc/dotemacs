(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default))
 '(org-agenda-files
   '("/Users/grc/org/agenda.org" "/Users/grc/org/appointments.org" "/Users/grc/org/chef.org" "/Users/grc/org/dev-leads.org" "/Users/grc/org/devnotes.org" "/Users/grc/org/devnotes2.org" "/Users/grc/org/distance-calc.org" "/Users/grc/org/endpoint.org" "/Users/grc/org/facebook.org" "/Users/grc/org/fun-links.org" "/Users/grc/org/gdpr.org" "/Users/grc/org/guildford.org" "/Users/grc/org/home.org" "/Users/grc/org/janet.org" "/Users/grc/org/journal.org" "/Users/grc/org/musings.org" "/Users/grc/org/new-mac.org" "/Users/grc/org/new-office.org" "/Users/grc/org/notes.org" "/Users/grc/org/org-config.org" "/Users/grc/org/personal-todo.org" "/Users/grc/org/release-process.org" "/Users/grc/org/sarah.org" "/Users/grc/org/wag.org" "/Users/grc/org/zsh.org"))
 '(package-selected-packages
   '(flycheck graphviz-dot-mode org2blog helm-projectile sudo-edit helm-ebdb blimp yaml-mode org-gcal yasnippet-classic-snippets bbdb define-word slime f3 dash go-mode shell-pop spacemacs-theme cider google-this which-key pp-c-l dired-narrow use-package tc spotlight smartparens slack skewer-mode restclient projectile pdf-tools ox-reveal origami org-plus-contrib magit helpful helm-dash haskell-mode gnuplot crux auctex))
 '(python-shell-completion-native-enable nil)
 '(safe-local-variable-values
   '((eval setq-local org-html-postamble nil)
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
 '(font-lock-function-name-face ((t (:foreground "deep sky blue"))))
 '(org-todo ((t (:foreground "OrangeRed1" :weight bold)))))
