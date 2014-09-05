(message "Initialising org-mode")


;;; Functions which will cause org to be loaded
(autoload 'org "org")
(autoload 'org-agenda "org")
(autoload 'org-store-link "org")
(autoload 'org-iswitchb "org")
(autoload 'org-capture "org")

;;; Things which invoke those function
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(define-key global-map "\C-cc" 'org-capture)


;;; After loading org-mode
(eval-after-load 'org
  '(progn 
    
     (setq org-agenda-span 'day)
     (setq org-use-sub-superscripts nil)

     ;; Capture template
     (setq org-capture-templates
           '(("t" "Scheduled task" entry
              (file "")
              "\n\n** TODO %^{Title?}\nSCHEDULED: <%<%Y-%m-%d %a>>\nContext: %a\n%?"
              :Empty-lines 1)
             ("n" "Note" entry
              (file "")
              "\n\n**  %?\n"
              :Empty-lines 1)
             ("d" "Dev notes" entry
              (file "~/org/devnotes.org"
                    "\n\n* %^{Title}\n%?"))
             ("w" "" entry ;; 'w' for 'org-protocol'
              (file+headline "www.org" "Notes")
              "* %^{Title}\n\n  Source: %u, %c\n\n  %i")))


     (setq org-default-notes-file (concat org-directory "/notes.org"))

                                        ; enable logging
     (setq org-log-done 'time)


     ;; Make org-table mode available in email
     (add-hook 'message-mode-hook 'turn-on-orgtbl)

     (require 'footnote)
     (add-hook 'message-mode-hook 'footnote-mode)






     (setq org-babel-tangle-lang-exts (quote (("clojure" . "clj") ("emacs-lisp" . "el"))))

     (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t))) 

     (setq org-src-preserve-indentation t)

     ;; org and yasnippet workaround
     (defun yas/org-very-safe-expand ()
       (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

     (add-hook 'org-mode-hook
               (lambda ()
                 (make-variable-buffer-local 'yas/trigger-key)
                 (setq yas/trigger-key [tab])
                 (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
                 (define-key yas/keymap [tab] 'yas/next-field)))


     ;; Beamer in org 8 and later
     (require 'ox-beamer)



     ;; mobile org
     (require 'org-mobile)
     ;; Set to the name of the file where new notes will be stored
     (setq org-mobile-inbox-for-pull "~/org/flagged.org")
     ;; Set to <your Dropbox root directory>/MobileOrg.
     (setq org-mobile-directory "/Volumes/dav")

     ;; Enable encryption-decryption
     (setq org-mobile-use-encryption t)
     

     

;;; Org Trello integration

     (require 'dash)

     (require 'request)








     (require 'noflet)
     (require 'fakir)
     (require 'elnode)
     (require 's)
     (require 'web)
     (require 'db)

     (require 'org-trello)))


;;; Simple calendar sync using external shell script

(defun grc-org-update-calendar ()
       (interactive)
       (start-process "update-org-calendar"
                      nil
                      "/Users/grc/bin/update-org-calendar"))


(defvar grc-org-calendar-update-timer 
       (run-with-timer 1 (* 20 60) 'grc-org-update-calendar )
       "Timer used to reresh org calendar from google calendar.")

;;; Exported projects

(setq org-publish-project-alist
      '(("jujutsu"
         :base-directory "~/homers/org-expt/src"
         :publishing-directory "~/homers/org-expt/website"
         :publishing-function org-html-publish-to-html
         :make-index t
         )))
