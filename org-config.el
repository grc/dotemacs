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

     (setq org-log-done 'time)


     ;; Make org-table mode available in email
     (add-hook 'message-mode-hook 'turn-on-orgtbl)

     (setq org-babel-tangle-lang-exts (quote (("clojure" . "clj") ("emacs-lisp" . "el"))))

     (org-babel-do-load-languages
      'org-babel-load-languages
       '((emacs-lisp . t))) 

     (setq org-src-preserve-indentation t)

     ;; yasnippet interop as described in
     ;; http://capitaomorte.github.io/yasnippet/faq.html Note that the
     ;; variants on emacswiki fail with newer yasniipets as the naming
     ;; scheme changed from yas/ to yas-
     (add-hook 'org-mode-hook
          (let ((original-command (lookup-key org-mode-map [tab])))
            `(lambda ()
               (setq yas-fallback-behavior
        	     '(apply ,original-command))
               (local-set-key [tab] 'yas-expand))))


     ;; Beamer in org 8 and later
     (require 'ox-beamer)

     (setq org-todo-keywords
           '((sequence "TODO" "|" "DONE" "CANCELLED")))

     ))  ;; end of progn & eval after load


;;; Simple calendar sync using external shell script

(defun grc-org-update-calendar ()
       (interactive)
       (start-process "update-org-calendar"
                      nil
                      "/Users/grc/bin/update-org-calendar"))


(defvar grc-org-calendar-update-timer 
       (run-with-timer 1 (* 20 60) 'grc-org-update-calendar )
       "Timer used to refresh org calendar from google calendar.")





