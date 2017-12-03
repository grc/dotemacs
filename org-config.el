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

     (require 'org-capture)
     (require 'org-agenda)
     
     (setq org-agenda-span 8)
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
              (file "~/org/devnotes.org")
              "\n\n* %^{Title}    %^g\n%u\n\n%?"
              :empty-lines 1)

             ("s" "Sarah" entry
              (file "~/org/sarah.org")
              "\n\n* %^{Title}    %^g\n%u\n\n%?"
              :empty-lines 1)

             ("w" "World accorging" entry 
              (file "wag.org")
                "\n\n* %^{Title}    %^g\n%u\n\n%?"
              :empty-lines 1)
              ))


     (setq org-highlight-latex-and-related '(latex))

     (setq org-default-notes-file (concat org-directory "/notes.org"))

     (setq org-log-done 'time)

     (setq org-latex-compiler "lualatex")
     ;; Make org-table mode available in email
     (add-hook 'message-mode-hook 'turn-on-orgtbl)

     (setq org-babel-tangle-lang-exts (quote (("clojure" . "clj") ("emacs-lisp" . "el") ("python" . "py"))))

     (org-babel-do-load-languages
      'org-babel-load-languages
      '((emacs-lisp . t)
        (python . t)
        (shell . t))) 

     (setq org-src-preserve-indentation t)

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




;; org-reveal presentations
(require 'ox-reveal)
(setq org-reveal-root "file://../reveal.js-3.4.1")

;; Org Publishing

(setq org-publish-project-alist
      '(("jujutsu" 
         :base-directory "~/homers/jujutsu/org-based-site"
         :publishing-directory "~/homers/jujutsu/org-exported-site"
         :publishing-function org-html-publish-to-html)))
