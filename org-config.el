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

     ;; ;; org and yasnippet workaround
     ;; (defun yas/org-very-safe-expand ()
     ;;   (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))

     ;; (add-hook 'org-mode-hook
     ;;           (lambda ()
     ;;             (make-variable-buffer-local 'yas/trigger-key)
     ;;             (setq yas/trigger-key [tab])
     ;;             (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
     ;;             (define-key yas/keymap [tab] 'yas/next-field)))


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
     

     (setq org-todo-keywords
           '((sequence "TODO" "|" "DONE" "CANCELLED")))

))


;;; Simple calendar sync using external shell script

(defun grc-org-update-calendar ()
       (interactive)
       (start-process "update-org-calendar"
                      nil
                      "/Users/grc/bin/update-org-calendar"))


(defvar grc-org-calendar-update-timer 
       (run-with-timer 1 (* 20 60) 'grc-org-update-calendar )
       "Timer used to refresh org calendar from google calendar.")



(defun grc-org-insert-essay-template ()
  (interactive)
  (insert "#+TITLE: 
          #+DATE: 
          #+AUTHOR: 
          #+DESCRIPTION: Thoughts on how to grow a software development team          
          #+OPTIONS: ':nil *:t -:t ::t <:t H:3 \\n:nil ^:t arch:headline
          #+OPTIONS: author:t c:nil creator:comment d:(not \"LOGBOOK\") date:t
          #+OPTIONS: e:t email:nil f:t inline:t num:nil p:nil pri:nil stat:t
          #+OPTIONS: tags:t tasks:t tex:t timestamp:t toc:nil todo:t |:t
          #+CREATOR: Emacs 25.0.50.1 (Org mode 8.2.10)
          
          #+EXCLUDE_TAGS: noexport
          #+LANGUAGE: en
          #+SELECT_TAGS: export
          #+OPTIONS: texht:t
          #+LATEX_CLASS: article
          #+LATEX_CLASS_OPTIONS: [a4paper, 11pt]
          #+LATEX_HEADER_EXTRA: \\usepackage{fontspec} \\defaultfontfeatures{Ligatures=TeX} \\setmainfont{Calibri}
          #+LATEX_HEADER_EXTRA: \\usepackage{microtype} \\usepackage{parskip}
          #+LATEX_HEADER_EXTRA: \\renewcommand{\\bfdefault}{b}")
          


;;; Exported projects
          




  (setq org-publish-project-alist
        '(("jujutsu-content"
           :auto-sitemap nil
           :base-directory "~/homers/jujutsu/newwebsite/src"
           :publishing-directory "~/homers/jujutsu/newwebsite/published"
           :publishing-function org-html-publish-to-html
           :html-head "<link rel='stylesheet' type='text/css' href='http://www.jujutsu.org.uk/jujutsu.css' />"
           :html-preamble "<h1 id='site-id'>Daiwa Ryu Jujutsu <span id='tagline'>a traditional martial art</span></h1><div id='menu'><a href='/'>Home</a><a href='/classdetails/'>Class details</a><a href='/articles/'>Articles</a></div> '"
           :html-postamble ""
           :recursive t
           :make-index t)
          ("jujutsu-static"
           :base-directory "~/homers/jujutsu/newwebsite/src"
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory "~/homers/jujutsu/newwebsite/published"
           :recursive t
           :publishing-function org-publish-attachment)
          ("jujutsu"
           :components ("jujutsu-content" "jujutsu-static"))))
)
