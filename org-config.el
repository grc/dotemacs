;;; org-config.el --- Personalised org mode configuration

;;; Commentary:
;; Personalised configuration for org mode.  Covers mostly capture
;; templates and the jujutsu website which is exported as HTML from an
;; org mode project.  Org2blog configuration is handled by a separate
;; file.


;;; Code:

(message "Initialising org-mode")

(setq use-package-verbose t)

(use-package org
 
 
  
  :bind
  (("\C-ca" . org-agenda)
   ("\C-cb" . org-iswitchb)
   ("\C-cl" . org-store-link)
   ("\C-cc" . org-capture))
  
  :config
    
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (gnuplot . t)
     (python . t)
     (shell . t)))

  (setq org-babel-python-command "python3")


;; New LaTeX backend that parses LATEX_HEADER lines, allowing macro replacement in them.
  (org-export-define-derived-backend 'grc-latex 'latex
    :options-alist '(( :latex-header-extra "LATEX_HEADER" nil nil parse)
                     (:latex-header-extra "LATEX_HEADER_EXTRA" nil nil parse))
    :menu-entry ''(?j "Giles' latex" org-latex-export-to-pdf))

  

  ;; See  https://zge.us.to/emacs-style.html#fn10 for advice on not using setq in :config
  :custom
  (org-babel-tangle-lang-exts '(("clojure" . "clj")
                                ("emacs-lisp" . "el")
                                ("python" . "py")))
  (org-capture-templates                
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

     ("f" "Financial notes" entry
      (file "~/personal/financial/fin-notes.org")
      "\n\n* %^{Title}    %^g\n%u\n\n%?"
      :empty-lines 1)

     ("i" "Investigation task" entry
      (file "~/org/investigate.org")
      "\n\n** TODO %^{Title?}\n%?"
      :empty-lines 1)

     
     ("w" "Work" entry
      (file "~/org/worknotes.org")
      "\n\n* %^{Title}    %^g\n%u\n\n%?"
      :empty-lines 1)

     ("s" "Sarah" entry
      (file "~/org/sarah.org")
      "\n\n* %^{Title}    %^g\n%u\n\n%?" :empty-lines 1)

     ("h" "Handover" entry
      (file "~/org/handover.org")
      "\n\n* %^{Title}    \n%u\n\n%?" :empty-lines 1)
     
     
     ("p" "Protocol" entry
      (file "~/org/capture.org")
      "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?")
     
     ("L" "Protocol Link" entry
      (file "~/org/capture.org")
      "* %? [[%:link][%:description]] \nCaptured On: %U"))) 

  (org-default-notes-file "~/org/notes.org")

  (org-log-done 'time)
  (org-highlight-latex-and-related '(latex))
  (org-html-html5-fancy   t)
  (org-latex-compiler "lualatex")
                                        ; Get rid of the hyperref  config so I can use  LATEX_HEADER blocks to  specify via \hypersetup
  (org-latex-hyperref-template nil)
  (org-publish-project-alist
   '(("jj-org-files"
      :auto-sitemap t
      :base-directory "~/personal/jujutsu/org-site/src"
      :html-doctype "html5"
      :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/bootstrap.css\" />"
      :html-head-include-default-style nil
      :html-head-include-scripts nil
      :html-postamble nil
      :publishing-directory "~/Sites/jujutsu"
      :publishing-function org-jujutsu-site-publish-to-html
      :recursive t
      :with-toc nil
      :section-numbers nil)
     ("jj-css"
      :base-directory "~/personal/jujutsu/org-site/src/css"
      :base-extension "css"
      :html-style nil
      :publishing-directory  "~/Sites/jujutsu/css"
      :publishing-function org-publish-attachment)
     ("jj-images"
      :base-directory "~/personal/jujutsu/org-site/src/images"
      :base-extension any
      :publishing-directory  "~/Sites/jujutsu/images"
      :publishing-function org-publish-attachment)
     ("jj-htaccess"
      :base-directory "~/personal/jujutsu/org-site/src"
      :base-extension "htaccess"
      :publishing-directory  "~/Sites/jujutsu"
      :publishing-function org-publish-attachment)

     ("jujutsu"
      :components ("jj-org-files" "jj-css" "jj-images" "jj-htaccess"))))
  (org-todo-keywords '((sequence "TODO" "|" "DONE" "CANCELLED"))))

;; (use-package ox-beamer
;;   :after org-mode)

;; (use-package org-protocol)
;; (use-package helm-org-rifle
;;   :ensure t)


;; (use-package org-gcal
;;   :after org-mode
;;   :config
;;   (setq org-gcal-client-id (first (netrc-credentials "gcal"))
;;         org-gcal-client-secret (second (netrc-credentials "gcal"))
;;         org-gcal-file-alist '(("giles@pexip.com" .  "~/org/schedule.org")))
;;   ;(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
;;   ;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
;;   )

;; (defun grc-org-update-calendar ()
;;        (interactive)
;;        (start-process "update-org-calendar"
;;                       nil
;;                       "/Users/grc/bin/update-org-calendar"))


;; (defvar grc-org-calendar-update-timer
;;        (run-with-timer 1 (* 20 60) 'grc-org-update-calendar )
;;        "Timer used to refresh org calendar from google calendar.")

;; (defvar grc-org-gcal-refresh
;;        (run-with-timer 1 (* 20 60) 'org-gcal-refesh-token )
;;        "Timer used to refresh org calendar from google calendar.")


  

;; Org Publishing

;; The jujutsu org exporter is kept out of the normal elisp tree and
;; can be found iin the jujutsu web site directory structure.
                                        ; 
(require 'ox-jujutsu-site nil t)

(setq org-publish-project-alist
      '(("jj-org-files"
         :auto-sitemap t
         :base-directory "~/personal/jujutsu/org-site/src"
         :html-doctype "html5"
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/bootstrap.css\" />"
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-postamble nil
         :publishing-directory "/var/www/jujutsu"
         :publishing-function org-jujutsu-site-publish-to-html
         :recursive t
         :with-toc nil
         :section-numbers nil)
        ("jj-css"
         :base-directory "~/personal/jujutsu/org-site/src/css"
         :base-extension "css"
         :html-style nil
         :publishing-directory  "/var/www/jujutsu/css"
         :publishing-function org-publish-attachment)
        ("jj-js"
         :base-directory "~/personal/jujutsu/org-site/src/js"
         :base-extension "js"
         :html-style nil
         :publishing-directory  "/var/www/jujutsu/js"
         :publishing-function org-publish-attachment)
        ("jj-images"
         :base-directory "~/personal/jujutsu/org-site/src/images"
         :base-extension any
         :publishing-directory  "/var/www/jujutsu/images"
         :publishing-function org-publish-attachment)
        ("jj-htaccess"
         :base-directory "~/personal/jujutsu/org-site/src"
         :base-extension "htaccess"
         :publishing-directory  "/var/www/jujutsu"
         :publishing-function org-publish-attachment)

        ("jujutsu"
         :components ("jj-org-files" "jj-css" "jj-js" "jj-images" "jj-htaccess"))))
                                        
                                        



;;; Org Latex Export

;; Allow line breaks in URLs
;(add-to-list 'org-latex-default-packages-alist "\\PassOptionsToPackage{hyphens}{url}")


(setq org-latex-hyperref-template nil)



;; Keep bibliography, zettelkasten and basic org directory on Kakapo
(if (string= system-name "kakapo")
    (setq grc-kakapo-home "~/")
  (setq grc-kakapo-home    "/System/Volumes/Data/mnt/Resources/grc-kakapo/"))


;; These are all relative to grc-kakapo-home
(setq grc-brain-dir "study/brain"
      grc-bibliography-dir "study/bibliography/"
      grc-bibliography-file "references.bib"
      grc-bib-notes "study/bibliography/notes"
      grc-bib-search-dirs '((concat grc-kakapo-home grc-bibliography-dir)))

;; org-roam
(use-package org-roam
  :ensure t

  :after org
  
  :preface
  (setq org-roam-v2-ack t)

  :custom
  (org-roam-directory (concat grc-kakapo-home grc-brain-dir))

  :bind
  ("C-c n l" . org-roam-buffer-toggle)
  ("C-c n f" . org-roam-node-find)

  (:map org-mode-map
        (("C-c n i" . org-roam-node-insert)))

  :config
  (setq org-roam-capture-templates '(("d" "default" plain "%?"
                                      :if-new (file+head "${slug}.org"
                                                         "#+TITLE: ${title}\n#+DATE: %T\n")
                                      :unnarrowed t)))
  
  ;; this sets up various file handling hooks so your DB remains up to date
  (org-roam-setup))


(setq bibtex-dialect 'biblatex)

(use-package ebib
  :ensure t
  
  :config
  (setq ebib-bibtex-dialect 'biblatex)
  (setq ebib-notes-directory (concat grc-kakapo-home grc-bib-notes))
  (setq ebib-bib-search-dirs '((concat grc-kakapo-home grc-bib-search-dirs)))
  (setq ebib-preload-bib-files '(grc-bibliography-file)))


(use-package helm-bibtex
  :ensure t
  :config
  (setq bibtex-completion-bibliography (concat grc-kakapo-home grc-bibliography-dir grc-bibliography-file))
  (setq bibtex-completion-notes-path  (concat grc-kakapo-home grc-bib-notes)))

(provide 'org-config)

;;; org-config.el ends here
