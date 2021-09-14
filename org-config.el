;;; org-config.el --- Personalised org mode configuration

;;; Commentary:
;; Personalised configuration for org mode.  Covers mostly capture
;; templates and the jujutsu website which is exported as HTML from an
;; org mode project.  Org2blog configuration is handled by a separate
;; file.


;;; Code:

(message "Initialising org-mode")

(setq use-package-verbose t)

(use-package org-mode
  :init
  (add-hook 'message-mode-hook 'turn-on-orgtbl)
  
  :bind
  (("\C-ca" . org-agenda)
   ("\C-cb" . org-iswitchb)
   ("\C-cl" . org-store-link)
   ("\C-cc" . org-capture))
  
  :config
  (setq 
        org-src-window-setup ‘current-window)
  
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (gnuplot . t)
     (python . t)
     (shell . t)))

  (setq org-babel-python-command "python3")

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

:mode "\\.org\\'"

(use-package ox-beamer
  :after org-mode)

(use-package org-protocol)
(use-package helm-org-rifle)


(use-package org-gcal
  :after org-mode
  :config
  (setq org-gcal-client-id (first (netrc-credentials "gcal"))
        org-gcal-client-secret (second (netrc-credentials "gcal"))
        org-gcal-file-alist '(("giles@pexip.com" .  "~/org/schedule.org"))))

;; Org Publishing
(require 'ox-jujutsu-site)

(setq org-publish-project-alist
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
                                        
                                        



;;; Org Latex Export

;; Allow line breaks in URLs
(add-to-list 'org-latex-default-packages-alist "\\PassOptionsToPackage{hyphens}{url}")


(setq org-latex-hyperref-template nil)




;; org-roan
(use-package org-roam
  :after org
  
  :custom
  (org-roam-directory "~/brain")
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



;; (use-package org-ref
;;   :config
;;   (setq reftex-default-bibliography '("~/bibliography/references.bib"))

;;         ;; see org-ref for use of these variables
;;         (setq org-ref-bibliography-notes "~/bibliography/notes.org"
;;               org-ref-default-bibliography '("~/bibliography/references.bib")
;;               org-ref-pdf-directory "~/bibliography/bibtex-pdfs/"))



(setq grc-bib-notes  "~/bibliography/notes")
(setq grc-bibliography  "~/bibliography/references.bib")

(use-package ebib
  :config
  (setq ebib-bibtex-dialect 'biblatex)
  (setq ebib-notes-directory grc-bib-notes)
  (setq ebib-bib-search-dirs '("~/bibliography"))
  (setq ebib-preload-bib-files '("references.bib")))


(use-package helm-bibtex
  :config
  (setq bibtex-completion-bibliography grc-bibliography)
  (setq bibtex-completion-notes-path  grc-bib-notes))

(provide 'org-config)

;;; org-config.el ends here
