;;; org-config.el --- Personalised org mode configuration

;;; Commentary:
;; Personalised configuration for org mode.  Covers mostly capture
;; templates and the jujutsu website which is exported as HTML from an
;; org mode project.  Org2blog configuration is handled by a separate
;; file.


;;; Code:

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

             ("w" "Work" entry
              (file "~/org/worknotes.org")
              "\n\n* %^{Title}    %^g\n%u\n\n%?"
              :empty-lines 1)

             ("s" "Sarah" entry
              (file "~/org/sarah.org")
              "\n\n* %^{Title}    %^g\n%u\n\n%?" :empty-lines 1)))


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
        (gnuplot . t)
        (python . t)
        (shell . t)))

     (setq org-src-preserve-indentation t)

     ;; Beamer in org 8 and later
     (require 'ox-beamer)

     (setq org-todo-keywords
           '((sequence "TODO" "|" "DONE" "CANCELLED")))

     ))  ;; end of progn & eval after load



;;; TODO - replace my external script with org-gcal




(use-package org-gcal
  :ensure t
  :config
                                        ;TODO: Imoort secrets in org-gcal-secrets
  (setq org-gcal-client-id (first (netrc-credentials "gcal"))
        org-gcal-client-secret (second (netrc-credentials "gcal"))
        org-gcal-file-alist '(("giles@pexip.com" .  "~/org/schedule.org")))
  ;(add-hook 'org-agenda-mode-hook (lambda () (org-gcal-sync) ))
  ;(add-hook 'org-capture-after-finalize-hook (lambda () (org-gcal-sync) ))
  )

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




;; org-reveal presentations

;;; Commentary:
;; 

(require 'ox-reveal)
(setq org-reveal-root "file://../reveal.js-3.4.1")

;; Org Publishing

(setq jujutsu-base-dir "~/homers/jujutsu/org-site/src")
(setq jujutsu-publish-dir "~/Sites/jujutsu")
(setq org-publish-project-alist
      '(("jj-org-files"
         :auto-sitemap t
         :base-directory "~/homers/jujutsu/org-site/src"
         :html-head "<link rel=\"stylesheet\" type=\"text/css\" href=\"/css/style1.css\" />"
         :html-postamble "<p>Copyright &copy Giles Chamberlin 1985 - 2019</p>"
         :publishing-directory "~/Sites/jujutsu"
         :publishing-function org-html-publish-to-html
         :recursive t
         :with-toc nil
         :section-numbers nil)
        ("jj-css"
         :base-directory "~/homers/jujutsu/org-site/src/css"
         :base-extension "css"
         :html-style nil
         :publishing-directory  "~/Sites/jujutsu/css"
         :publishing-function org-publish-attachment)
        ("jj-images"
         :base-directory "~/homers/jujutsu/org-site/src/images"
         :base-extension any
         :publishing-directory  "~/Sites/jujutsu/images"
         :publishing-function org-publish-attachment)
        ("jj-htaccess"
         :base-directory "~/homers/jujutsu/org-site/src"
         :base-extension "htaccess"
         :publishing-directory  "~/Sites/jujutsu"
         :publishing-function org-publish-attachment)

        ("jujutsu"
         :components ("jj-org-files" "jj-css" "jj-images" "jj-htaccess"))))




;; Allow <title> tag to be different from the pages <h1> Use
;; #+HTML_HEAD_EXTRA: <title>foo bar</title> to specify the new title,
;; advice and filter below will remove the original one.

(defun my-meta-advice (orig-fun &rest args)
  "Overwrite the <title> element in the html <head> section.
‘grc-html-meta-title’ contains the new value.
ORIG-FUN is expected to be `org-html--build-meta-info'.
ARGS should be a one element list containing the info block."
  (let* ((info (first args))
         (orig-title (plist-get info :title))
         (modified-args (plist-put info :title "delete me"))
         (ret (apply orig-fun (list modified-args))))
    (plist-put info :title orig-title)
    ret))

(advice-add 'org-html--build-meta-info :around #'my-meta-advice)

(defun my-delete-html-title (text backend info)
  "Remove title element.
TEXT is the exported text, BACKEND the backend in use and INFO the communications block."
       (when (org-export-derived-backend-p backend 'html)
         (replace-regexp-in-string "^<title>delete me</title>$" "" text)))


;; Consider using #+BIND in file to allow this functionality to be
;; specified on a file by file basis rather than globally.

(add-to-list 'org-export-filter-final-output-functions 'my-delete-html-title)




(provide 'org-config)

;;; org-config.el ends here
