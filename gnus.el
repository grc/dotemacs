(require 'nnir)

(setq gnus-check-new-newsgroups nil)
(setq gnus-always-read-dribble-file t)
(setq gnutls-min-prime-bits 727) 	; back to the gnutls default

(setq gnus-summary-line-format "%U%R%z%d%I%(%[%4L: %-23,23f%]%) %s\n")


;; (setq gnus-select-method
;;       '(nnimap "newwork"
;;                                   (nnimap-stream network)
;;                                   (nnimap-port 143)
;;                                   (nnimap-address "127.0.0.1")
;;                                   (nnimap-record-commands t)))


;; (setq gnus-select-method
;;       '(nnimap "gmail"
;; 	       (nnimap-address "imap.gmail.com")  ; it could also be imap.googlemail.com if that's your server.
;; 	       (nnimap-server-port "imaps")
;; 	       (nnimap-stream ssl)))

(setq gnus-select-method
      '(nnimap "jujutsu"
                 (nnimap-stream network)
                 (nnimap-port 143)
                 (nnimap-address "127.0.0.1")))

(setq gnus-secondary-select-methods
      '((nnimap "workacct"
                (nnimap-stream network)
                (nnimap-port 143)
                (nnimap-address "127.0.0.1"))))

(add-hook 'gnus-group-mode-hook 'gnus-topic-mode)

(setq message-forward-as-mime nil)


;; Mail aliases.  Type e.g. ukrd SPC to get all the UK R&D team.

(add-hook 'message-setup-hook 'bbdb-mail-aliases)





;;; gnus-alias to select an identity to post as


(require 'gnus-alias)
(setq gnus-alias-identity-alist 
      '(("workacct"
	 nil
	 "giles@pexip.com"
	 "Pexip"
	 nil
	 nil
	 "Giles Chamberlin\nMobile: +44 (0)7801 972920")
	("jujutsu"
	 nil
	 "giles@jujutsu.org.uk"
	 nil
	 nil
         "Giles")
	("sjld"
	 nil
	 "giles@sjld.co.uk"
	 nil
	 ""
	 "Giles")))

(gnus-alias-init)
(setq gnus-alias-default-identity "workacct")
(define-key message-mode-map [f8] 'gnus-alias-select-identity)



(setq mm-discouraged-alternatives '("text/html" "text/richtext"))



;;; ical integration 


;(add-to-list 'load-path "~/git-repos/ical-event")
;(require 'gnus-calendar)
;(gnus-calendar-setup)
    
;; to enable optional iCalendar->Org sync functionality
;; NOTE: both the capture file and the headline(s) inside must already exist
;(setq gnus-calendar-org-capture-file "~/org/notes.org")
;(setq gnus-calendar-org-capture-headline '("Calendar"))
;(gnus-calendar-org-setup)

(setq gnus-treat-mail-gravatar 'head)



;;; Check for attachments
;;; Taken from http://www.emacswiki.org/emacs/GnusAttachmentReminder
(defun check-attachments-attached () 
  (interactive) 
  (save-excursion 
    (goto-char (point-min)) 
    (let* ( 
           ;; Nil when message came from outside (eg calling emacs as editor) 
           ;; Non-nil marker of end of headers. 
           (internal-messagep 
            (re-search-forward 
             (concat "^" (regexp-quote mail-header-separator) "$") nil t)) 
           (end-of-headers              ; Start of body. 
            (copy-marker 
             (or internal-messagep 
                 (re-search-forward "^$" nil t) 
                 (point-min)))) 
           (limit  
            (or (re-search-forward "^-- $" nil t)  
                (point-max)))               
           (old-case-fold-search case-fold-search)) 
      (unwind-protect 
          (progn 
            (goto-char end-of-headers)
            ; TODO: Could add a check for ":whitespace:> " to prevent
            ; false positives on quoted text.  Had a few of those
            ; recently.
            (when (re-search-forward "attach" limit t) 
              (goto-char end-of-headers) 
              ;; the word 'attach' has been used, can we find an 
              ;; attachment? 
              (unless  
                  (or (re-search-forward "^<#/" limit t) 
                      (y-or-n-p 
                       "No attachment. Send the message? ") 
                      (error "no message sent"))))) 
        (set-marker end-of-headers nil)))))  

(add-hook 'message-send-hook 'check-attachments-attached) 


;;; Hard new lines
;;; Idea is to use hard new lines for return key so
;;; that emails can be sent as flowed.  We display the hard new lines
;;; so that code excerpts etc can be fixed up if needed before
;;; sending.
(defun my-mark-hard-newlines (beg end &rest _ignore)
  (interactive (list (point-min) (point-max)))
  (save-excursion
    (goto-char beg)
    (while (search-forward "\n" end t)
       (let ((pos (1- (point))))
         (if (get-text-property pos 'hard)
             ;; Use `copy-sequence', because display property values must not be `eq'!
             (add-text-properties pos (1+ pos)
                                  (list 'display (copy-sequence "↵\n")))
           (remove-text-properties pos (1+ pos) '(displaynil)))))))

(defun my-use-and-mark-hard-newlines ()
  (interactive)
  (use-hard-newlines)
  (add-hook 'after-change-functions 'my-mark-hard-newlines nil  t))

;; I've gone off hard new lines for the moment - replies are too hard
;; to structure with quoted text.

;(with-eval-after-load "message"
;  (add-hook 'message-mode-hook 'my-use-and-mark-hard-newlines))



(autoload 'cite-cite "cite" "A simple cite function for Emacs" nil)
(setq message-cite-function 'cite-cite)

 ;; Demons
 
(require 'gnus-demon)
(gnus-demon-add-handler 'gnus-demon-scan-mail 10 2)


;; Pretty icons for gnus
;(require 'all-the-icons-gnus)
;(all-the-icons-gnus-setup)



;; Local Variables:
;; mode: emacs-lisp
;; End:
