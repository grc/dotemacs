;;; mail-config.el
;;; Emacs configuration of all things email related
;;; all that is, except gnus, whose init file is declared below


;;; Use the latest gnus if available
(add-to-list 'load-path "~/git-repos/gnus/lisp")
(add-to-list 'Info-directory-list "~/git-repos/gnus/texi")


(setq user-mail-address "giles@jujutsu.org.uk")

(setq gnus-init-file "~/.emacs.d/gnus.el")

;; Authentication credentials are held on .authinfo

(require 'message)
;; In order to use gmail's smtp server, I needed to install gnutls on OSX.
(setq message-insert-canlock nil
      message-send-mail-function 'smtpmail-send-it
      smtpmail-default-smtp-server "smtp.gmail.com"
      smtpmail-smtp-server "smtp.gmail.com"
      smtpmail-smtp-service 587
      smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil)))






;;; Setting appropriate smtp-user
;; gmail authentication means that the from header in an email is
;; overwritten with the email address associated with the account
;; you're using to send that mail.  So we want to be able to change
;; the credentials used to log in to gmail depending on where the
;; email is meant to come from.
;;
;; authinfo can contain multiple machine lines, each with a different
;; user name and password.  `smtpmail-smtp-user' will use a set of
;; credentials corresponding to the given user name.  So in
;; `message-send-hook' we set `smtpmail-smtp-user' to an appropriate
;; value depending on who we're sending as.


(defun grc-from-field ()
  (interactive)
  (save-excursion
    (save-restriction
      (message-narrow-to-headers-or-head)
      (message-fetch-field "From"))))

(defun grc-smtp-user ()
  (interactive)
  (let ((from-field (grc-from-field)))
    (cond 
     ((string-match "jujutsu" from-field) "gileschamberlin@gmail.com")
     ((string-match "pexip" from-field) "giles@pexip.com"))))

(defun grc-set-smtp-user ()
  (setq smtpmail-smtp-user (grc-smtp-user))
  (message "Setting smtp-user to %s" smtpmail-smtp-user))

(add-hook 'message-send-hook 'grc-set-smtp-user)
