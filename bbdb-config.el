;;;; BBDB 
;;;; Configure for BBDB v3
;;;  git://git.savannah.nongnu.org/bbdb.git



;; You have to run `make install' to generate bbdb-loadefs.  By default that
;; install to /usr/local/share/emacs/site-lisp

(add-to-list 'load-path "/usr/local/share/emacs/site-lisp")
(require 'bbdb-loaddefs)
(require 'subr-x)

(bbdb-initialize 'gnus 'message)

(bbdb-mua-auto-update-init)

(setq bbdb-default-country "UK")

(setq bbdb-file "~/.emacs.d/bbdb")           


;;; Extracting info from BBDB for form letters etc
;;; use the textmerg package and the following LaTeX invocation:
;;;
;; \Fields{\firstname\surname-\address}
;; \Merge{people.dat}{%
;; form letter
;; }
;;
;; The '-' after \surname means that address may or may not be present

(defun grc-pexip-employees ()
  (interactive)
  (insert (string-join
           (mapcar #'grc-bbdb-name-address
                   (bbdb-search (bbdb-records) nil "Pexip Ltd"))
           "\n")))
(defun grc-pexip-ltd-names ()
  (interactive)
  (insert (string-join
           (mapcar #'grc-bbdb-full-name
                   (bbdb-search (bbdb-records) nil "Pexip Ltd"))
           "\n")))


(defun grc-pexip-ukrd ()
  "Return records for all Pexip UK R&D people.
R&D memberships is defined by the presence of the ukrd mail alias."
  (interactive)
  (bbdb-search (bbdb-records) nil nil nil
               (cons 'mail-alias "ukrd")))


(defun grc-pexip-rd ()
  (bbdb-search (bbdb-records) nil nil nil
                                 (cons 'group "r&d")))



(defun grc-pexip-employees-postcodes ()
  (interactive)
  (insert (string-join
           (mapcar #'grc-home-postcode-as-string
                   (bbdb-search (bbdb-records) nil "Pexip Ltd"))
           "\n")))

(defun grc-home-address-p (address)
  (string= "home" (bbdb-address-label address)))

(defun grc-bbdb-home-address (record)
  "Return the home address field if present, nil otherwise"
  (car (remove-if-not #'grc-home-address-p
                      (bbdb-record-address record))))


(defun grc-home-postcode (record)
  (let ((home (grc-bbdb-home-address record)))
    (if home
        (bbdb-address-postcode home))))

(defun grc-home-postcode-as-string (record)
  (let ((home (grc-bbdb-home-address record)))
    (if home
        (string-join
         (list
          (bbdb-address-postcode home))))))

(defun grc-home-address-as-string (record)
  (let ((home (grc-bbdb-home-address record))
         (latex-line-break "\\\\"))
    (if home
        (string-join
         (list
          (string-join (bbdb-address-streets home) latex-line-break)
          (bbdb-address-city home)
          (bbdb-address-postcode home))
         latex-line-break))))

(defun grc-insert-name-address (name)
  "Insert name and address from BBDB"
  (interactive "sName to search for: ")
  (let ((recs(bbdb-search (bbdb-records) name)))
    (assert (= 1 (length recs)) nil "Too many records found.")
    (insert (grc-bbdb-name-address (car recs)))))


(defun grc-pexip-address-book ()
  (interactive)
  (mapcar #'grc-bbdb-addressbook-entry
          (grc-pexip-employees)))

(defun grc-bbdb-name-address (record)
    "Return a string containing the name and home address of the given record.
String is suitable for use as data with the LaTeX textmerg package"
    (assert (not (listp record)) nil "You have passed a list where a single record was expected.")
    (let ((given-name (bbdb-record-firstname record))
          (surname (bbdb-record-lastname record))
          (home-address (grc-home-address-as-string record)))
      (string-join (list given-name
                         surname
                         home-address
                         " ") ;to give us a blank line
                   "\n")))

(defun grc-bbdb-addressbook-entry (record)
  (let ((given-name (bbdb-record-firstname record))
          (surname (bbdb-record-lastname record))
          (home-address (grc-home-address-as-string record))
          (insert (string-join '("foo"
                                 given-name
                                 " "
                                 surname
                                 "\n"
                                 home-address
                                 "\n"))))))




(defun grc-bbdb-full-name (record)
  "Return a string containing the full name"
    (assert (not (listp record)) nil "You have passed a list where a single record was expected.")
    (let ((given-name (bbdb-record-firstname record))
          (surname (bbdb-record-lastname record)))
      (string-join (list given-name
                         surname)
                   " ")))



(defun grc-update-email-contact ()
  "Updates records of all recipients with the time and subject of the email contact. 
Intended to be called from message-send-hook"
  (interactive)
  (let 
      ((annotation  (concat
                     "<"
                     (current-time-string)
                     "> "
                     (message-fetch-field "subject"))))
    (bbdb-mua-annotate-recipients annotation
                                  "contacted"
                                  nil
                                  'update)))

(add-to-list 'bbdb-layout-alist '(short-email
                                  (order  mail)
                                  (primary . t)
                                  (toggle . t)))


(defun bbdb-display-record-short-email (record layout fields)
  (let ((copy (copy-sequence record)))
    (bbdb-record-set-field copy 'organization '(""))
    (bbdb-display-record-one-line copy
                                  layout
                                  fields)))
