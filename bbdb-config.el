;;;; BBDB 
;;;; Configure for BBDB v3
;;;  git://git.savannah.nongnu.org/bbdb.git

(require 'bbdb-loaddefs)
(bbdb-initialize 'gnus 'message)

(bbdb-mua-auto-update-init)

(setq bbdb-default-country "UK")

(setq bbdb-file "~/.emacs.d/bbdb")           


;;; Extracting info from BBDB for form letters etc

(defun grc-pexip-employees ()
  (interactive)
  (insert (string-join
           (mapcar #'grc-bbdb-name-address
                   (bbdb-search (bbdb-records) nil "Pexip Ltd"))
           "\n")))

(defun grc-home-address (record)
  (let* ((addrs (bbdb-record-address record))
         (home (car (remove-if-not (lambda (a) (string= "home" (bbdb-address-label a)))
                                   addrs)))
         (latex-line-break "\\\\"))
    (if home
        (string-join
         (list
          (string-join (bbdb-address-streets home) latex-line-break)
          (bbdb-address-city home))
         latex-line-break))))

(defun grc-bbdb-name-address (record)
  "Return a string containing the name and home address of the given record.
String is suitable for use as data with the LaTeX textmerg package"
  
  (let ((given-name (bbdb-record-firstname record))
        (surname (bbdb-record-lastname record))
        (home-address (grc-home-address record)))
    (string-join (list given-name
                       surname
                       home-address
                       " ") ;to give us a blank line
                 "\n")))





