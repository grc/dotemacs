;;;; BBDB 
;; Note that when building BBDB from scratch from CVS you need to do a 
;; `make autoloads' before 'make'

(require 'bbdb)
(bbdb-initialize 'gnus 'message)
(add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus)
(bbdb-insinuate-message)

(setq bbdb-file "~/.emacs.d/bbdb")           ;; keep ~/ clean; set before loading


;; This block was initially lifted from
;; http://emacs-fu.blogspot.co.uk/2009/08/managing-e-mail-addresses-with-bbdb.html
(setq 
    bbdb-offer-save 1                        ;; 1 means save-without-asking
    bbdb-use-pop-up nil                        ;; allow popups for addresses
    bbdb-electric-p nil                        ;; be disposable with SPC
    bbdb-popup-target-lines  1               ;; very small
    bbdb-dwim-net-address-allow-redundancy t ;; always use full name
    bbdb-quiet-about-name-mismatches t 
    bbdb-always-add-address t                ;; add new addresses to existing...
                                             ;; ...contacts automatically
    bbdb-canonicalize-redundant-nets-p t     ;; x@foo.bar.cx => x@bar.cx
    bbdb-completion-type nil                 ;; complete on anything
    bbdb-complete-name-allow-cycling t       ;; cycle through matches
                                             ;; this only works partially
    bbbd-message-caching-enabled t           ;; be fast
    bbdb-elided-display t                    ;; single-line addresses

    ;; auto-create addresses from mail
    bbdb/news-auto-create-p 'bbdb-ignore-some-messages-hook   
    bbdb-ignore-some-messages-alist ;; don't ask about fake addresses
    ;; NOTE: there can be only one entry per header (such as To, From)
    ;; http://flex.ee.uec.ac.jp/texi/bbdb/bbdb_11.html

    '(( "From" . "no.?reply\\|DAEMON\\|daemon\\|facebook\\|twitter\\|linkedin\\|github")))



(setq bbdb-auto-notes-alist
      '(("Organization" (".*" company 0))))
