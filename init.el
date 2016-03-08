;;; init.el --- Emacs init file
;;; Code:
(setq inhibit-splash-screen t)


(require 'server)
(unless (server-running-p)
  (server-start))


;;; Use new .el files in preference to older .elc
(setq load-prefer-newer t)

;;; Set up my load path
(add-to-list 'load-path "~/elisp")

(let ((default-directory "~/elisp"))
  (normal-top-level-add-subdirs-to-load-path))




;;; Buffer naming when visiting several files with the same name
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

(setq-default indent-tabs-mode nil)
(setq visible-bell t)
(put 'narrow-to-region 'disabled nil)
(set-mouse-color "white")





;;; Move to the 21st Century and adopt package management
(require 'package)
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))


;; Signature checking is fairly new to elpa (as of Oct 2014) and I was
;; having trouble installing the basic gnu package so let's disable
;; the checks
(setq package-check-signature nil)

(package-initialize)




;;; For some reason, calc out of the box gives multiplication a higher
;;; precedence than division so 10/2*5 = 1.  This is at variance with
;;; the BODMAS rule I learnt at school. Explanation of the vagaries
;;; can be found at
;;; https://en.wikipedia.org/wiki/Order_of_operations#Exceptions
(setq calc-multiplication-has-precedence nil)

;; Ability to jump through change locations
(require 'goto-chg)
(global-set-key [(control ?.)] 'goto-last-change)
(global-set-key [(control ?,)] 'goto-last-change-reverse)



(require 'hippie-exp)
(global-set-key "\M-/" 'hippie-expand)


;; yasnippet
(require 'yasnippet)
(yas-global-mode)


;;; Repeatable command idea taken from abo-abo:
;;; http://oremacs.com/2015/01/14/repeatable-commands/
(require 'cl)
(defun def-rep-command (alist)
  "Return a lambda that calls the first function of ALIST.
It sets the transient map to all functions of ALIST."
  (lexical-let ((keymap (make-sparse-keymap))
                (func (cdar alist)))
    (mapc (lambda (x)
            (define-key keymap (car x) (cdr x)))
          alist)
    (lambda (arg)
      (interactive "p")
      (funcall func arg)
      (set-transient-map keymap t))))

(global-set-key (kbd "<f2> g")
                (def-rep-command
                    '(("g" . text-scale-increase)
                      ("l" . text-scale-decrease))))
(global-set-key (kbd "<f2> l")
                (def-rep-command
                    '(("l" . text-scale-decrease)
                      ("g" . text-scale-increase))))


;;; zsh, my normal default, doesn't work at all nicely in emacs'
;;; shell: the completion menu isn't displayed, the right hand prompt
;;; fails etc. Use something more conservative if using M-x shell:
(setq explicit-shell-file-name "/bin/bash")


(defalias 'yes-or-no-p 'y-or-n-p)
(require 'ag)                           ; grep on steroids


;;; Dired related functionality

(require 'dired-x)

;; Narrow dired to match a filter.
;; Restore the original buffer with `g'
(use-package dired-narrow
  :ensure t
  :bind (:map dired-mode-map
              ("/" . dired-narrow)))



;;; font-lock-mode warning text has an associated help string,
;;; displayed by default on mouse over. I'd like that echoed in the
;;; minibuffer.
(setq help-at-pt-display-when-idle t)

(require 'flyspell)
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
;; uk dictionary installed via port install aspell-dict-en
;; language customisations occur in .aspell.conf:
;; lang en
;; master en_GB



(defun grc-text-hook ()
  (flyspell-mode)
  (abbrev-mode)
  (auto-fill-mode 1))

(add-hook 'text-mode-hook 'grc-text-hook)



;;; Spelling correction

;;; C-x C-i will use ispell to correct the word at
;;; point, then add it to the list of abbrevs for future automatic
;;; correction.  Use `list-abbrevs' to see how consistently bad your
;;; spelling is.
(define-key ctl-x-map "\C-i" 'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase (or (thing-at-point 'word) "")))
    (unless (string= aft bef)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft))))

(setq save-abbrevs t)
(setq-default abbrev-mode t)





; Mark support in the presence of transient mark mode
; http://www.masteringemacs.org/articles/2010/12/22/fixing-mark-commands-transient-mark-mode/
(defun push-mark-no-activate ()
  "Pushes `point' to `mark-ring' and does not activate the region
Equivalent to \\[set-mark-command] when \\[transient-mark-mode] is disabled"
  (interactive)
  (push-mark (point) t nil)
  (message "Pushed mark to ring"))
(global-set-key (kbd "C-`") 'push-mark-no-activate)


(defun jump-to-mark ()
  "Jumps to the local mark, respecting the `mark-ring' order.
This is the same as using \\[set-mark-command] with the prefix argument."
  (interactive)
  (set-mark-command 1))
(global-set-key (kbd "M-`") 'jump-to-mark)

(defun exchange-point-and-mark-no-activate ()
  "Identical to \\[exchange-point-and-mark] but will not activate the region."
  (interactive)
  (exchange-point-and-mark)
  (deactivate-mark nil))
(define-key global-map [remap exchange-point-and-mark] 'exchange-point-and-mark-no-activate)





; http://emacswiki.org/emacs/InteractivelyDoThings
(require 'ido)
(ido-mode 'both)
(setq ido-auto-merge-delay-time 999)


;;; https://github.com/nonsequitur/smex/
;;; Faster completion for M-p 

(require 'smex)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)







(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)



;;; Version control
(require 'vc)
(require 'vc-git)
(setq vc-follow-symlinks t) ;; don't ask when following symlinks to a controlled file




(eval-after-load 'info 
  '(progn (info-initialize)
	  (add-to-list 'Info-directory-list "~/git-repos.magit")))

;;; Magit
(require 'magit)
(global-set-key (kbd "<f5>") 'magit-status)



;; Sunrise/Sunset stuff
(require 'solar)
(setq calendar-location-name "Warborough, Oxfordshire")
(setq calendar-latitude 51.6 calendar-longitude 1.1) 


(require 'browse-kill-ring)
(browse-kill-ring-default-keybindings)


;;; TRAMP Mode
(require 'tramp)

;; multi hop configuration
(add-to-list 'tramp-default-proxies-alist
	     '(nil "\\`root\\'" "/ssh:%h:"))
(add-to-list 'tramp-default-proxies-alist
	     '((regexp-quote (system-name)) nil nil))
(add-to-list 'tramp-default-proxies-alist
             '("nagios-uk" "root" "/ssh:%h:"))



;; Message mode
(require 'footnote)
(add-hook 'message-mode-hook 'footnote-mode)



;;; eshell

(autoload 'eshell "eshell")
(eval-after-load 'eshell
  '(progn 
     
     (setq eshell-history-size 1024)
     (require 'em-smart)
     (eshell-smart-initialize)

     (defun grc-eshell-last-arg ()
       (last eshell-last-arguments))

     (defun grc-eshell-keys ()
       (local-set-key (kbd "M-.") 'grc-eshell-last-arg))

     (add-hook 'eshell-mode-hook 'grc-eshell-keys )))

;; eshell completion:
(require 'pcmpl-git)
(require 'pcmpl-lein)




;; boxquote provides the ability to add nicely formatted textblocks to
;; emails etc.  If it's not on the system it's not a disaster.
(require 'boxquote nil t) 




;;; find-file-root: opening a file as root

(defvar find-file-root-prefix (if (featurep 'xemacs) "/[sudo/root@localhost]" "/sudo:root@localhost:" )
  "*The filename prefix used to open a file with `find-file-root'.")

(defvar find-file-root-history nil
  "History list for files found using `find-file-root'.")

(defvar find-file-root-hook nil
  "Normal hook for functions to run after finding a \"root\" file.")

(defun find-file-root ()
  "*Open a file as the root user.
   Prepends `find-file-root-prefix' to the selected file name so that it
   maybe accessed via the corresponding tramp method."

  (interactive)
  (require 'tramp)
  (let* ( ;; We bind the variable `file-name-history' locally so we can
	 ;; use a separate history list for "root" files.
	 (file-name-history find-file-root-history)
	 (name (or buffer-file-name default-directory))
	 (tramp (and (tramp-tramp-file-p name)
		     (tramp-dissect-file-name name)))
	 path dir file)

    ;; If called from a "root" file, we need to fix up the path.
    (when tramp
      (setq path (tramp-file-name-localname tramp)
	    dir (file-name-directory path)))

    (when (setq file (read-file-name "Find file (UID = 0): " dir path))
      (find-file (concat find-file-root-prefix file))
      ;; If this all succeeded save our new history list.
      (setq find-file-root-history file-name-history)
      ;; allow some user customization
      (run-hooks 'find-file-root-hook))))

(global-set-key [(control x) (control r)] 'find-file-root)



;;; Video conferencing support

;; (require 'grc-video)
;; (setq grc-video-endpoints
;;     '(("work-snoopy" . "10.44.1.66")))

;; (global-set-key "\C-cv" 'grc-bbdb-video-dial)



(require 'misc)
(define-key global-map [(meta ?z)] 'zap-up-to-char) ; Rebind M-z 



;; multiple cursors
(require 'multiple-cursors)
(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)


;;; popwin.el from git@github.com:m2ym/popwin-el.git
(require 'popwin)
(popwin-mode 1)



;;; Spelling correction
(define-key ctl-x-map "\C-i" 'endless/ispell-word-then-abbrev)

(defun endless/ispell-word-then-abbrev (p)
  "Call `ispell-word'. Then create an abbrev for the correction made.
With prefix P, create local abbrev. Otherwise it will be global."
  (interactive "P")
  (let ((bef (downcase (or (thing-at-point 'word) ""))) aft)
    (call-interactively 'ispell-word)
    (setq aft (downcase (or (thing-at-point 'word) "")))
    (unless (string= aft bef)
      (message "\"%s\" now expands to \"%s\" %sally"
               bef aft (if p "loc" "glob"))
      (define-abbrev
        (if p local-abbrev-table global-abbrev-table)
        bef aft))))

(setq save-abbrevs t)
(setq-default abbrev-mode t)


;;; Use Symbola font for any characters not found in my default font:
;;; TODO not available in emacs 24
;(set-fontset-font "fontset-default" nil 
 ;                 (font-spec :size 20 :name "Symbola"))







;;; Using an Apple magic mouse I'm occasionally generating unwanted
;;; mouse events which confuse emacs:
(dolist (k '([mouse-6] [down-mouse-6] [drag-mouse-6] [double-mouse-6] [triple-mouse-6]
             [mouse-7] [down-mouse-7] [drag-mouse-7] [double-mouse-7] [triple-mouse-7]))
  (global-unset-key k))


;;;; Pexip MCU functionality

(require 'pexip)
(defconst pexip-production "10.47.2.49"
  "Address of management node of production MCU")
(global-set-key "\C-cp" (lambda ()(interactive) (pex-insert-version pexip-production)))



(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)


;;; Keymap stuff heavily inspired by http://endlessparentheses.com/the-toggle-map-and-wizardry.html

(define-prefix-command 'grc/toggle-map)

(define-key ctl-x-map "t" 'grc/toggle-map)

(define-key grc/toggle-map "p" #'paredit-mode)
(define-key grc/toggle-map "r" #'dired-toggle-read-only)
(autoload 'dired-toggle-read-only "dired" nil t)
(define-key grc/toggle-map "w" #'whitespace-mode)



;;; Make it easier to access email
(defun grc-mail ()
  "If gnus already has a group buffer open, switch to it,
otherwise run gnus to create such a buffer."
  (interactive)
  (if (get-buffer "*Group*" )
      (switch-to-buffer "*Group*")
    (gnus)))

(global-set-key (kbd "<f12>") #'grc-mail)



;;; Load in other config

(setq config-dir "~/.emacs.d")

(setq configs '( "auctex-config"
                 "bbdb-config"
                 "erc-config"
                 "mail-config"
                 "org-config"
                 ;"org-blog-config"
                 "prog-config"))




(dolist (config configs)
  (message (format "loading %s" config))
  (load (format "%s/%s" config-dir config)))



;;; Move customisations to their own file
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)









;;; init.el ends here
