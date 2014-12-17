;;; prog-config.el
;;; Programming modes

;;; Rainbow delimiters: colour pmatching pairs of parentheses etc
;;; Makes sense to turn this on wherever we have paredit mode enabled
(global-prettify-symbols-mode 1)


(require 'rainbow-delimiters)
;;; Colours are a bit too subtle by default so saturate them
(require 'cl-lib)
(require 'color)
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 30)))







;;; Javascript


(require 'js2-mode)
(require 'js-comint)
;; Use node as our repl
(setq inferior-js-program-command "node")
(setenv "NODE_NO_READLINE" "1")
(setq inferior-js-program-command "/usr/bin/java org.mozilla.javascript.tools.shell.Main")
(add-hook 'js2-mode-hook '(lambda () 
			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
			    (local-set-key "\C-cb" 'js-send-buffer)
			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
			    (local-set-key "\C-cl" 'js-load-file-and-go)
			    ))

(require 'flymake-jslint)
(add-hook 'js2-mode-hook 'flymake-jslint-load)

;;; Emacs Lisp
(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)


(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))


;;; Lisp

(setq inferior-lisp-program "sbcl")
(load (expand-file-name "~/quicklisp/slime-helper.el"))

;; Add hook to assorted lisp modes
(mapc (lambda (hook) (add-hook hook 'paredit-mode))
      '(clojure-mode-hook
	slime-repl-mode-hook
	emacs-lisp-mode-hook
	ielm-mode-hook))

(when (locate-library "paredit")
  (autoload 'paredit-mode "paredit"
    "Turn on pseudo-structural editing of Lisp code."
    t)

  ;; By default paredit-splice-sexp is bount to M-s which stomps on a lot
  ;; of useful key bindings, so fix that.  Similarly the `\' behaviour is
  ;; weird.
  (eval-after-load 'paredit
    '(progn 
       (define-key paredit-mode-map "\M-s" nil) 
       (define-key paredit-mode-map "\C-cs" 'paredit-splice-sexp)
       (define-key paredit-mode-map "\\" nil))))




;;; Clojure
(unless (package-installed-p 'cider)
  (package-install 'cider))

(add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)


;;; Python
(package-initialize)
(elpy-enable)
(setq python-check-command "epylint")

;; puppet
(when (locate-library "puppet-mode")
  (autoload 'puppet-mode "puppet-mode")
  (eval-after-load 'puppet-mode
    '(progn 
      (add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode)))))



;;; Shell script mode
  ;; I use C-c ? for magit-status, but shell mode binds it sh-show-inden

(eval-after-load 'sh-script
  '(progn
     (define-key sh-mode-map "\C-c?" nil)))


