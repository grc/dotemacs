;;; prog-config.el --- Personal configuration for programming modes


;;; Rainbow delimiters:
;; colour pmatching pairs of parentheses etc Makes sense to turn this
;; on wherever we have paredit mode enabled

;;; Code:

(global-prettify-symbols-mode 1)
;;; But if point is on the symbol, unprettify it
(customize-set-value 'prettify-symbols-unprettify-at-point 'right-edge)


;;; Commentary:
;; 

(require 'rainbow-delimiters)
;;; Colours are a bit too subtle by default so saturate them
(require 'cl-lib)
(require 'color)
(cl-loop
 for index from 1 to rainbow-delimiters-max-face-count
 do
 (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
   (cl-callf color-saturate-name (face-foreground face) 30)))




;;; Enable hide-show-mode for programming, but for goodness sake give
;;; it some decent key bindings
(add-hook 'prog-mode-hook
          (lambda()
            (local-set-key (kbd "C-c <right>") 'hs-show-block)
            (local-set-key (kbd "C-c <left>")  'hs-hide-block)
            (local-set-key (kbd "C-c <up>")    'hs-hide-all)
            (local-set-key (kbd "C-c <down>")  'hs-show-all)
            (hs-minor-mode t)))


(add-hook 'prog-mode-hook 'flyspell-prog-mode)



;;; Python
(setq python-shell-interpreter "python3")
(add-hook 'python-mode-hook 'blacken-mode)


;; taken from https://stackoverflow.com/questions/2658475/python-mode-import-problem
(defun python-reinstate-current-directory ()
  "When running Python, add the current directory ('') to the head of sys.path.
For reasons unexplained, run-python passes arguments to the
interpreter that explicitly remove '' from sys.path. This means
that, for example, using `python-send-buffer' in a buffer
visiting a module's code will fail to find other modules in the
same directory.

Adding this function to `inferior-python-mode-hook' reinstates
the current directory in Python's search path."
  (python-send-string "sys.path[0:0] = ['']"))

(add-hook 'inferior-python-mode-hook 'python-reinstate-current-directory)

;;; Javascript


(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

(use-package js2-refactor
  :disabled)

;; (require 'js-comint)
;; ;; Use node as our repl
;; (setq inferior-js-program-command "node")
;; (setenv "NODE_NO_READLINE" "1")
;; (add-hook 'js2-mode-hook '(lambda ()
;; 			    (local-set-key "\C-x\C-e" 'js-send-last-sexp)
;; 			    (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
;; 			    (local-set-key "\C-cb" 'js-send-buffer)
;; 			    (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
;; 			    (local-set-key "\C-cl" 'js-load-file-and-go)
;; 			    ))

  




(require 'flymake-jslint)
(add-hook 'js2-mode-hook 'flymake-jslint-load)


;;; Emacs Lisp
(require 'eldoc)
(add-hook 'emacs-lisp-mode-hook #'eldoc-mode)

(require 'checkdoc)
(add-hook 'emacs-lisp-mode-hook 'checkdoc-minor-mode)

(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'elisp-slime-nav-mode))


;;; Lisp

(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))


(use-package smartparens-config
  :commands (smartparens-mode)
  :ensure smartparens
  :init
  (add-hook 'prog-mode-hook 'smartparens-mode)
  (add-hook 'slime-repl-mode-hook 'smartparens-mode))

;; ;; Add hook to assorted lisp modes
;; (mapc (lambda (hook) (add-hook hook 'paredit-mode))
;;       '(clojure-mode-hook
;; 	slime-repl-mode-hook
;; 	emacs-lisp-mode-hook
;; 	ielm-mode-hook))

;; (when (locate-library "paredit")
;;   (autoload 'paredit-mode "paredit"
;;     "Turn on pseudo-structural editing of Lisp code."
;;     t)

;;   ;; By default paredit-splice-sexp is bount to M-s which stomps on a lot
;;   ;; of useful key bindings, so fix that.  Similarly the `\' behaviour is
;;   ;; weird.
;;   (eval-after-load 'paredit
;;     '(progn
;;        (define-key paredit-mode-map "\M-s" nil)
;;        (define-key paredit-mode-map "\C-cs" 'paredit-splice-sexp)
;;        (define-key paredit-mode-map "\\" nil))))




;;; Clojure
;; (unless (package-installed-p 'cider)
;;   (package-install 'cider))
;; (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)


;;; Puppet
(when (locate-library "puppet-mode")
  (autoload 'puppet-mode "puppet-mode")
  (add-to-list 'auto-mode-alist '("\\.pp$" . puppet-mode)))



;;; Documentation browser.

;;; Helm dash provides an emacs interface to the dash documentation
;;; sets for various languages.

;; To actually load up a docset use helm-dash-activate-docset
(use-package helm-dash
  :bind (("C-c ?" . helm-dash)
         ("C-c C-?" . helm-dash-at-point )))

(provide 'prog-config)

;;; prog-config.el ends here
