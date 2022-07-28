;;; prog-config.el --- Personal configuration for programming modes


;;; Code:

(global-prettify-symbols-mode 1)
;;; But if point is on the symbol, unprettify it
(customize-set-value 'prettify-symbols-unprettify-at-point 'right-edge)


;;; Commentary:
;; 

(use-package rainbow-delimiters)

;;; Colours are a bit too subtle by default so saturate them
;; (require 'cl-lib)
;; (require 'color)
;; (cl-loop
;;  for index from 1 to rainbow-delimiters-max-face-count
;;  do
;;  (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
;;    (cl-callf color-saturate-name (face-foreground face) 30)))




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



;; C families

(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))


;;; Python
(setq python-shell-interpreter "python3")


(use-package blacken
  :ensure t
  :hook (python-mode . blacken-mode))



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


;;; Emacs Lisp
;; (use-package eldoc
;;   :hook emacs-lisp-mode)


;; (require 'checkdoc)
;; (add-hook 'emacs-lisp-mode-hook 'checkdoc-minor-mode)

;; (require 'elisp-slime-nav)
;; (dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
;;   (add-hook hook 'elisp-slime-nav-mode))


;;; Lisp

(setq inferior-lisp-program "sbcl")
(setq slime-contribs '(slime-fancy))


;(use-package racket-mode)

;(add-hook 'racket-mode-hook      #'racket-unicode-input-method-enable)
;(add-hook 'racket-repl-mode-hook #'racket-unicode-input-method-enable)




;;; Clojure Experiments

;; (use-package clojure-mode)

;(use-package cider)





;;; Documentation browser.

;;; Helm dash provides an emacs interface to the dash documentation
;;; sets for various languages.

;; To actually load up a docset use helm-dash-activate-docset
(use-package helm-dash
  :bind (("C-c ?" . helm-dash)
         ("C-c C-?" . helm-dash-at-point )))

(provide 'prog-config)

;;; prog-config.el ends here
