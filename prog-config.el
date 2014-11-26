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



;;; Smartparens
;; global
(require 'smartparens)
(smartparens-global-mode t)

;; highlights matching pairs
(show-smartparens-global-mode t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management

(define-key sp-keymap (kbd "C-M-f") 'sp-forward-sexp)
(define-key sp-keymap (kbd "C-M-b") 'sp-backward-sexp)

(define-key sp-keymap (kbd "C-M-d") 'sp-down-sexp)
(define-key sp-keymap (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key sp-keymap (kbd "C-S-a") 'sp-beginning-of-sexp)
(define-key sp-keymap (kbd "C-S-d") 'sp-end-of-sexp)

(define-key sp-keymap (kbd "C-M-e") 'sp-up-sexp)
(define-key emacs-lisp-mode-map (kbd ")") 'sp-up-sexp)
(define-key sp-keymap (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key sp-keymap (kbd "C-M-t") 'sp-transpose-sexp)

(define-key sp-keymap (kbd "C-M-n") 'sp-next-sexp)
(define-key sp-keymap (kbd "C-M-p") 'sp-previous-sexp)

(define-key sp-keymap (kbd "C-M-k") 'sp-kill-sexp)
(define-key sp-keymap (kbd "C-M-w") 'sp-copy-sexp)

(define-key sp-keymap (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key sp-keymap (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key sp-keymap (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key sp-keymap (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key sp-keymap (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key sp-keymap (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key sp-keymap (kbd "M-D") 'sp-splice-sexp)
(define-key sp-keymap (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key sp-keymap (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key sp-keymap (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key sp-keymap (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key sp-keymap (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key sp-keymap (kbd "C-M-]") 'sp-select-next-thing)

(define-key sp-keymap (kbd "M-F") 'sp-forward-symbol)
(define-key sp-keymap (kbd "M-B") 'sp-backward-symbol)

(define-key sp-keymap (kbd "H-t") 'sp-prefix-tag-object)
(define-key sp-keymap (kbd "H-p") 'sp-prefix-pair-object)
(define-key sp-keymap (kbd "H-s c") 'sp-convolute-sexp)
(define-key sp-keymap (kbd "H-s a") 'sp-absorb-sexp)
(define-key sp-keymap (kbd "H-s e") 'sp-emit-sexp)
(define-key sp-keymap (kbd "H-s p") 'sp-add-to-previous-sexp)
(define-key sp-keymap (kbd "H-s n") 'sp-add-to-next-sexp)
(define-key sp-keymap (kbd "H-s j") 'sp-join-sexp)
(define-key sp-keymap (kbd "H-s s") 'sp-split-sexp)

;; do not autoinsert ' pair if the point is preceeded by word.  This
;; will handle the situation when ' is used as a contraction symbol in
;; natural language.  Nil for second argument means to keep the
;; original definition of closing pair.
(sp-pair "'" nil :unless '(sp-point-after-word-p))

(defun sp-lisp-invalid-hyperlink-p (_ action _)
  (when (eq action 'navigate)
    (or (and (looking-at "\\sw\\|\\s_")
             (save-excursion
               (backward-char 2)
               (looking-at "\\sw\\|\\s_")))
        (and (save-excursion
               (backward-char 1)
               (looking-at "\\sw\\|\\s_"))
             (save-excursion
               (forward-char 1)
               (looking-at "\\sw\\|\\s_"))))))

;; emacs is lisp hacking enviroment, so we set up some most common
;; lisp modes too
(sp-with-modes sp--lisp-modes
  ;; disable ', it's the quote character!
  (sp-local-pair "'" nil :actions nil)
  ;; also only use the pseudo-quote inside strings where it serve as
  ;; hyperlink.
  (sp-local-pair "`" "'"
                 :when '(sp-in-string-p
                         sp-in-comment-p)
                 :unless '(sp-lisp-invalid-hyperlink-p)
                 :skip-match (lambda (ms mb me)
                               (cond
                                ((equal ms "'")
                                 (or (sp-lisp-invalid-hyperlink-p "`" 'navigate '_)
                                     (not (sp-point-in-string-or-comment))))
                                (t (not (sp-point-in-string-or-comment)))))))









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


