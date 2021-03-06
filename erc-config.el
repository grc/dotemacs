;;; ERC
;; Note that this is now developed wthin emacs, not the Savannah repository

(message "loading erc config")
(require 'erc)
(require 'tls)
(add-hook 'erc-mode-hook 'flyspell-mode)
(add-hook 'erc-mode-hook 'abbrev-mode)
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NAMES" "MODE"))

(setq erc-track-position-in-mode-line t)

(setq erc-nick "sensen")

;; Clears out annoying erc-track-mode stuff for when we don't care.
;; Useful for when ChanServ restarts :P
(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
(global-set-key (kbd "C-c r") 'reset-erc-track-mode)

(defadvice erc-track-find-face (around erc-track-find-face-promote-query activate)
  (if (erc-query-buffer-p) 
      (setq ad-return-value (intern "erc-current-nick-face"))
    ad-do-it))


(defadvice erc-track-modified-channels (around erc-track-modified-channels-promote-query activate)
  (if (erc-query-buffer-p)
      (setq erc-track-priority-faces-only 'nil))
  ad-do-it
  (if (erc-query-buffer-p)
      (setq erc-track-priority-faces-only 'all)))


;;; Reduce the noise generated by channel tracking: I only want to see
;;; the channel if my nick is mentioned or one of my keywords
(setq erc-current-nick-highlight-type 'nick)
(setq erc-keywords '("\\berc[-a-z]*\\b" "\\bemms[-a-z]*\\b"))

(setq erc-track-exclude-types '("JOIN" "PART" "QUIT" "NICK" "MODE"))
(setq erc-track-use-faces t)
(setq erc-track-faces-priority-list
      '(erc-current-nick-face erc-keyword-face erc-default-face))
(setq erc-track-priority-faces-only 'all)



 ;;; change header line face if disconnected
(defface erc-header-line-disconnected
  '((t (:foreground "black" :background "indianred")))
  "Face to use when ERC has been disconnected.")
    
(defun erc-update-header-line-show-disconnected ()
  "Use a different face in the header-line when disconnected."
  (erc-with-server-buffer
    (cond ((erc-server-process-alive) 'erc-header-line)
	  (t 'erc-header-line-disconnected))))
    
(setq erc-header-line-face-method 'erc-update-header-line-show-disconnected)


(load "~/grc-config/erc-auth.el")       ; credentials for pexip slack account

(defun pex ()
  (interactive)
  (erc-tls :server "pexip.irc.slack.com"
       :nick "giles"
       :password pexip-slack-password))

(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#zsh" "#stumpwm")))
