;;; ERC
;; Note that this is now developed wthin emacs, not the Savannah repository

(message "loading erc config")
(require 'erc)
(add-hook 'erc-mode-hook 'flyspell-mode)
(add-hook 'erc-mode-hook 'abbrev-mode)
(setq erc-hide-list '("JOIN" "PART" "QUIT" "NAMES" "MODE"))

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

(require 'erc-image)
(add-to-list 'erc-modules 'image)
(erc-update-modules)

(require 'tls)

(load "~/grc-config/erc-auth.el")       ; credentials for pexip slack account

(defun pex ()
  (interactive)
  (erc-tls :server "pexip.irc.slack.com"
       :nick "giles"
       :password pexip-slack-password))

(setq erc-autojoin-channels-alist
      '(("irc.pexnote.com" "#pexnote") ; our server is still announcing the old FQDN
	("freenode.net" "#emacs" "#zsh" "#stumpwm")))
