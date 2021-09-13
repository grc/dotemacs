;; (require 'helm-config)
;; (helm-mode 1)
;; (define-key global-map [remap find-file] 'helm-find-files)
;; (define-key global-map [remap occur] 'helm-occur)
;; (define-key global-map [remap list-buffers] 'helm-buffers-list)
;; (define-key global-map [remap dabbrev-expand] 'helm-dabbrev)
;; (define-key global-map [remap execute-extended-command] 'helm-M-x)
;; (unless (boundp 'completion-in-region-function)
;;   (define-key lisp-interaction-mode-map [remap completion-at-point] 'helm-lisp-completion-at-point)
;;   (define-key emacs-lisp-mode-map       [remap completion-at-point] 'helm-lisp-completion-at-point))


(use-package helm
  :ensure t
  :demand
  :bind (("M-x" . helm-M-x)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-buffers-list)
         ("M-s  o" . helm-occur))
         ("M-y" . helm-show-kill-ring)
         ("C-x r b" . helm-filtered-bookmarks)
  :preface(require ' helm-config)
  :config (helm-mode 1))




;;; Helm Projectile
;;; Details at https://tuhdo.github.io/helm-projectile.html

(use-package helm-projectile
  :ensure t
  :config
  (projectile-global-mode)
  (helm-projectile-on)
 ; Bug with projectile command map.
; See: https://github.com/bbatsov/helm-projectile/issues/116
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

  (setq projectile-completion-system 'helm))




;;; ;; helm-swoop
;; ;; https://github.com/ShingoFukuyama/helm-swoop
;; (add-to-list 'load-path "~/elisp/helm-swoop")

;; (require 'helm-swoop)
;; ;; Change the keybinds to whatever you like :)
;; (global-set-key (kbd "M-i") 'helm-swoop)
;; (global-set-key (kbd "M-I") 'helm-swoop-back-to-last-point)
;; (global-set-key (kbd "C-c M-i") 'helm-multi-swoop)
;; (global-set-key (kbd "C-x M-i") 'helm-multi-swoop-all)



;; ;; kill ring
;; (global-set-key (kbd "M-y") 'helm-show-kill-ring)

