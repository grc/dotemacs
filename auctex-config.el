;;; AucTex
;;; tex-site takes care of setting up the autoloads so don't
;;; directly load auctex.el
(message "Initialising AucTeX")

(use-package  tex
  :ensure auctex

  :config
  (setq-default TeX-PDF-mode t)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))
(message "Finished with AucTeX")
