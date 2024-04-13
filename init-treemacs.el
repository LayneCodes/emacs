;; 设置F7快捷键打开treemacs
(use-package treemacs
  :ensure t
  :defer t
  :config
  :bind
  (:map global-map
        ([f7]   . treemacs))
  (:map treemacs-mode-map
	("/" . treemacs-advanced-helpful-hydra)))
  (require 'treemacs-evil)
(provide 'init-treemacs)
;;; treemacs.el ends here
