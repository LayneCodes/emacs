;; 设置F7快捷键打开treemacs
(use-package treemacs
  :ensure t
  :defer t
  :config
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ([f7]   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ;; ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  (:map treemacs-mode-map
	("/" . treemacs-advanced-helpful-hydra)))
  (require 'treemacs-evil)
(provide 'init-treemacs)
;;; treemacs.el ends here
