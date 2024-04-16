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
  (evil-define-key 'treemacs treemacs-mode-map (kbd "n") #'treemacs-next-line);将 n 键在 treemacs 的 evil 模式状态下绑定到 treemacs-next-line 函数
(provide 'init-treemacs)
;;; treemacs.el ends here
