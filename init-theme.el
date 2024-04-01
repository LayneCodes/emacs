(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(load-theme 'dracula t)
;; 设置光标选中文本的背景色为好看的紫色
(set-face-attribute 'region nil :background "#9074b4")
;; 设置光标选中文本的背景色为 Dracula 主题中的绿色
;(set-face-attribute 'region nil :background "#D3D3D3")
(provide 'init-theme)
