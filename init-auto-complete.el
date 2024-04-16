;;==配置auto-complete。内容较多单独放一个目录。==============
(add-to-list 'load-path "~/.emacs.d/plugins/auto-complete")
(require 'auto-complete)
(require 'auto-complete-config)


;; 开启全局设定(包含哪些模式在ac-modes里查看)
(global-auto-complete-mode t)
;;添加org-mode模式，在该模式下会自动开启自动完成
(add-to-list 'ac-modes 'org-mode)
;; 添加shell-mode模式，在该模式下会自动开启自动完成
(add-to-list 'ac-modes 'shell-mode)
;;使用自带字典
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugins/auto-complete/dict")
(ac-config-default)

;; 输入2个字符才开始补全
(setq ac-auto-start 2)

;; 补全的快捷键，用于需要提前补全-当还没有输入指定个数字符时显示弹出菜单。
(global-set-key "\M-/" 'auto-complete)  

;;使用增强的popup列表
(require 'pos-tip)
(setq ac-quick-help-prefer-pos-tip t)

;;使用quick-help
(setq ac-use-quick-help t)
(setq ac-quick-help-delay 0.5)

;; Show 0.8 second later
(setq ac-auto-show-menu 0.8)

;; 设置tab键的使用模式--??
;;(setq ac-dwim t)

;;使用fuzzy功能
(require 'fuzzy)
(setq ac-fuzzy-enable t)

;; 设置 auto-complete 的导航快捷键
(eval-after-load 'auto-complete
  '(progn
     ;; 当 auto-complete 菜单激活时，使用 C-n 和 C-p 进行候选项导航
     (define-key ac-completing-map (kbd "C-n") 'ac-next)
     (define-key ac-completing-map (kbd "C-p") 'ac-previous)))

;;菜单
(setq ac-menu-height 12)
(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")  
;;==end auto-complete===============================
(provide 'init-auto-complete)
