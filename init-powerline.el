;;; init-powerline.el --- 
(use-package powerline
  :ensure t  ; 确保powerline包被安装
  :config
  (powerline-default-theme))  ; 使用powerline的默认主题

(defun my-evil-mode-line-tag (state)
  "Return a formatted string representing the current evil state."
  ;; 将evil模式的状态转换为大写并格式化
  (let ((tag (upcase (symbol-name state))))
    (concat " " tag " ")))

(defun my-mode-line-modified-status ()
  "Return a string representing the buffer's modified status."
  ;; 根据缓冲区的修改状态返回不同的字符串
  (cond
   ((buffer-modified-p) "[*]")  ; 如果缓冲区被修改，显示[*]
   (buffer-read-only "[RO]")  ; 如果缓冲区是只读的，显示[RO]
   (t "[-]")))  ; 否则显示[-]

;; 更新mode-line-format以在任何模式下包含修改状态
(setq-default mode-line-format
              (list
               '(:eval (my-evil-mode-line-tag evil-state))  ; 动态显示evil模式状态
               '(:eval (my-mode-line-modified-status))  ; 动态显示文件修改状态
               " %b [%l:%c] %p"))  ; 显示文件名、行号、列号和位置百分比

(provide 'init-powerline)
;;; init-powerline.el ends here