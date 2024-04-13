(defun show-file-name ()
;; Show the full path file name in the minibuffer.
  (interactive)
  (message (buffer-file-name))
  (kill-new (file-truename buffer-file-name)))
(global-set-key "\C-cz" 'show-file-name)

(global-set-key (kbd "M-w") 'kill-region)              ; change M-w and C-w，M-w is copy
(global-set-key (kbd "C-w") 'kill-ring-save)           ; change M-w and C-w，C-w is cut

;; 自定义两个函数
;; Faster move cursor
(defun next-ten-lines()
  "Move cursor to next 10 lines."
  (interactive)
  (next-line 10))

(defun previous-ten-lines()
  "Move cursor to previous 10 lines."
  (interactive)
  (previous-line 10))
;; 绑定到快捷键
(global-set-key (kbd "M-n") 'next-ten-lines)            ; 光标向下移动 10 行
(global-set-key (kbd "M-p") 'previous-ten-lines)        ; 光标向上移动 10 行
;; 设置mac-command键为Meta键
(setq mac-command-modifier 'meta)
;; 刷新buffer,无需确认
(global-set-key (kbd "<f5>") (lambda () (interactive) (revert-buffer nil t)))

(provide 'init-keyboard)
;; init-keyboard.el ends here
