;;; init-evil.el --- Evil mode configuration
(require 'evil)
(evil-mode 1)
(use-package evil
  :ensure t
  :init
  (setq evil-default-state 'normal)  ; 默认进入Normal模式
  (setq evil-disable-insert-state-bindings t)  ; 在插入模式下使用 Emacs 键绑定
  (setq evil-undo-system 'undo-redo)  ; 使用 Emacs 28+ 的 undo-redo 系统
  ;; 设置 shell 模式下的初始状态为 Emacs 状态
  (evil-set-initial-state 'shell-mode 'emacs)
  (defun my-shell-mode-hook ()
  ;; 当尝试关闭 shell 模式的 buffer 时，不进行二次确认
  (let ((process (get-buffer-process (current-buffer))))
    (when process
      (set-process-query-on-exit-flag process nil))))

  (add-hook 'shell-mode-hook 'my-shell-mode-hook)
  :config
  (evil-mode 1)

  ;; 在这些模式下保持其他Emacs快捷键
  (evil-make-overriding-map global-map 'insert)

  ;; normal模式中保持C-e移动到行尾
  (define-key evil-normal-state-map (kbd "C-e") 'move-end-of-line)
  ;; noramal模式中保持C-a移动到行首
  (define-key evil-normal-state-map (kbd "C-a") 'move-beginning-of-line)
  ;; noramal模式中保持C-n光标
  (define-key evil-normal-state-map (kbd "C-n") 'evil-next-visual-line)
  ;; noramal模式中保持C-p光标
  (define-key evil-normal-state-map (kbd "C-p") 'evil-previous-visual-line)
  ;; normal模式中保持C-f光标移动
  (define-key evil-normal-state-map (kbd "C-f") 'evil-forward-char)
  ;; normal模式中保持C-b光标移动
  (define-key evil-normal-state-map (kbd "C-b") 'evil-backward-char)
  ;; visual模式中设置C-w为复制
  (define-key evil-visual-state-map (kbd "C-w") 'evil-copy-region)
  ;; 应用更改
  (evil-normalize-keymaps))

;; 确保Emacs状态下的键绑定不受影响
(evil-set-initial-state 'emacs-lisp-mode 'emacs)
(defun my/org-tab-behavior ()
  "在Org模式中，根据光标位置调整heading级别或缩进。"
  (interactive)
  (if (org-at-heading-p)
      (org-cycle)
    (indent-for-tab-command)))

(with-eval-after-load 'evil
  (define-key evil-insert-state-map (kbd "TAB") 'my/org-tab-behavior)
  (define-key evil-insert-state-map [tab] 'my/org-tab-behavior))
(provide 'init-evil)
;;; init-evil.el ends here


