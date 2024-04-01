;;; init-startup.el --- Works when startup Emacs

;;; Commentary:
;;; (c) Cabins, github.com/cabins/.emacs.d

;;; Code:


;; Settings for system encoding
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;;font highlight
(global-font-lock-mode t)

;; Settings for backup files
(setq make-backup-files nil
      auto-save-default nil)

;; Adjust garbage collection thresholds during startup
(setq-default frame-title-format '("%b"))

(setq inhibit-startup-screen t)

;; I don't like the bell ring
(setq ring-bell-function #'ignore
      visible-bell nil)

;;font sizes
(set-face-attribute 'default nil :height 130)

;;set the place of the windows
;;(set-frame-position (selected-frame) 100 50)
;;set the size of the windows
;;(set-frame-width (selected-frame) 72)
;;(set-frame-height (selected-frame) 32)

;;replace the yes and no
(fset 'yes-or-no-p 'y-or-n-p)

;;display line num
(global-display-line-numbers-mode t)
;; set cursor |
(setq-default cursor-type 'bar)

;; set search A is A  not a
(setq-default case-fold-search nil)

;; 设置大文件警告阈值为 100MB
(setq large-file-warning-threshold (* 100 1024 1024))

;; set tab length 4
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq indent-line-function 'insert-tab)
(provide 'init-startup)
;;; init-startup.el ends here
