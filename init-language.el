    ;;; cperl-mode is preferred to perl-mode                                  
    ;;; "Brevity is the soul of wit" <foo at acm.org>                               
(defalias 'perl-mode 'cperl-mode)
(setq cperl-invalid-face nil)
(setq cperl-electric-keywords t) ;; expands for keywords such as
(setq cperl-hairy t) ;; Turns on most of the CPerlMode options                                    ;; foreach, while, etc...

;; Define the major mode
(define-derived-mode generic-highlighting-mode fundamental-mode "Generic"
  "A major mode for generic syntax highlighting.")

;; Define the syntax highlighting rules
(defvar generic-highlighting-font-lock-keywords
  `(
    ;; Highlight comments (anything after #, anywhere in the line)
    ("\\(#.*\\)" . font-lock-comment-face)
    ;; Highlight strings (anything between single or double quotes)
    ("\\(\"[^\"]*\"\\|'[^']*'\\)" . font-lock-string-face)
    ;; Highlight keys (anything before a colon)
    ("\\([^:\n]+\\):" 1 font-lock-keyword-face)
    ;; Highlight standalone numbers
    ("\\b[0-9]+\\b" . font-lock-constant-face)
    )
  "Highlighting expressions for generic highlighting mode.")

;; Apply the syntax highlighting rules to the mode
(defun generic-highlighting-apply-syntax-highlighting ()
  (setq font-lock-defaults '(generic-highlighting-font-lock-keywords)))

;; Add the syntax highlighting to the mode hook
(add-hook 'generic-highlighting-mode-hook 'generic-highlighting-apply-syntax-highlighting)

;;打开文件时检查是否有已知的模式与文件扩展名匹配，如果没有，则自动切换到 generic-highlighting-mode
(defun switch-to-generic-highlighting-mode-if-unknown ()
  "Switches to generic-highlighting-mode if the current buffer is in fundamental-mode."
  (when (eq major-mode 'fundamental-mode)
    (generic-highlighting-mode)))

(add-hook 'find-file-hook 'switch-to-generic-highlighting-mode-if-unknown)


(provide 'init-language)
