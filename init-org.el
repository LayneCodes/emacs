;;; init-org.el --- 
;;;Zettelkasten 笔记法配置

(use-package org-roam
   :ensure t
   :after org
   :init
   (setq org-roam-v2-ack t) ;; Acknowledge V2 upgrade
   :config
   (org-roam-setup)
   ;;--------------------------
   ;; Handling file properties for ‘LAST_MODIFIED’
   ;;--------------------------
   (defun pv/org-find-time-file-property (property &optional anywhere)
     "Return the position of the time file PROPERTY if it exists.

When ANYWHERE is non-nil, search beyond the preamble."
     (save-excursion
       (goto-char (point-min))
       (let ((first-heading
              (save-excursion
                (re-search-forward org-outline-regexp-bol nil t))))
         (when (re-search-forward (format "^#\\+%s:" property)
                                  (if anywhere nil first-heading)
                                  t)
           (point)))))

   (defun pv/org-has-time-file-property-p (property &optional anywhere)
     "Return the position of time file PROPERTY if it is defined.

As a special case, return -1 if the time file PROPERTY exists but
is not defined."
     (when-let ((pos (pv/org-find-time-file-property property anywhere)))
       (save-excursion
         (goto-char pos)
         (if (and (looking-at-p " ")
                  (progn (forward-char)
                         (org-at-timestamp-p 'lax)))
             pos
           -1))))
   (defun pv/org-set-time-file-property (property &optional anywhere pos)
    "Set the time file PROPERTY in the preamble.

When ANYWHERE is non-nil, search beyond the preamble.

If the position of the file PROPERTY has already been computed,
it can be passed in POS."
    (when-let ((pos (or pos
                        (pv/org-find-time-file-property property))))
      (save-excursion
        (goto-char pos)
        (if (looking-at-p " ")
            (forward-char)
          (insert " "))
        (delete-region (point) (line-end-position))
        (let* ((now (format-time-string "[%Y-%m-%d %a %H:%M]")))
          (insert now)))))

  (defun pv/org-set-last-modified ()
    "Update the LAST_MODIFIED file property in the preamble."
    (when (derived-mode-p 'org-mode)
      (pv/org-set-time-file-property "last_modified")))
   :hook
   (before-save . pv/org-set-last-modified) ; 保存文件时调用
   :custom
   (org-roam-directory (concat org-directory "roam/"))  ; 设置 org-roam 目录

   ;; 自定义默认模板
   (org-roam-capture-templates
    '(("d" "default" plain "%?"
       :if-new
       (file+head "${slug}-%<%Y%m%d%H%M%S>.org"
                  "#+title: ${title}\n#+date: %u\n#+last_modified: \n\n")
       :immediate-finish t)))
   :bind (("C-c n f" . org-roam-node-find)
          (:map org-mode-map
            (("C-c n i" . org-roam-node-insert)
            ("C-c n o" . org-id-get-create)
            ("C-c n t" . org-roam-tag-add)
            ("C-c n a" . org-roam-alias-add)
            ("C-c n l" . org-roam-buffer-toggle)))))
;;;任务管理
(setq org-directory (file-truename "~/org/"))
(setq pv/org-agenda-files `(,(concat org-directory "Agenda/")))

(use-package org
  :init
  (require 'org-indent)
  :config
  (defun pv/init-org-hook ()
    (setq truncate-lines nil)
    (org-toggle-pretty-entities)) ; display LaTeX symbols
  (defun pv/org-skip-subtree-if-priority (priority)
    "Skip an agenda subtree if it has a priority of PRIORITY.

PRIORITY may be one of the characters ?A, ?B, or ?C."
    (let ((subtree-end (save-excursion (org-end-of-subtree t)))
          (pri-value (* 1000 (- org-lowest-priority priority)))
          (pri-current (org-get-priority (thing-at-point 'line t))))
      (if (= pri-value pri-current)
          subtree-end
        nil)))
  (defun pv/org-skip-subtree-if-habit ()
    "Skip an agenda entry if it has a STYLE property equal to \"habit\"."
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (if (string= (org-entry-get nil "STYLE") "habit")
          subtree-end
        nil)))
  :hook
  (org-mode . pv/init-org-hook)
  :custom
  (org-hide-leading-stars t "clearer way to display")
  (org-startup-with-inline-images t "always display inline image")
  (org-image-actual-width 600 "set width of image when displaying")
  (org-outline-path-complete-in-steps nil)
  (org-todo-keywords
   (quote ((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
           (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)" "MEETING"))))
  (org-todo-keyword-faces
   (quote (("TODO" :foreground "goldenrod1" :weight bold)
           ("NEXT" :foreground "DodgerBlue1" :weight bold)
           ("DONE" :foreground "SpringGreen2" :weight bold)
           ("WAITING" :foreground "LightSalmon1" :weight bold)
           ("CANCELLED" :foreground "LavenderBlush4" :weight bold)
           ("MEETING" :foreground "IndianRed1" :weight bold))))
  (org-todo-state-tags-triggers
   (quote (("CANCELLED" ("CANCELLED" . t))
           ("WAITING" ("WAITING" . t))
           (done ("WAITING"))
           ("TODO" ("WAITING") ("CANCELLED"))
           ("NEXT" ("WAITING") ("CANCELLED"))
           ("DONE" ("WAITING") ("CANCELLED")))))
  ;(setq org-todo-state-tags-triggers nil)
  (org-adapt-indentation t)
  (org-agenda-files pv/org-agenda-files)
  ;; Do not dim blocked tasks
  (org-agenda-dim-blocked-tasks nil)
  ;; compact the block agenda view
  (org-agenda-compact-blocks t)
  (org-agenda-span 7)
  (org-agenda-start-day "-2d")
  (org-agenda-start-on-weekday nil)
  (org-agenda-tags-column -86) ; default value auto has issues
  ;; Custom agenda command definitions
  (org-agenda-custom-commands
   (quote (("d" "Daily agenda and all TODOs"
            ((tags "PRIORITY=\"A\""
                   ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                    (org-agenda-overriding-header "High-priority unfinished tasks:")))
             (agenda "" ((org-agenda-ndays 1)))
             (alltodo ""
                      ((org-agenda-skip-function '(or (pv/org-skip-subtree-if-habit)
                                                      (pv/org-skip-subtree-if-priority ?A)
                                                      (org-agenda-skip-if nil '(scheduled deadline))))
                       (org-agenda-overriding-header "ALL normal priority tasks:"))))
            ((org-agenda-compact-blocks t)))
           ("p" "Projects"
            ((agenda "" nil)
             (tags "REFILE"
                   ((org-agenda-overriding-header "Tasks to Refile")
                    (org-tags-match-list-sublevels nil)))
             (tags-todo "-CANCELLED/!"
                        ((org-agenda-overriding-header "Stuck Projects")
                         (org-agenda-skip-function 'bh/skip-non-stuck-projects)
                         (org-agenda-sorting-strategy
                          '(category-keep))))
             (tags-todo "-CANCELLED/!NEXT"
                        ((org-agenda-overriding-header (concat "Project Next Tasks"
                                                               (if bh/hide-scheduled-and-waiting-next-tasks
                                                                   ""
                                                                 " (including WAITING and SCHEDULED tasks)")))
                         (org-agenda-skip-function 'bh/skip-projects-and-habits-and-single-tasks)
                         (org-tags-match-list-sublevels t)
                         (org-agenda-todo-ignore-scheduled bh/hide-scheduled-and-waiting-next-tasks)
                         (org-agenda-todo-ignore-deadlines bh/hide-scheduled-and-waiting-next-tasks)
                         (org-agenda-todo-ignore-with-date bh/hide-scheduled-and-waiting-next-tasks)
                         (org-agenda-sorting-strategy
                          '(todo-state-down effort-up category-keep))))
             (tags "-REFILE/"
                   ((org-agenda-overriding-header "Tasks to Archive")
                    (org-agenda-skip-function 'bh/skip-non-archivable-tasks)
                    (org-tags-match-list-sublevels nil))))
            nil))))
  :bind
  (("C-c a" . 'org-agenda)
   :map org-mode-map
   ("C-c C-q" . counsel-org-tag)))

;;
(setq pv/org-refile-file (concat org-directory "refile.org"))

(use-package org
  :custom
  (org-export-with-sub-superscripts nil)
  (org-use-sub-superscripts nil);禁用下划线的下标显示
  ;; ...
  (org-capture-templates
   (quote (("t" "todo" entry (file pv/org-refile-file)
            "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
           ("r" "respond" entry (file pv/org-refile-file)
            "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
           ("n" "note" entry (file pv/org-refile-file)
            "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
           ("w" "org-protocol" entry (file pv/org-refile-file)
            "* TODO Review %c\n%U\n" :immediate-finish t)
           ("m" "Meeting" entry (file pv/org-refile-file)
            "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t))))
  (org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))
  ;; Use full outline paths for refile targets - we file directly with IDO
  (org-refile-use-outline-path 'file)
    ;; Allow refile to create parent tasks with confirmation
  (org-refile-allow-creating-parent-nodes (quote confirm))
  
  (defun my/org-mode-setup ()
    (require 'evil)
    (add-hook 'org-read-date-minibuffer-local-map (lambda ()
        (define-key evil-motion-state-local-map (kbd "<return>") nil))))

  (add-hook 'org-mode-hook #'my/org-mode-setup)
  :bind
  ;; ...
  (("C-c a" . 'org-agenda)
   ("C-c c" . 'org-capture)
   :map org-mode-map
   ("C-c C-q" . counsel-org-tag)))
;; 设置org-archive 归档按照日期存档,如果父任务未完成,则子任务不归档
(setq org-archive-location (concat org-directory "archive/%s_archive::datetree/"))

(defun my/org-archive-done-tasks ()
  "Archive all tasks that are marked as DONE and have no undone children."
  (interactive)
  (org-map-entries
   (lambda ()
     (unless (my/org-has-undone-children-p)
       (org-archive-subtree)
       (setq org-map-continue-from (outline-previous-heading))))
   "/DONE" 'tree))

(defun my/org-has-undone-children-p ()
  "Check if the current heading has children that are not DONE."
  (save-excursion
    (let ((has-undone-children nil))
      (org-goto-first-child)
      (while (and (not has-undone-children) (org-goto-sibling))
        (unless (member (org-get-todo-state) '("DONE" "CANCELLED"))
          (setq has-undone-children t)))
      has-undone-children)))

(defun my/schedule-archive-done-tasks ()
  "Schedule `my/org-archive-done-tasks` to run every seven days."
  (run-at-time "00:59" (* 7 24 60 60) 'my/org-archive-done-tasks))
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "TAB") 'org-cycle))

(provide 'init-org)
;;; init-org.el ends here
