;; -*- lexical-binding: t; -*-

;; [[file:../gwp-scratch.note::2ad3390b][2ad3390b]]
(use-package org-noter
  :config
  (setq org-noter-always-create-frame nil))
;; 2ad3390b ends here

;; [[file:../gwp-scratch.note::8a535ad4][8a535ad4]]
(defun gwp::org-note::create-annotation-file (document-path)
  (let* ((note-file "annotation.note")
         (document-name (file-name-nondirectory document-path))
         (document-base (file-name-base document-name)))
    (with-current-buffer (switch-to-buffer (find-file-noselect note-file))
      (goto-char (point-max))
      (insert "* " document-base)
      (org-set-property org-noter-property-doc-file document-name)
      (save-buffer))))

;;;###autoload
(defun gwp::org-note::dired-annotate-file-at-point ()
  "标注 dired buffer 中所定的(pdf)文件"
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (gwp::org-note::create-annotation-file (dired-get-file-for-visit))
    (user-error "not in dired buffer")))
;; 8a535ad4 ends here

;; [[file:../gwp-scratch.note::7f3b3bee][7f3b3bee]]
(defun gwp::org-note::to-read-file-in-READ (document-path read-dir)
  (let* ((document-name (file-name-nondirectory document-path))
         (document-in-read (concat read-dir "/" document-name)))

    ;; 将当前文件复制至READ 目录下
    (message (format "%s => %s" document-path document-in-read))
    (unless (file-exists-p read-dir)
      (make-directory read-dir))
    ;; 自动建立前置目录
    (rename-file document-path document-in-read)
    ;; 再将该文件反向软链回当前目录
    (make-symbolic-link (file-truename document-in-read) document-path)))

;;;###autoload
(defun gwp::org-note::annotate-pdf-in-READ (file)
  "将 file (PDF) 放至READ 目录下"
  (let* ((read-dir (read-directory-name "分类目录: " "~/Boox/READ/")))
    (gwp::org-note::to-read-file-in-READ file read-dir)))

;;;###autoload
(defun gwp::org-note::symbol-link-move-back (this-file)
  "将当前软链所指向的文件取回来, 同时删除源文件"
  (let* ((target-path (file-truename this-file)))
    (if (file-symlink-p this-file)
        (when (file-exists-p target-path)
          (delete-file this-file)
          (rename-file target-path this-file 1)
          (message "Moved from: %s" target-path))
      (user-error "not a symlink file"))))

;;;###autoload
(defun gwp::org-note::dired-annotate-pdf-in-READ ()
  "将 dired buffer 中所选定的(pdf)文件放至READ 目录下"
  (interactive)

  (if (derived-mode-p 'dired-mode)
      (let* ((file (dired-get-file-for-visit)))
        (gwp::org-note::annotate-pdf-in-READ file)
        (dired-do-redisplay))
    (user-error "not in dired buffer")))

;;;###autoload
(defun gwp::dired::symbol-link-move-back ()
  "在 dired 中, 将当前软链所指向的文件取回来, 同时删除源文件"
  (interactive)

  (if (derived-mode-p 'dired-mode)
      (let* ((this-file (dired-get-file-for-visit)))
        (gwp::org-note::symbol-link-move-back this-file)
        (dired-do-redisplay))
    (user-error "not in dired buffer")))
;; 7f3b3bee ends here

;; [[file:../gwp-scratch.note::6c8dad94][6c8dad94]]
;;;###autoload
(defun gwp::dired::locate-file-at-point ()
  "dired buffer 中, 用 locate 找到与当前文件同名的所有文件"
  (interactive)
  (locate (file-name-nondirectory (dired-get-file-for-visit))))
;; 6c8dad94 ends here

;; [[file:../gwp-scratch.note::ac1d0086][ac1d0086]]
;; (use-package org-id
;;   :custom
;;   (org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id))

(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id)
;; ac1d0086 ends here

;; [[file:../gwp-scratch.note::98fd4d7a][98fd4d7a]]
(use-package el-patch)

(defun gwp::org-id-find-id-file (id)
  (let* ((rg-command (format "ripgrep -l --color never -e '^\\s*:ID:\\s+%s' /home/ybyygu/.cache/notes" id))
         (output (shell-command-to-string rg-command))
         (file (car (split-string output "[\r\n]+" t))))
    file))

(el-patch-feature org-id)
(with-eval-after-load 'org-id
  (el-patch-defun org-id-find-id-file (id)
    "Query the id database for the file in which ID is located."
    (unless org-id-locations (org-id-locations-load))
    (or (and org-id-locations
             (hash-table-p org-id-locations)
             (gethash id org-id-locations))
        ;; Fall back on current buffer
        (or
         (gwp::org-id-find-id-file id)
         (buffer-file-name (or (buffer-base-buffer (current-buffer))
                               (current-buffer)))))))
;; 98fd4d7a ends here

;; [[file:../gwp-scratch.note::48102b4f][48102b4f]]
(use-package org-sidebar
  :custom
  (org-sidebar-side 'left)                                                         ; 新版中 left 是默认
  (org-ql-sidebar-buffer-setup-hook nil)                                           ; 避免多行显示, 太乱
  (org-sidebar-default-fns '(gwp::org-sidebar--backlinks org-sidebar--todo-items)) ; 使用反链视图
  (org-sidebar-tree-jump-fn 'org-sidebar-tree-jump-source)                         ; 跳至源文件对应的位置, 而不是 narrowed heding
  :config
  ;; 2025-02-28: 升级到 emacs 30后, 得禁用以下设置, 不然会报错了
  ;; org-ql-use-preamble: 提前生成一个正则表达式预处理模板（preamble），旨在加速某些查询（尤其是基于文本的正则匹配）
  (setq org-ql-use-preamble nil)
  ;; 避免误按
  (;; map! :map org-sidebar-tree-map
   ;;      [mouse-1] nil
   ;;      [drag-mouse-1] nil
   ))

;;;###autoload
(defun gwp::org-backlinks ()
  "显示指向当前 heading 的反向链接"
  (interactive)

  (let* ((org-sidebar-side 'right)
         (id (org-entry-get (point) "ID"))
         (custom-id (org-entry-get (point) "CUSTOM_ID"))
         (query (gwp::org-backlinks--get-query id custom-id)))
    (org-sidebar-ql (gwp::org-backlinks-search-files id)
      query :title (concat "Links to: " (org-get-heading t t)))))

(defun gwp::org-backlinks--get-query (id custom-id)
  (cond ((and id custom-id)
         ;; This will be slow because it isn't optimized to a single regexp.  :(
         (warn "Entry has both ID and CUSTOM_ID set; query will be slow")
         `(or (link :target ,(concat "id:" id))
              (link :target ,(concat "id:" custom-id))))
        ((or id custom-id)
         `(link :target ,(concat "id:" (or id custom-id))))
        (t (error "Entry has no ID nor CUSTOM_ID property"))))


;; reference:
;; (collection (funcall ffip-project-search-function cmd))
(defun gwp::org-backlinks-search-files (keyword)
  "搜索文件系统中所有的.note文件, 返回包含引用 keyword 的文件名"
  (let* (
         (rg-command (format "ripgrep -l --color never -e %s /home/ybyygu/.cache/notes" keyword))
         (output (shell-command-to-string rg-command))
         (collection (split-string output "[\r\n]+" t))
         result)
    ;; (message "shell output:\n%s" result)
    (dolist (file collection result) (push file result))
    result))

(defun gwp::org-sidebar--backlinks (source-buffer)
  "在 org-sidebar 中显示 backlinks buffer"
  (let* ((display-buffer (generate-new-buffer (format "org-sidebar<%s>" (buffer-name source-buffer))))
         (title (propertize (concat "反链条目: " (buffer-name source-buffer)) 'help-echo "含有指向当前heading链接的条目"))
         (id (org-entry-get (point) "ID"))
         (custom-id (org-entry-get (point) "CUSTOM_ID"))
         (source-buffers (gwp::org-backlinks-search-files id))
         query)
    (with-current-buffer display-buffer
      (setf org-sidebar-source-buffer source-buffer))

    ;; 如果当前 heading 无 ID, 不报错
    (condition-case err
        (setq query (gwp::org-backlinks--get-query id custom-id))
      (error
       (message "%s" (error-message-string err))
       (setq source-buffers nil)))

    (org-ql-search source-buffers
      query
      :buffer display-buffer
      :title title)

    display-buffer))
;; 48102b4f ends here

;; [[file:../gwp-scratch.note::1773f1a3][1773f1a3]]
(defun gwp::org-note::get-pdf-file ()
  (save-excursion
    (if (search-backward ":NOTER_DOCUMENT" nil t)
        (progn
          (org-back-to-heading)
          (let ((pdf (org-element-property :NOTER_DOCUMENT (org-element-at-point))))
            (message "%s" pdf)))
      (message "no pdf doc found"))))

(defun gwp::org-note::get-pdf-page ()
  (save-excursion
    (org-back-to-heading)
    (let ((property (org-element-property :NOTER_PAGE (org-element-at-point))))
      (let ((value (car (read-from-string property))))
        (cond
         ((consp value) (car value))
         (t value))))))

;;;###autoload
(defun gwp::org-note::new-note ()
  "在当前 heading 下插入新的文献阅读笔记"
  (interactive)
  ;; (let ((current-prefix-arg '(4)))     ; C-u
  ;;   (call-interactively #'org-insert-heading))
  ;; (insert (read-string "笔记标题: "))
  (if (org-at-heading-p)
      (let ((page (read-number "PDF 页码: " 1)))
        (org-set-property "NOTER_PAGE" (number-to-string page)))
    (user-error "not at org heading.")))

;;;###autoload
(defun gwp::org-note::open-pdf ()
  "使用 llpp 来打开当前笔记对应的 pdf 文件, 并转到指定的页码"
  (interactive)
  (let ((page (gwp::org-note::get-pdf-page))
        (pdf (gwp::org-note::get-pdf-file)))
    (if page
        (start-process "llpp" nil "llpp" pdf "-page" (format "%s" page))
      ;; (start-process "okular" nil "okular" pdf "-p" (format "%s" page))
      (start-process "llpp" nil "llpp" pdf)
      ;; (start-process "okular" nil "okular" pdf)
      )))
;; 1773f1a3 ends here

;; [[file:../gwp-scratch.note::43d2dac5][43d2dac5]]
(require 'embark)

(bind-key "R" 'gwp::org-note::annotate-pdf-in-READ embark-file-map)
(bind-key "T" 'gwp::org-note::symbol-link-move-back embark-file-map)
;; 43d2dac5 ends here

;; [[file:../gwp-scratch.note::8ae833e2][8ae833e2]]
(require 'dired)

(gwp::local-leader-def
  :keymaps 'dired-mode-map
  "n" #'(gwp::org-note::dired-annotate-file-at-point :which-key "PDF 批注")
  "R" #'(gwp::org-note::dired-annotate-pdf-in-READ :which-key "置入待读(READ)")
  "T" #'(gwp::dired::symbol-link-move-back :which-key "取回软链源文件")
  "L" #'(gwp::dired::locate-file-at-point :which-key "locate 同名文件"))

(require 'org)

(gwp::local-leader-def
  :keymaps 'org-mode-map
  "n" '(:ignore t :which-key "org note")
  "nb" #'gwp::org-backlinks
  "no" #'gwp::org-note::open-pdf
  )
;; 8ae833e2 ends here

;; [[file:../gwp-scratch.note::7fbc6e78][7fbc6e78]]
(use-package denote
  :ensure t
  :hook
  (dired-mode . denote-dired-mode)
  :custom
  ;; 默认是 org. .note 与 mime 桌面系统配置更好一些
  (denote-file-type 'org)
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-rename-confirmations '(add-front-matter)) ; 少一些确认项
  (denote-known-keywords '("fact" "thread" "question" "insight"))
  :config
  (setq denote-backlinks-show-context t)
  (setq denote-directory "~/Workspace/Notes/"
        denote-silo-extras-directories '(
                                         "~/Workspace/Notes/areas"
                                         "~/Workspace/Notes/projects"
                                         "~/Workspace/Notes/resources"
                                         "~/Workspace/Notes/publish"
                                         ))
  (setq denote-prompts '(subdirectory title keywords))
  ;; 在子目录选择时不显示 org-mode attachment 对应的 data 目录
  (setq denote-excluded-directories-regexp "data\\|graphs")
  (setq denote-excluded-files-regexp "data\\|graphs")

  ;; 默认用 .note, 而不是 .org
  (let ((org-settings (alist-get 'org denote-file-types)))
    (add-to-list 'denote-file-types
                 `(org ,@(plist-put (copy-tree org-settings) :extension ".note"))))

  ;; 修改 buffer 名称, 不然依原文字名是有些丑
  (denote-rename-buffer-mode 1))

;; 可以更方便地搜索 denote 笔记, 充许逐层过滤
(use-package denote-search)

(use-package consult-denote
  :after (denote consult)
  :init
  (consult-denote-mode t)
  :custom
  (consult-denote-find-command 'consult-fd)
  (consult-denote-grep-command 'consult-ripgrep)
  )

;; 更方便地显示 denote 所有笔记
(use-package denote-menu
  :bind (:map denote-menu-mode-map
              ("/ c" . denote-menu-clear-filters)
              ("/ r" . denote-menu-filter)
              ("/ k" . denote-menu-filter-by-keyword)
              ("/ o" . denote-menu-filter-out-keyword)))

(use-package denote-explore)
;; 7fbc6e78 ends here

;; [[file:../gwp-scratch.note::b8e9b0ca][b8e9b0ca]]
(defun my-denote-always-rename-on-save-based-on-front-matter ()
  "Rename the current Denote file, if needed, upon saving the file.
Rename the file based on its front matter, checking for changes in the
title or keywords fields.

Add this function to the `after-save-hook'."
  (let ((denote-rename-confirmations nil)
        (denote-save-buffers t)) ; to save again post-rename
    (when (and buffer-file-name (denote-file-is-note-p buffer-file-name))
      (ignore-errors (denote-rename-file-using-front-matter buffer-file-name))
      (message "Buffer saved; Denote file renamed"))))

(add-hook 'after-save-hook #'my-denote-always-rename-on-save-based-on-front-matter)
;; b8e9b0ca ends here

;; [[file:../gwp-scratch.note::63f83f7b][63f83f7b]]
(use-package denote
  :config
  (defun gwp::denote-new-note-in-currrent-directory ()
    "在当前目录下建立 denote 笔记"
    (interactive)
    (let ((denote-directory (expand-file-name default-directory))
          (denote-prompts '(title keywords)))
      (call-interactively 'denote))))
;; 63f83f7b ends here

;; [[file:../gwp-scratch.note::8bff31e2][8bff31e2]]
(general-define-key
 :prefix-map 'gwp::note-map
 "d" '(gwp::denote-dispatch :which-key "denote"))

(general-define-key
 :prefix-map 'gwp::develop-map
 "d" '(gwp::denote-dispatch :which-key "denote"))

;; transient 按键, 方便记忆
(require 'denote)
(transient-define-prefix gwp::denote-dispatch ()
  "Invoke a denote.el command from a list of available commands."
  ["Create"
   ("d" "New note" denote-silo-extras-create-note)
   ("cr" "With region" denote-region)
   ("cd" "With date" denote-date)
   ("cn" "New note in current directory" gwp::denote-new-note-in-currrent-directory)
   ]
  ["Update"
   ("ut" "Rename file title" denote-rename-file-title)
   ("uk" "Rename file keywords" denote-rename-file-keywords)
   ("ua" "Add front matter" denote-add-front-matter)
   ]
  ["Search/Find/List"
   ("f" "Find or create note" denote-open-or-create) ; 这个更方便
   ("F" "Find notes" consult-denote-find)            ; 有不少限制, 比如不支持拼音搜索. 但适合找更多的, 非 denote 管理的文件, 比如 data 目录下
   ("o" "Open or create" denote-silo-extras-open-or-create) ; 过滤下 silo
   ("s" "search notes using denote-search" denote-search)
   ("m" "list notes" denote-menu-list-notes)
   ("r" "random note" denote-explore-random-note)
   ]
  ["Link"
   ("l" "Create link" denote-link-or-create)
   ("L" "Find link" denote-find-link)
   ("ib" "Insert org dblock backlinks" denote-org-extras-dblock-insert-backlinks)
   ("id" "Insert org dblock links" denote-org-extras-dblock-insert-links)
   ("B" "Backlinks" denote-backlinks)
   ("b" "Find all backlink" denote-find-backlink)]
  )
;; 8bff31e2 ends here

;; [[file:../gwp-scratch.note::8d4b377b][8d4b377b]]
(provide 'init-note)
;; 8d4b377b ends here
