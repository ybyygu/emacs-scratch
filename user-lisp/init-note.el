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
  ;; "nb" #'gwp::org-backlinks
  "ns" #'org-note-search-by-id
  "nb" #'org-note-show-backlinks
  "no" #'gwp::org-note::open-pdf
  )
;; 8ae833e2 ends here

;; [[file:../gwp-scratch.note::9717b843][9717b843]]
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :custom
  (denote-file-type 'org)
  ;; 2026-01-25: 可以设置为多个不同的目录, 但感觉很多函数对这个支持的还有 bug
  (denote-directory
   '("~/Workspace/Notes/"
     ;; "~/Workspace/Obsidian/"
     ))
  (denote-infer-keywords t)
  (denote-sort-keywords t)
  (denote-rename-confirmations '(add-front-matter)) ; 少一些确认项
  (denote-known-keywords '("fact" "thread" "question" "insight"))
  (denote-prompts '(subdirectory title keywords))
  ;; 启用历史补全
  (denote-history-completion-in-prompts t)
  :config
  ;; 在子目录选择时不显示 org-mode attachment 对应的 data 目录
  (setq denote-excluded-directories-regexp
        (rx (or bos "/")
            (or "data" "ltximg" "assets" ".git")
            (opt "/")
            eos))
  (setq denote-org-front-matter
        (concat "#+title:      %s\n"
                "#+date:       %s\n"
                "#+filetags:   %s\n"
                "#+identifier: %s\n"
                "#+SETUPFILE: ~/Notes/common.org\n\n"))
  ;; 默认用 .note, 而不是 .org
  (let ((org-settings (copy-tree (alist-get 'org denote-file-types))))
    (plist-put org-settings :extension ".note")
    (setf (alist-get 'org denote-file-types) org-settings))
  ;; 修改 buffer 名称, 不然依原文字名是有些丑
  (denote-rename-buffer-mode 1))

(use-package denote-silo
  :ensure t
  :after denote
  :config
  (setq denote-silo-directories
        '("~/Workspace/Notes/areas"
          "~/Workspace/Notes/projects"
          "~/Workspace/Notes/resources"
          "~/Workspace/Notes/ai-drafts"
          "~/Workspace/Notes/publish")))

(use-package denote-org
  :ensure t
  :after denote)
;; 9717b843 ends here

;; [[file:../gwp-scratch.note::63f83f7b][63f83f7b]]
(defun gwp::denote-new-note-in-current-directory ()
  "在当前目录下建立 denote 笔记。"
  (interactive)
  (require 'denote) ; 确保命令可用，不依赖 init 加载顺序
  (let ((denote-directory (expand-file-name default-directory))
        (denote-prompts '(title keywords)))
    (call-interactively #'denote)))
;; 63f83f7b ends here

;; [[file:../gwp-scratch.note::b8e9b0ca][b8e9b0ca]]
(defvar-local gwp--denote-rename-in-progress nil)

(defun my-denote-always-rename-on-save-based-on-front-matter ()
  "Auto rename Denote note after save, guarded against reentry."
  (unless gwp--denote-rename-in-progress
    (when buffer-file-name
      ;; 确保 denote 的函数都已定义（避免 void-function）
      (require 'denote nil t)
      (when (and (featurep 'denote)
                 (cond
                  ;; 4.x 新函数（如果存在就用它）
                  ((fboundp 'denote-file-has-denoted-filename-p)
                   (denote-file-has-denoted-filename-p buffer-file-name))
                  ;; 回退：老版本/或更保守的判定
                  ((fboundp 'denote-file-is-note-p)
                   (denote-file-is-note-p buffer-file-name))))
        (let ((gwp--denote-rename-in-progress t)
              (denote-rename-confirmations nil)
              (denote-save-buffers t)
              (old buffer-file-name))
          (condition-case err
              (progn
                (denote-rename-file-using-front-matter old)
                (unless (string-equal old buffer-file-name)
                  (message "Denote renamed: %s -> %s"
                           (file-name-nondirectory old)
                           (file-name-nondirectory buffer-file-name))))
            (error
             (message "Denote rename failed: %s" (error-message-string err)))))))))

(add-hook 'after-save-hook #'my-denote-always-rename-on-save-based-on-front-matter)
;; b8e9b0ca ends here

;; [[file:../gwp-scratch.note::20e2a70e][20e2a70e]]
(use-package denote-protocol
  :ensure nil ; 因为是本地加载，不需要从包管理器获取
  )
;; 20e2a70e ends here

;; [[file:../gwp-scratch.note::7c74026b][7c74026b]]
(defun gwp::dired-copy-denote-link ()
  "Copy a Denote link for the file at point in Dired (v4.1 compatible).
修复：在格式化链接字符串前先检查 ID 是否存在，防止 nil 导致 format 报错。"
  (interactive)
  (let* ((file (dired-get-filename nil t))
         (id (denote-retrieve-filename-identifier file)))
    (if id
        (let* ((type (denote-filetype-heuristics file))
               (title (or (denote-retrieve-title-or-filename file type) ""))
               ;; 确保 format 只有在 id 为 string 时才执行
               (link (format denote-org-link-format id title)))
          (kill-new link)
          (message "Copied: %s" link))
      (user-error "File at point is not a Denote note"))))
;; 7c74026b ends here

;; [[file:../gwp-scratch.note::2c7d2ffc][2c7d2ffc]]
(defun gwp/denote-grep-md-note (query)
  "Search QUERY in .md/.note files under the Denote directory.

File collection uses `fd' (fast), while rendering reuses Denote's
results buffer, so it stays as pretty as `denote-grep'."
  (interactive (list (denote-grep-query-prompt)))
  (let* ((dir (if (listp denote-directory)
                  (car denote-directory)
                denote-directory))
         (dir (expand-file-name dir))
         (default-directory dir)
         ;; 用 fd 快速收集文件列表（-a 输出绝对路径）
         (files (split-string
                 (with-output-to-string
                   (with-current-buffer standard-output
                     (process-file "fd" nil t nil
                                   "-t" "f" "-a"
                                   "-e" "md" "-e" "note")))
                 "\n" t)))
    (unless files
      (user-error "No .md/.note files under %s" dir))
    (denote-make-links-buffer query files nil
                              denote-grep-display-buffer-action)))
;; 2c7d2ffc ends here

;; [[file:../gwp-scratch.note::be4b72b7][be4b72b7]]
(defun gwp/denote-ai-draft ()
  (interactive)
  (let* ((target-path (expand-file-name "ai-drafts" (denote-directory)))
         (title (denote-title-prompt "fleeting-ai-note"))
         (denote-use-directory target-path)
         (denote-use-file-type 'markdown-yaml)
         (denote-use-keywords '("question"))
         (denote-use-title title))
    (unless (file-exists-p target-path) (make-directory target-path t))
    (call-interactively #'denote)
    (message "AI Draft created in %s" target-path)))
;; be4b72b7 ends here

;; [[file:../gwp-scratch.note::c2fd095c][c2fd095c]]
;; --- Denote transient dispatch (Denote 4.1 friendly) -------------------------

(require 'seq) ; for `seq-find' (built-in since Emacs 25)
(require 'transient)

;; transient 按键, 方便记忆
(defun gwp::conditional-link-command ()
  "Execute link command based on current mode.
In Dired mode, run `gwp::dired-copy-denote-link`.
Otherwise, run `denote-link-or-create`."
  (interactive)
  (if (derived-mode-p 'dired-mode)
      (call-interactively #'gwp::dired-copy-denote-link)
    (call-interactively #'denote-link-or-create)))

(transient-define-prefix gwp::denote-dispatch ()
  "Invoke a denote.el command from a list of available commands."
  ["Create"
   ("d"  "New note (silo)"                 denote-silo-create-note)
   ("ca" "AI Draft (Markdown)"             gwp/denote-ai-draft)
   ("cr" "With region"                     denote-region)
   ("cd" "With date"                       denote-date)
   ("cn" "New note in current directory"   gwp::denote-new-note-in-current-directory)]
  ["Update"
   ("ut" "Rename file title"               denote-rename-file-title)
   ("uk" "Rename file keywords"            denote-rename-file-keywords)
   ;; v4+: `denote-add-front-matter` 弃用，rename 系列会自动补/改 front matter
   ("ua" "Rename file (also fixes front matter)" denote-rename-file)]
  ["Search/Find/List"
   ("f" "Find or create note"              denote-open-or-create)
   ("F" "Find notes (consult-denote)"      consult-denote-find)
   ("o" "Open or create (silo)"            denote-silo-open-or-create)
   ;; v4+: denote-search 并入 core -> denote-grep
   ("s" "Search notes (grep)"              denote-grep)
   ("S" "Search notes/md files (grep)"     gwp/denote-grep-md-note)
   ("m" "List notes (denote-menu)"         denote-menu-list-notes)
   ("r" "Random note (denote-explore)"     denote-explore-random-note)]
  ["Link"
   ("l"  "Link (Create/Copy)"              gwp::conditional-link-command)
   ("L"  "Find link"                       denote-find-link)
   ("ib" "Insert org dblock backlinks"     denote-org-dblock-insert-backlinks)
   ("id" "Insert org dblock links"         denote-org-dblock-insert-links)
   ("B"  "Backlinks"                       denote-backlinks)
   ("b"  "Find all backlink"               denote-find-backlink-with-location)])

(general-define-key
 :prefix-map 'gwp::note-map
 "d" '(gwp::denote-dispatch :which-key "denote"))

(general-define-key
 :prefix-map 'gwp::develop-map
 "d" '(gwp::denote-dispatch :which-key "denote"))
;; c2fd095c ends here

;; [[file:../gwp-scratch.note::8d4b377b][8d4b377b]]
(provide 'init-note)
;; 8d4b377b ends here
