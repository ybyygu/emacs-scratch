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
                                         )
        denote-org-front-matter (concat "#+title:      %s\n"
                                        "#+date:       %s\n"
                                        "#+filetags:   %s\n"
                                        "#+identifier: %s\n"
                                        "#+SETUPFILE: ~/Notes/common.org\n\n"
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

;; [[file:../gwp-scratch.note::dc7fe12d][dc7fe12d]]
(require 'el-patch)
(require 'denote) ; Ensure denote definitions are loaded
(require 'dired)  ; Ensure dired functions are available

(el-patch-defun denote-get-path-by-id (id)
  "Return absolute path of file with ID using fd (recursive, first match only).
Searches within `denote-directory`. Uses fd --max-results 1.
NOTE: This returns the *first* file fd finds, potentially ignoring extension preferences if multiple matches exist across directories.
Patched by el-patch to use fd."
  ;; --- Start of patched code ---
  (let* ((denote-dir (expand-file-name denote-directory))
         (pattern (concat "^" (regexp-quote id) "--.*"))
         ;; Handle fd/fdfind executable
         (fd-executable (or (executable-find "fd")
                            (executable-find "fdfind")))
         ;; Error out if fd/fdfind is not found
         (_ (unless fd-executable
              (error "el-patch (denote-get-path-by-id): Cannot find 'fd' or 'fdfind' executable in PATH")))
         ;; Build the command with --max-results 1
         ;; [2026-01-23 Fri] -L 跟随软链接
         (command (format "%s -L -a --max-results 1 --type f --regex %s %s"
                          (shell-quote-argument fd-executable)
                          (shell-quote-argument pattern)
                          (shell-quote-argument denote-dir)))
         ;; Execute fd, capture output, and remove potential trailing newline
         (output (string-trim-right (shell-command-to-string command))))

    ;; If output is not empty, return it (it's the absolute path). Otherwise, return nil.
    (unless (string-empty-p output)
      output))
  ;; --- End of patched code ---
  )

(defun gwp::dired-copy-denote-link ()
  "Copy a Denote link for the file at point in Dired.
The link format is [[denote:ID][Title]]. Title is retrieved using
Denote's v3.1.0 heuristics (preferring front matter if available).
Signals an error if point is not on a file or if the file
does not have a recognizable Denote ID in its name."
  (interactive)
  ;; Ensure we are in a Dired buffer
  (unless (derived-mode-p 'dired-mode)
    (error "Not in a Dired buffer"))

  ;; Get the full filename at point, don't prompt, return full path
  (let* ((file (dired-get-filename nil t))
         ;; Check if we got a file before proceeding
         (_ (unless file (error "No file at point")))
         ;; Try to extract the Denote ID using Denote's function that errors out
         ;; (denote-retrieve-filename-identifier-with-error is available in v3.1.0)
         (denote-id (denote-retrieve-filename-identifier-with-error file))
         ;; Determine the file type using heuristics (available in v3.1.0)
         (file-type (denote-filetype-heuristics file))
         ;; Get the title using Denote's function (available in v3.1.0).
         ;; This prioritizes front matter title if available and readable,
         ;; falling back to the filename component (raw slug in v3.1.0).
         ;; Returns nil if neither found, so we default to "".
         (denote-title (or (denote-retrieve-title-or-filename file file-type) ""))
         ;; Construct the link string with ID and Title
         (denote-link (format "[[denote:%s][%s]]" denote-id denote-title)))

    ;; Copy to kill ring (clipboard)
    (kill-new denote-link)
    ;; Provide user feedback in the echo area
    (message "Copied Denote link: %s" denote-link)))

;; Assuming you still want the same keybinding (e.g., C-c d l):
(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "C-c d l") #'gwp::dired-copy-denote-link))
;; dc7fe12d ends here

;; [[file:../gwp-scratch.note::20e2a70e][20e2a70e]]
(use-package denote-protocol
  :ensure nil ; 因为是本地加载，不需要从包管理器获取
  :commands (denote-protocol-copy-formatted-uri denote-protocol-copy-uri)
  )
;; 20e2a70e ends here

;; [[file:../gwp-scratch.note::be4b72b7][be4b72b7]]
(defun gwp/denote-ai-draft ()
  "先询问标题（默认为 flomo），然后在 ~/Notes/ai-drafts/ 下创建 Markdown 笔记。"
  (interactive)
  (let* ((target-subdir-name "ai-drafts")
         ;; 基于你的全局配置获取完整路径
         (target-path (expand-file-name target-subdir-name (denote-directory)))
         ;; 1. 弹出询问框，默认输入设为 flomo
         ;; 在 v3.1 中，denote-title-prompt 的参数是 (denote-title-prompt &optional default-title prompt-text)
         (title (denote-title-prompt "fleeting-ai-note"))
         ;; 2. 锁定 MD 环境
         (denote-file-type 'markdown-yaml)
         (denote-directory target-path))

    ;; 确保物理目录存在
    (unless (file-exists-p target-path)
      (make-directory target-path t))

    ;; 3. 创建笔记
    (denote
     title             ; 使用刚才输入的标题
     '("fact")         ; 默认关键字
     'markdown-yaml    ; 指定格式
     nil               ; 路径已由 denote-directory 锁定，此处传 nil
     nil nil nil)

    (message "AI Draft '%s' created in ai-drafts/" title)))
;; be4b72b7 ends here

;; [[file:../gwp-scratch.note::8bff31e2][8bff31e2]]
(general-define-key
 :prefix-map 'gwp::note-map
 "d" '(gwp::denote-dispatch :which-key "denote"))

(general-define-key
 :prefix-map 'gwp::develop-map
 "d" '(gwp::denote-dispatch :which-key "denote"))

;; transient 按键, 方便记忆
(defun gwp::conditional-link-command ()
  "Execute link command based on current mode.
In Dired mode, run `gwp::dired-copy-denote-link`.
Otherwise, run `denote-link-or-create`."
  (interactive) ; Make it callable interactively
  (if (derived-mode-p 'dired-mode)
      (call-interactively #'gwp::dired-copy-denote-link)
    (call-interactively #'denote-link-or-create)))

(transient-define-prefix gwp::denote-dispatch ()
  "Invoke a denote.el command from a list of available commands."
  ["Create"
   ("d" "New note" denote-silo-extras-create-note)
   ("ca" "AI Draft (Markdown)" gwp/denote-ai-draft)
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
   ("l" "Link (Create/Copy)" gwp::conditional-link-command)
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
