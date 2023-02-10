;; [[file:../gwp-scratch.note::b402e642][b402e642]]
;; -*- lexical-binding: t; -*-
;; b402e642 ends here

;; [[file:../gwp-scratch.note::e2f6b646][e2f6b646]]
(use-package org
  :ensure nil
  :commands (org-mode org-agenda org-capture)
  :config
  ;; treat .note files as org-mode
  (add-to-list 'auto-mode-alist '("\\.note\\'" . org-mode))
  (add-to-list 'auto-mode-alist '("NOTE" . org-mode))

  (setq org-blank-before-new-entry nil)
  (setq org-default-notes-file (concat org-directory "/life.note"))

  ;; 保留以前的 Alt-Return 键行为, Alt-Return
  (org-defkey org-mode-map [(meta return)] 'org-meta-return)

  ;; https://orgmode.org/manual/Clean-view.html
  (setq org-startup-indented t)      ;Enable `org-indent-mode' on Org startup
  (with-eval-after-load 'org-indent
    (setq org-indent-indentation-per-level 1)) ;; default = 2

  ;; 对齐headline中的TAGs
  (setq org-tags-column -80)

  ;; 避免误编辑
  (setq org-catch-invisible-edits 'show-and-error))
;; e2f6b646 ends here

;; [[file:../gwp-scratch.note::0c698627][0c698627]]
(use-package org
  :config
  ;; https://orgmode.org/manual/Clean-view.html
  (setq org-startup-indented t)      ;Enable `org-indent-mode' on Org startup
  (with-eval-after-load 'org-indent
    (setq org-indent-indentation-per-level 1)) ;; default = 2

  ;; 对齐headline中的TAGs
  (setq org-tags-column -80)

  ;; 方便用 property 来控制 image 显示大小
  (setq org-image-actual-width nil)

  ;; 避免误编辑
  (setq org-catch-invisible-edits 'show-and-error)

  ;; 避免显示subtree之间多余的空行
  (setq org-cycle-separator-lines 0)

  ;; 禁用*bold*等标注的字体效果. 写代码时容易弄花显示. 比如__init__.
  (setq org-fontify-emphasized-text nil)

  ;; 默认为 ...
  (setq org-ellipsis "⤵")

  ;; 表格中文混排更整齐些
  ;; (custom-set-faces
  ;;  '(org-table ((t (:family "Ubuntu Mono")))))
  )
;; 0c698627 ends here

;; [[file:../gwp-scratch.note::10584ca0][10584ca0]]
;; 显示光标所在处的内容
(defun gwp::org-show-context-at-point ()
  (interactive)
  (call-interactively #'org-show-subtree)
  ;; 从下面的命令看来的
  ;; (call-interactively 'org-mark-ring-goto)
  ;; (org-show-context 'mark-goto)
  ;; (when (or (org-invisible-p) (org-invisible-p2)) (org-show-context 'mark-goto))
  (call-interactively #'org-reveal))

;; 默认的为 org-reveal, 但不太好用
(bind-key "C-c C-r" 'gwp::org-show-context-at-point org-mode-map)
;; 10584ca0 ends here

;; [[file:../gwp-scratch.note::67171862][67171862]]
(use-package org
  :config
  (defun gwp::goto-line-unhide (&rest _)
    (org-bookmark-jump-unhide))

  (advice-add #'goto-line :after #'gwp::goto-line-unhide)
  (advice-add #'avy-goto-line :after #'gwp::goto-line-unhide))
;; 67171862 ends here

;; [[file:../gwp-scratch.note::2f61258f][2f61258f]]
;; https://stackoverflow.com/questions/17590784/how-to-let-org-mode-open-a-link-like-file-file-org-in-current-window-inste
;; Depending on universal argument try opening link
(defun gwp::org-open-at-point-dwim (&optional arg)
  (interactive "P")
  (cond
   ((equal arg '(16))                    ; C-u C-u
    (let ((org-link-frame-setup (quote ((file . find-file)))))
      (org-open-at-point)))
   ((equal arg '(4))                     ; C-u
    (let ((org-link-frame-setup (quote ((file . find-file-other-frame)))))
      (org-open-at-point)))
   (t                                   ; the default behavior
    (let ((org-link-frame-setup (quote ((file . find-file-other-window)))))
      (org-open-at-point)))))

;; 注释代码时, 在org code block下特殊处理. 不然光标会跳开很远.
(defun gwp/comment-or-uncomment-dwim ()
  (interactive)
  (save-excursion
    (if (org-in-src-block-p)
        (progn
          (org-edit-src-code)
          (call-interactively 'comment-dwim)
          (org-edit-src-exit))
      (call-interactively 'comment-dwim))))

(bind-key "C-c C-o" 'gwp::org-open-at-point-dwim org-mode-map)

(gwp::local-leader-def
  :keymaps 'org-mode-map
  "o"      #'(gwp::org-open-at-point-dwim :which-key "open at point"))
;; 2f61258f ends here

;; [[file:../gwp-scratch.note::fbbec921][fbbec921]]
;; 取自doom org moudle
(defun gwp::org-dwim-at-point (&optional arg)
  "Do-what-I-mean at point.

If on a:
- checkbox list item or todo heading: toggle it.
- clock: update its time.
- footnote reference: jump to the footnote's definition
- footnote definition: jump to the first reference of this footnote
- table-row or a TBLFM: recalculate the table's formulas
- table-cell: clear it and go into insert mode. If this is a formula cell,
  recaluclate it instead.
- babel-call: edit org-src
- statistics-cookie: update it.
- latex fragment: toggle it.
- link: follow it
- otherwise, refresh all inline images in current tree."
  (interactive "P")
  (if (button-at (point))
      (call-interactively #'push-button)
    (let* ((context (org-element-context))
           (type (org-element-type context)))
      ;; skip over unimportant contexts
      (while (and context (memq type '(verbatim code bold italic underline strike-through subscript superscript)))
        (setq context (org-element-property :parent context)
              type (org-element-type context)))
      (pcase type
        (`clock (org-clock-update-time-maybe))

        (`footnote-reference
         (org-footnote-goto-definition (org-element-property :label context)))

        (`footnote-definition
         (org-footnote-goto-previous-reference (org-element-property :label context)))

        ((or `planning `timestamp)
         (org-follow-timestamp-link))

        (`babel-call
         (org-babel-lob-execute-maybe))

        (`statistics-cookie
         (save-excursion (org-update-statistics-cookies arg)))

        ;; Hacked by ybyygu at 2021-04-13
        ((or `src-block `inline-src-block)
         (org-edit-special arg))

        ((or `latex-fragment `latex-environment)
         (org-latex-preview arg))

        (`link
         (let* ((lineage (org-element-lineage context '(link) t))
                (path (org-element-property :path lineage)))
           (if (or (equal (org-element-property :type lineage) "img")
                   (and path (image-type-from-file-name path)))
               (org-toggle-inline-images)
             ;; 强制在本窗口打开
             (let ((current-prefix-arg '(16)))     ; C-u C-u
               (call-interactively #'gwp::org-open-at-point-dwim)))))

        ((guard (org-element-property :checkbox (org-element-lineage context '(item) t)))
         (let ((match (and (org-at-item-checkbox-p) (match-string 1))))
           (org-toggle-checkbox (if (equal match "[ ]") '(16)))))

        (_
         (if (or (org-in-regexp org-ts-regexp-both nil t)
                 (org-in-regexp org-tsr-regexp-both nil  t)
                 (org-in-regexp org-link-any-re nil t))
             (call-interactively #'org-open-at-point)
           ;; (+org--toggle-inline-images-in-subtree
           ;;  (org-element-property :begin context)
           ;;  (org-element-property :end context))
           ))))))

(gwp::local-leader-def
  :keymaps 'org-mode-map
  "RET"    #'gwp::org-dwim-at-point
  [return] #'gwp::org-dwim-at-point
  )
;; fbbec921 ends here

;; [[file:../gwp-scratch.note::7330d8ac][7330d8ac]]
;; (setq browse-url-browser-function 'browse-url-firefox)

;; If available, use `xdg-open' to open URLs.
(setq-default
 browse-url-browser-function (quote browse-url-generic)
 browse-url-generic-program "xdg-open")
;; 7330d8ac ends here

;; [[file:../gwp-scratch.note::*latex preview][latex preview:1]]
(setq org-format-latex-options (plist-put org-format-latex-options :scale 2.5))
;; latex preview:1 ends here

;; [[file:../gwp-scratch.note::*toggle][toggle:1]]
(defun gwp::org-toggle-checkbox ()
  (interactive)
  (unless (org-at-item-p)
    (call-interactively #'org-toggle-item))
  (let ((current-prefix-arg '(4)))     ; C-u
    (call-interactively #'org-toggle-checkbox)))
;; toggle:1 ends here

;; [[file:../gwp-scratch.note::994db730][994db730]]
(require 'org-attach)
;; 可用父节点定义的 attach 目录
(setq org-attach-use-inheritance t)

;;;###autoload
(defun gwp::org-attach-auto-directory ()
  "为当前 headline 设置 DIR 属性 (基于 ID)"
  (interactive)

  (let* ((attach-dir (org-attach-dir-from-id (org-id-new)))
         (current-dir (file-name-directory (or default-directory
                                               buffer-file-name)))
         (attach-dir-relative (file-relative-name attach-dir current-dir)))
    (org-entry-put nil "DIR" attach-dir-relative)
    attach-dir))
;; 994db730 ends here

;; [[file:../gwp-scratch.note::458d7b11][458d7b11]]
(org-link-set-parameters "zotero" :follow #'gwp/org-zotero-open :export #'gwp/org-zotero-export)

(defun gwp/org-zotero-open (path)
  (setq url (format "zotero:%s" path))
  (browse-url url))

;; rust-modules
;; (add-to-list 'load-path "/home/ybyygu/Workspace/Programming/emacs/rust-modules")
(require 'zotero)

(defun gwp/zotero-search-by-tag (name)
  "Search Zotero entries by tag using ivy."
  (interactive "sTag: ")

  (let* ((candidates (zotero-search-items-by-tag name))
	 (item (completing-read "Zotero entries: " candidates nil t)))
    (gwp--zotero-open-attachments item)))

(defun gwp/zotero-search-by-collection (name)
  "Search Zotero entries by collection name using ivy."
  (interactive "sCollection: ")

  (let* ((candidates (zotero-search-items-by-collection name))
	 (item (completing-read "zotero entries: " candidates nil t)))
    (gwp--zotero-open-attachments item)))

(defun gwp--zotero-show-related-items (x)
  "show related items from selection"
  (let* ((candidates (zotero-get-related-items x))
	 (item (completing-read "Related: " candidates nil t)))
    (gwp--zotero-open-attachments item)))

(defun gwp--zotero-annotate-attachment (pdf-file)
  "Annotate the attachment with org-noter."
  (let ((annotation-file (expand-file-name (car org-noter-default-notes-file-names) (file-name-directory pdf-file))))
    (progn
      ;; create an empty annotation file if not exists
      (unless (file-exists-p annotation-file) (write-region "" nil annotation-file))
      (org-open-file pdf-file)
      (org-noter))))

;; (advice-add #'find-file-other-tab :around #'eaf--find-file-advisor)
(defun gwp--zotero-open-attachments (x)
  "completion for zotero attachments."
  (require 'consult)

  (let* ((candidates (zotero-get-selected-item-attachment-paths x))
	 ;; (attach (completing-read "Open attachment: " candidates nil t)))
         ;; 可应用 file 类型的 consult 目标
	 (attach (consult--read candidates :prompt "Open attachment: " :category 'file)))
    (org-open-file attach)))

(defun gwp--zotero-insert-link (x)
  (let ((uri (zotero-get-selected-item-link x)))
    (if uri
        (progn
          (message "%s!" x)
          (insert "[[" uri "][" "zotero-item" "]]"))
      (error "No link extracted from: %s" x))))

(defun gwp--zotero-open-link (x)
  (let ((uri (zotero-get-selected-item-link x)))
    (if uri
        (progn
          (message "%s!" x)
          (org-link-open-from-string (format "[[%s]]" uri)))
      (error "No link extracted from: %s" x))))

(defun gwp/org-open-zotero-attachments-at-point (arg)
  "Handle zotero attachments in org-mode"
  (interactive "P")
  (let ((ct (org-element-context)))
    (if (eq 'link (org-element-type ct))
        (let ((link (org-element-property :raw-link ct)))
          (when link
            (let ((key (zotero-get-item-key-from-link link)))
              (if key
                  (gwp--zotero-open-attachments key)
                (error "Invalid zotero link!"))))))))

(defun gwp/org-open-zotero-related-at-point (arg)
  "Open related zotero items for zotero link at point"
  (interactive "P")
  (let ((ct (org-element-context)))
    (if (eq 'link (org-element-type ct))
        (let ((link (org-element-property :raw-link ct)))
          (when link
            (let ((key (zotero-get-item-key-from-link link)))
              (if key
                  (gwp--zotero-show-related-items key)
                (error "Invalid zotero link!"))))))))

(defun gwp/insert-new-zotero-item (arg)
  "Create a new zotero item (report)"
  (interactive "P")

  (let ((uri (zotero-create-new-note)))
    (if uri
        (progn
          (message "%s!" uri)
          (insert "[[" uri "][" "zotero-note" "]]"))
      (error "create zotero item failed!"))))

;; https://www.reddit.com/r/emacs/comments/f3o0v8/anyone_have_good_examples_for_transient/
(require 'transient)
(transient-define-prefix gwp/zotero-search-transient ()
  "Search zotero database"
  ["Search zotero items:"
   ("t" "search by tag" gwp/zotero-search-by-tag)
   ("c" "search by collection" gwp/zotero-search-by-collection)
   ("o" "open attachments at point" gwp/org-open-zotero-attachments-at-point)
   ("r" "open related items at point" gwp/org-open-zotero-related-at-point)
   ]
  )

(gwp::local-leader-def
  :keymaps 'org-mode-map
  "z" '(gwp/zotero-search-transient :which-key "zotero"))
;; 458d7b11 ends here

;; [[file:../gwp-scratch.note::95825713][95825713]]
(defun gwp::new-memo-time-stamp (arg)
  "Insert a new org-mode memo entry under heading at point."
  (interactive "P")
  (unless (org-at-heading-p)
    (org-up-element))
  ;; (call-interactively 'crux-smart-open-line)
  (call-interactively 'org-next-visible-heading)
  (call-interactively 'org-insert-todo-heading)
  (call-interactively 'org-time-stamp-inactive)
  (when (meow-normal-mode-p) (call-interactively 'meow-insert))
  (insert " "))

(defun gwp::new-item-time-stamp (arg)
  (interactive "P")
  (if (org-in-item-p)
      (progn
        (call-interactively 'org-beginning-of-item)
        (call-interactively 'org-insert-item))
    (insert "- "))
  (call-interactively 'org-time-stamp-inactive)
  (insert " ")
  (when (meow-normal-mode-p) (call-interactively 'meow-insert)))
;; 95825713 ends here

;; [[file:../gwp-scratch.note::492d6ae4][492d6ae4]]
(transient-define-prefix gwp::org-insert-transient ()
  "Common insert commands"
  ["常用"
   ("n" "Insert note" org-add-note) ; or C-c C-z
   ("m" "Insert memo" gwp::new-memo-time-stamp)
   ("i" "Insert item" gwp::new-item-time-stamp)
   ("p" "Set property" org-set-property)
   ("h" "Insert heading" org-insert-heading)
   ("s" "Insert subheading" org-insert-subheading)
   ("t" "Insert structured template" org-insert-structure-template)]
  ["备用"
   ("e" "Set effort" org-set-effort)
   ("d" "Insert drawer" org-insert-drawer)
   ("f" "Insert footnote" org-footnote-new)
   ("l" "Insert link" org-insert-link)])

(gwp::local-leader-def
  :keymaps 'org-mode-map
  "i" #'(gwp::org-insert-transient :which-key "insert")
  )
;; 492d6ae4 ends here

;; [[file:../gwp-scratch.note::3b9396a6][3b9396a6]]
(defun gwp/org-image-attributes-default (&optional caption)
  "default image attributes: caption, name label, width ..."
  "Annotate LINK with the time of download."
  (format (concat
           (concat  "#+caption: " (read-string "Caption: " caption) "\n")
           ;; set unique figure name
           (format "#+name: fig:%s\n" (substring (org-id-new) 0 8))
           ;; unit in px; for displaying in org-mode
           "#+attr_org: :width 800\n"
           ;; unit in cm; for exporting as odt
           "#+attr_odt: :width 10\n"
           )))

(defun gwp/org-insert-image-attributes (&optional caption)
  "insert image attributes such as caption and labels"
  (interactive)
  (insert (gwp/org-image-attributes-default caption)))

(defun gwp/org-download-annotate (link)
  "Annotate LINK with the time of download."
  (gwp/org-image-attributes-default))

(use-package org-download
  :commands
  org-download-delete
  org-download-yank
  org-download-clipboard
  :hook ((org-mode . org-download-enable)) ; 启用拖放功能
  :bind (:map org-mode-map
         ("C-c v" . org-download-clipboard))
  :config
  (setq org-download-method 'attach
        org-download-annotate-function 'gwp/org-download-annotate
        ;; org-download-image-html-width 900 ; in px
        ;; org-download-image-latex-width 16 ; in cm
        ;; 2021-09-03: 直接调用org-download-clipboard即可, 以下代码不必要
        ;; org-download-screenshot-method
        ;; (cond ((executable-find "txclip")  "txclip paste --image -o %s")
        ;;       ((executable-find "scrot") "scrot -s %s"))
        ))
;; 3b9396a6 ends here

;; [[file:../gwp-scratch.note::0caa1907][0caa1907]]
(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode)
  :init
  ;; ◉ ○ ◆ » ◇ ▶ ▷
  (setq org-superstar-headline-bullets-list '("☰" "▶" "▷" "»"))
  )
;; 0caa1907 ends here

;; [[file:../gwp-scratch.note::99465500][99465500]]
;; 编辑代码时在下方新开窗口
;;(setq org-src-window-setup 'split-window-below)
(setq org-src-window-setup 'current-window)
;; 99465500 ends here

;; [[file:../gwp-scratch.note::c2ffdd20][c2ffdd20]]
;; Add convenience lang alias for markdown blocks
(add-to-list 'org-src-lang-modes '("toml" . conf-toml))
;; c2ffdd20 ends here

;; [[file:../gwp-scratch.note::d309f5b7][d309f5b7]]
(setq org-src-preserve-indentation nil)
(setq org-edit-src-content-indentation 0) ;Default = 2

;; helper functions for literate programming
;; taking from: https://github.com/grettke/help/blob/master/Org-Mode_Fundamentals.org
(defun help/set-org-babel-default-header-args (property value)
  "Easily set system header arguments in org mode.

PROPERTY is the system-wide value that you would like to modify.

VALUE is the new value you wish to store.

Attribution: URL `http://orgmode.org/manual/System_002dwide-header-arguments.html#System_002dwide-header-arguments'"
  (setq org-babel-default-header-args
        (cons (cons property value)
              (assq-delete-all property org-babel-default-header-args))))

;; 几个重要的header args:
(help/set-org-babel-default-header-args :padline "yes")
(help/set-org-babel-default-header-args :mkdirp "yes")
(help/set-org-babel-default-header-args :comments "link")
;; d309f5b7 ends here

;; [[file:../gwp-scratch.note::661f0512][661f0512]]
(defun gwp/org-babel-tangle-no()
  "Turn on or turn off tangling current code block"
  (interactive)
  (if (eq 'src-block (org-element-type (org-element-at-point)))
      (save-excursion
        (org-babel-goto-src-block-head)
        (if (re-search-forward ":tangle no" (line-end-position) t)
            (delete-region (match-beginning 0) (match-end 0))
          (org-babel-insert-header-arg "tangle" "no")))
    (org-set-property "header-args" ":tangle no")))
;; 661f0512 ends here

;; [[file:../gwp-scratch.note::1a4b128e][1a4b128e]]
(defun gwp/org-src-insert-name ()
  "If it doesn't have a NAME property then assign it an unique name."
  (interactive)
  (let ((element (org-element-at-point)))
    (if (eq 'src-block (org-element-type element))
        (if (not (org-element-property :name element))
            (save-excursion
              (goto-char (org-babel-where-is-src-block-head))
              (let ((i (current-indentation)))
                (save-excursion (insert "#+name: " (substring (org-id-new) 0 8) "\n"))
                (indent-to i)))
          (message "source block alread named"))
      (message "not in source block"))))
;; 1a4b128e ends here

;; [[file:../gwp-scratch.note::62fd7850][62fd7850]]
;;;###autoload
;; tangle blocks for current file at point
;; http://stackoverflow.com/questions/28727190/org-babel-tangle-only-one-code-block
;; call org-babel-tangle with C-u C-u
(defun gwp/org-babel-tangle-blocks()
  (interactive)
  ;; tangle blocks only for target file at point
  (let ((current-prefix-arg '(16)))     ; C-u C-u
    (call-interactively 'org-babel-tangle)))

;; narrow to subtree before calling org-babel-tangle
(defun gwp/org-tangle-subtree()
  "Tange src blocks in current subtree"
  (interactive)
  (org-narrow-to-subtree)
  (org-babel-tangle)
  (widen))

;;;###autoload
(defun gwp/org-edit-save-and-tangle ()
  "When in a sub-editing buffer, swith to the parent buffer and tangle the file blocks"
  (interactive)
  (save-excursion
    (org-edit-src-exit)
    ;; insert an unique code block name
    (gwp/org-src-insert-name)
    (call-interactively 'gwp/org-babel-tangle-blocks)
    (org-edit-src-code)))

;;;###autoload
(defun gwp::org-babel-tangle-dwim()
  "Tangle current file at point whenever in a sub-editing buffer or not"
  (interactive)
  ;; 标记当前位置
  ;; (gwp::mark-current-position)
  (if (org-src-edit-buffer-p)
      (gwp/org-edit-save-and-tangle)
    (if (eq 'src-block (org-element-type (org-element-at-point)))
        (progn
          ;; insert an unique code block name
          (gwp/org-src-insert-name)
          (call-interactively 'gwp/org-babel-tangle-blocks))
      (message "not in source block"))))
;; 62fd7850 ends here

;; [[file:../gwp-scratch.note::8aa4aca8][8aa4aca8]]
(defhydra gwp/org-jump-block ()
  "jump to org blocks"
  ("n" org-next-block "next block")
  ("p" org-previous-block "prev block")
  ("q" nil "quit")
  )

(defhydra gwp/org-jump-link ()
  "jump to org links"
  ("n" org-next-link "next link")
  ("p" org-previous-link "prev link")
  ("q" nil "quit")
  )
;; 8aa4aca8 ends here

;; [[file:../gwp-scratch.note::fa928b1c][fa928b1c]]
;;;###autoload
(defun gwp::org-babel-tangle-jump-to-file ()
  "Jump to tangle file for the source block at point."
  (interactive)
  (let ((mid (point))
        (element (org-element-at-point))
        (body-start (save-excursion
                      (progn
                        (org-babel-goto-src-block-head)
                        (next-line)
                        (point)
                        )))
        (tangle-file (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info 'light)))))
        offset)
    (if tangle-file
        (let ((block-name (org-element-property :name element))
              (tangle-file (expand-file-name tangle-file)))
          (if (file-readable-p tangle-file)
              (progn
                ;; open tangled file
                (find-file tangle-file)
                ;; if code block has a name, we jump to that block
                (when block-name
                  (beginning-of-buffer)   ; if point restored, the searching could fail
                  (when (search-forward (format "::%s" block-name) nil t)
                    (next-line)
                    (beginning-of-line)
                    (setq offset (- mid body-start))
                    (forward-char offset)
                    (recenter)
                    )))
            (error "Cannot open tangle file %S" tangle-file)))
      (message "not in source block"))))
;; fa928b1c ends here

;; [[file:../gwp-scratch.note::9b40c7cf][9b40c7cf]]
;;;###autoload
(defun gwp::org-babel-tangle-jump-to-org ()
  "Jump from a tangled code file to the related Org mode file."

  (require 'ol)
  (interactive)
  (let ((mid (point))
	start body-start end target-buffer target-char link block-name body)
    (save-window-excursion
      (save-excursion
    (while (and (re-search-backward org-link-bracket-re nil t)
            (not ; ever wider searches until matching block comments
             (and (setq start (line-beginning-position))
              (setq body-start (line-beginning-position 2))
              (setq link (match-string 0))
              (setq block-name (match-string 2))
              (save-excursion
                (save-match-data
                  (re-search-forward
                   (concat " " (regexp-quote block-name)
                       " ends here")
                   nil t)
                  (setq end (line-beginning-position))))))))
	(unless (and start (< start mid) (< mid end))
	  (error "Not in tangled code"))
        (setq body (buffer-substring body-start end)))
      ;; Go to the beginning of the relative block in Org file.
      (org-link-open-from-string link)
      (message "%s" link)
      (setq target-buffer (current-buffer))
      ;; (search-forward body)
      (if (string-match "[^ \t\n\r]:\\([[:digit:]]+\\)" block-name)
          (let ((n (string-to-number (match-string 1 block-name))))
            (if (org-before-first-heading-p) (goto-char (point-min))
              (org-back-to-heading t))
            ;; Do not skip the first block if it begins at point min.
            (cond ((or (org-at-heading-p)
                       (not (eq (org-element-type (org-element-at-point))
                		'src-block)))
                   (org-babel-next-src-block n))
                  ((= n 1))
                  (t (org-babel-next-src-block (1- n)))))
        (org-babel-goto-named-src-block block-name))
      (goto-char (org-babel-where-is-src-block-head))
      (forward-line 1)
      ;; Try to preserve location of point within the source code in
      ;; tangled code file.
      (let ((offset (- mid body-start)))
        (when (< end (+ offset (point))) ; ybyygu hacked here
          (forward-char offset)))
      (setq target-char (point)))
    (org-src-switch-to-buffer target-buffer t)
    (goto-char target-char)))
;; 9b40c7cf ends here

;; [[file:../gwp-scratch.note::097db727][097db727]]
;;;###autoload
(defun gwp::org-babel-narrow-to-tangle-heading ()
  "narrow至当前代码块对应的 tangle 文件所在级别"
  (interactive)
  (let ((org-indirect-buffer-display 'current-window)
	(tangle-file (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info 'light)))))
        (start-position (point))
        offset)
    ;; :tangle no 不能算
    (if (and tangle-file (not (string= tangle-file "no")))
        (save-excursion
          (if (search-backward (format ":tangle %s" tangle-file) nil t)
              (progn
                (setq offset (- start-position (point)))
                (org-tree-to-indirect-buffer)
                (forward-char offset)
                (message "narrowed to heading: %s" tangle-file))
            (message "no root headline found")))
      (message "narrowed to headline at point")
      (org-tree-to-indirect-buffer))))
;; 097db727 ends here

;; [[file:../gwp-scratch.note::82ecc499][82ecc499]]
(defun gwp::org-mark-link ()
  "Marks an org text link."
  (interactive)
  (let ((case-fold-search t)
        (re "\\[\\["))
    (unless (looking-at re)
      (search-backward-regexp re))
    (set-mark (point))
    (search-forward "]]")
    (exchange-point-and-mark)))
;; 82ecc499 ends here

;; [[file:../gwp-scratch.note::4971b464][4971b464]]
;;;###autoload
(defun gwp::search-all-notes-ivy (&optional arg)
  "search all notes in ~/.cache/notes"
  (interactive)
  (let ((counsel-rg-base-command (list
                                  "ripgrep"
                                  "-M" "240"
                                  "--with-filename"
                                  "--no-heading"
                                  "--line-number"
                                  "--color" "never"
                                  "%s")))
    (if arg
        (counsel-rg arg "~/.cache/notes")
      (counsel-rg "" "~/.cache/notes"))))


;;;###autoload
(defun gwp::search-all-notes (&optional arg)
  "search all notes in ~/.cache/notes"
  (interactive)
  (require 'consult)
  (let ((consult-ripgrep-args (list
                               "ripgrep"
                               "--null"
                               "--no-heading"
                               "--path-separator" "/"
                               "--line-number"
                               "--color" "never"
                               ".")))
    (if arg
        (consult-ripgrep "~/.cache/notes" arg)
      (consult-ripgrep "~/.cache/notes" ""))))
;; 4971b464 ends here

;; [[file:../gwp-scratch.note::05419467][05419467]]
(require 'simpleclip)

(defun gwp::search-org-notes (query)
  "Perform a text search on `org-directory'."
  (interactive
   (list (if (use-region-p)
             (buffer-substring-no-properties
              (region-beginning)
              (region-end))
           "")))

  (require 'org)
  (let ((counsel-rg-base-command (list
                                  "ripgrep"
                                  "-M" "240"
                                  "--with-filename"
                                  "--no-heading"
                                  "--line-number"
                                  "--color" "never"
                                  "%s")))
    (counsel-rg query org-directory)
    ))


;; credit: https://github.com/CsBigDataHub/counsel-fd/blob/master/counsel-fd.el
(defvar gwp--fd-command "fd --hidden --color never "
  "Base command for fd.")

;;;###autoload
(defun counsel-fd-file-jump (&optional initial-input initial-directory)
  "Jump to a file below the current directory.
List all files within the current directory or any of its subdirectories.
INITIAL-INPUT can be given as the initial minibuffer input.
INITIAL-DIRECTORY, if non-nil, is used as the root directory for search."
  (interactive
   (list nil
         (when current-prefix-arg
           (read-directory-name "From directory: "))))
  (let* ((default-directory (or initial-directory default-directory))
	 (d (completing-read "File: "
		      (split-string
		       (shell-command-to-string
			(concat gwp--fd-command "--type l --exclude '*.git'"))
		       "\n" t)
		      nil
		      t)))
    (find-file (expand-file-name d))))

(defun gwp::find-file-in-notes ()
  "Find a file under `~/.cache/notes', recursively."
  (interactive)
  (let ((default-directory "~/.cache/notes")
        (find-file-visit-truename t))
        (counsel-fd-file-jump)))
;; 05419467 ends here

;; [[file:../gwp-scratch.note::515195f9][515195f9]]
(general-define-key
 :prefix-map 'gwp::note-map
 ;; "s" 'gwp::search-org-notes
 "s" 'gwp::search-all-notes
 "f" 'gwp::find-file-in-notes
 )
;; 515195f9 ends here

;; [[file:../gwp-scratch.note::da4e0834][da4e0834]]
(unless init-no-x-flag
  (defun gwp::update-notes-cache ()
    (interactive)
    (message (shell-command-to-string "rebuild-note-cache.sh")))

  (require 'midnight)
  (midnight-mode t)
  ;; 默认延时为 3600 秒
  (midnight-delay-set 'midnight-delay 7200)

  (add-hook 'midnight-hook #'gwp::update-notes-cache))
;; da4e0834 ends here

;; [[file:../gwp-scratch.note::5dc0bf0f][5dc0bf0f]]
(setq org-capture-templates
      '(
        ("n" "Note" entry (file "~/Notes/refile.note")
         "* %u %? %(org-get-x-clipboard 'CLIPBOARD)\n  %:initial\n" :prepend t)
        ("t" "Task" entry (file+headline "~/Notes/life.note" "Tasks")
         "* TODO %^T %? %(org-get-x-clipboard 'CLIPBOARD)\n  %i" :prepend t)
        ("r" "Research Memo" entry (file+headline "~/Notes/research.note" "Memo")
         "* %u %? %(org-get-x-clipboard 'CLIPBOARD)\n  %i\n" :prepend t)
        ("p" "Paper" entry (file+headline "~/Notes/research.note" "Literature")
         "* TODO %u %? %(org-get-x-clipboard 'CLIPBOARD)\n  %i\n" :prepend t)
        ("j" "Life Journal" entry (file+headline "~/Notes/life.note" "Journals")
         "* %u %? %(org-get-x-clipboard 'CLIPBOARD)\n  %i\n" :prepend t)
        ("N" "Note from protocol" entry (file "~/Notes/refile.note")
         "* %u %? [[%:link][%:description]]\n  %:initial\n" :prepend t)))
;; 5dc0bf0f ends here

;; [[file:../gwp-scratch.note::43fd72e2][43fd72e2]]
(require 'org-agenda)

;; 2013-01-20: less is more
;; (setq org-agenda-files (append (file-expand-wildcards "~/Notes/*.note") (file-expand-wildcards "~/Notes/*/*.note")))
(setq org-agenda-files "~/Notes/.agenda_files")

;; the default is todo-start
(setq org-icalendar-use-scheduled (quote (event-if-not-todo event-if-todo todo-start)))
(setq org-icalendar-alarm-time 5)

;; Show all future entries for repeating tasks
(setq org-agenda-repeating-timestamp-show-all t)

;; do not show agenda dates if they are empty
(setq org-agenda-show-all-dates nil)

;; Sorting order for tasks on the agenda
(setq org-agenda-sorting-strategy
      (quote ((agenda time-up priority-down category-up)
              (todo priority-down)
              (tags priority-down))))

;; Start the weekly agenda today
(setq org-agenda-start-on-weekday nil)

;; do not include todo items
(setq org-agenda-include-all-todo nil)

;; 忽略已经完成的任务
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-done t)

;; 退出 agenda buffer 时还原之前的窗口
(setq org-agenda-restore-windows-after-quit t)
;; 43fd72e2 ends here

;; [[file:../gwp-scratch.note::f1b60139][f1b60139]]
(setq org-agenda-window-setup 'current-window)
;; f1b60139 ends here

;; [[file:../gwp-scratch.note::ded2ea25][ded2ea25]]
;; description for "g" prefix
(setq org-agenda-custom-commands '(("g" . "GTD contexts")))

;; project overview
(add-to-list 'org-agenda-custom-commands
             '("gp" "Project"
               (
                (tags "Project+Action+TODO=\"TODO\""
                      (
                       (org-agenda-overriding-header "Project\n------------------")
                       (org-agenda-sorting-strategy '(priority-down category-keep timestamp-up))))
                (tags "Action+Study+TODO=\"TODO\""
                      (
                       (org-agenda-overriding-header "Topics\n------------------")
                       (org-agenda-files '("~/Notes/research.note"))
                       (org-agenda-sorting-strategy '(priority-down timestamp-up))
                       (org-agenda-max-entries 5)))
                (tags "Action+TODO=\"TODO\""
                      (
                       (org-agenda-overriding-header "生活琐事\n------------------")
                       (org-agenda-files '("~/Notes/life.note"))
                       (org-agenda-sorting-strategy '(priority-down timestamp-up))
                       (org-agenda-max-entries 5)))
                )
               ;; options set here apply to the entire block
               (
                (org-tags-match-list-sublevels nil)
                (org-agenda-prefix-format "%-20c ")
                (org-agenda-todo-keyword-format "")
                (org-agenda-remove-tags t)
                (org-agenda-compact-blocks t))))

(add-to-list 'org-agenda-custom-commands
             '("gt" "Tasks"
               (
                (agenda ""
                        (;; (org-agenda-entry-types '(:deadline :scheduled))
                         (org-agenda-span (quote month)) ;; or (org-agenda-span 90)
                         (org-agenda-include-diary nil)
                         (org-agenda-overriding-header "Agenda\n------------------")))
                (tags-todo "ASAP"
                           ((org-agenda-entry-types '(:timestamp))
                            (org-agenda-skip-function '(org-agenda-skip-entry-if 'scheduled))
                            (org-agenda-overriding-header "\nASAP\n------------------")
                            (org-agenda-sorting-strategy '(priority-down category-keep timestamp-up))
                            (org-agenda-max-entries 20)
                            (org-agenda-prefix-format "%-12c ")
                            (org-agenda-compact-blocks t)))
                (tags-todo "TODO={READ}"
                           ((org-agenda-overriding-header "\n待读列表\n------------------")
                            (org-agenda-sorting-strategy '(category-keep priority-down))
                            (org-agenda-remove-tags t)
                            (org-agenda-prefix-format "%-12c ")
                            (org-agenda-compact-blocks t)))
                )
               ;; options set here apply to the entire block
               (
                (org-tags-match-list-sublevels nil)
                ;; (org-agenda-files '("~/Notes/research.note" "~/Notes/life.note"))
                (org-agenda-todo-keyword-format "")
                (org-agenda-remove-tags t))
               ;; agenda view exported with: Ctrl-C a e
               ("~/Notes/agenda.html" "~/Notes/agenda.txt")))


(defun gwp::org-agenda-gtd-task ()
  "show gtd task"
  (interactive)
  (org-agenda nil "gt"))

(defun gwp::org-agenda-gtd-task-this-buffer ()
  "show gtd task for this buffer"
  (interactive)
  (if (equal major-mode 'org-mode)
      (org-agenda nil "gt" 'buffer))
  (message "not in org mode buffer"))

(general-define-key
 :prefix-map 'gwp::note-map
 "a" '(org-agenda :which-key "org agenda")
 "l" '(org-store-link :which-key "org store link")
 "c" '(org-capture :which-key "org capture")
 "n" '(gwp::org-agenda-gtd-task :which-key "org agenda (gtd)")
 "b" '(gwp::org-agenda-gtd-task-this-buffer :which-key "org agenda (gtd) this buffer")
 )
;; ded2ea25 ends here

;; [[file:../gwp-scratch.note::fc776ca8][fc776ca8]]
;; (unbind-key "h" org-agenda-mode-map)
;; (unbind-key "l" org-agenda-mode-map)
;; (unbind-key "j" org-agenda-mode-map)
;; (unbind-key "k" org-agenda-mode-map)

;; org-agenda-add-note
;; (unbind-key "z" org-agenda-mode-map)

(bind-key "j" #'org-agenda-next-line org-agenda-mode-map)
(bind-key "k" #'org-agenda-previous-line org-agenda-mode-map)
(bind-key "h" #'backward-char org-agenda-mode-map)
(bind-key "l" #'forward-char org-agenda-mode-map)

(gwp::local-leader-def
 :keymaps 'org-agenda-mode-map
 "TAB"  #'org-next-link
 "RET" #'org-agenda-open-link
 )
;; fc776ca8 ends here

;; [[file:../gwp-scratch.note::6f58facc][6f58facc]]
(require 'org-crypt)
(require 'epa-file)
(epa-file-enable)

;; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-crypt-tag-matcher "crypt")
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
;; GPG key to use for encryption
(setq org-crypt-key "38D95BC6411A87E7") ; ybyygu@gmail.com
(setq org-crypt-disable-auto-save nil)
;; 6f58facc ends here

;; [[file:../gwp-scratch.note::*protocol][protocol:1]]
(require 'org-protocol)
;; protocol:1 ends here

;; [[file:../gwp-scratch.note::3516de82][3516de82]]
;; credit: https://github.com/noncog/.dotfiles/blob/master/.config/cogmacs/init.el
(defun noncog/no-delete-windows (oldfun &rest args)
  "A function for use with advice to prevent other functions from using delete-other-window."
  (cl-letf (((symbol-function 'delete-other-windows) 'ignore)) (apply oldfun args)))

(advice-add 'org-add-log-note :around #'noncog/no-delete-windows)
(advice-add 'org-fast-tag-selection :around #'noncog/no-delete-windows)
(advice-add 'org-fast-todo-selection :around #'noncog/no-delete-windows)
(advice-add 'org-export--dispatch-ui :around #'noncog/no-delete-windows)
(with-eval-after-load "org-capture"
  ;; prevent from hiding other windows
  (advice-add 'org-capture-place-template :around 'noncog/no-delete-windows))

;; 在下面新开窗口显示
(add-to-list 'display-buffer-alist
             '("*Org Note*"
               (display-buffer-below-selected)
               ;; (window-height . 10)
               ))
;; 3516de82 ends here

;; [[file:../gwp-scratch.note::e13e8c0f][e13e8c0f]]
(defun gwp::org-in-latext-env-p ()
  "Return whether point is latex environment."
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    (or (eql type 'latex-fragment)
        (eql type 'latex-environment))))

(defun gwp::org-in-link-env-p ()
  "Return whether point is link environment."
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    (eql type 'link)))

(defun gwp::org-in-zotero-link-p ()
  "Return whether point is zotero link environment."
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    (if (eq 'link type)
        (let ((link (org-element-property :raw-link context)))
          (s-starts-with? "zotero://" link)))))

(defun gwp::org-in-image-link-p ()
  "Return whether point is image link environment."
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    (if (eq 'link type)
        (let* ((lineage (org-element-lineage context '(link) t))
               (path (org-element-property :path lineage))
               (type (org-element-property :type lineage)))
          (or (equal type "img")
              (and path (image-type-from-file-name path)))
          ))))

(defun gwp::org-format-src-block ()
  (interactive)
  (org-babel-do-key-sequence-in-edit-buffer ""))


(transient-define-prefix gwp::org-menu-at-point ()
  "Show relevant menu for thing at point"
  ["src block" :if org-in-src-block-p
   ("RET" "edit" org-edit-special)
   ("n" "next" org-next-block :transient t)
   ("p" "prev" org-previous-block :transient t)
   ("b" "tangle dwim" gwp::org-babel-tangle-dwim)
   ("." "mark" org-babel-mark-block)
   ("f" "format" gwp::org-format-src-block)
   ]
  ["latex" :if gwp::org-in-latext-env-p
   ("RET" "toggle inline display" org-latex-preview)
   ]
  ["image" :if gwp::org-in-image-link-p
   ("RET" "toggle inline display" org-toggle-inline-images)
   ]
  ["link" :if gwp::org-in-link-env-p
   ("n" "next" org-next-link :transient t)
   ("p" "prev" org-previous-link :transient t)
   ("." "mark" gwp::org-mark-link)
   ("o" "open" gwp::org-open-at-point-dwim)
   ]
  ["heading" :if org-at-heading-p
   ("i" "insert memo" gwp::new-memo-time-stamp)
   ("t" "toggle todo" org-todo)
   ;; ("n" "next" org-next-visible-heading :transient t)
   ;; ("p" "prev" org-previous-visible-heading :transient t)
   ("." "mark" org-mark-subtree)
   ("a" "attachments" org-attach)
   ("A" "archive" org-archive-subtree)
   ("mh" "make heading (before)" org-insert-heading)
   ("ml" "make heading (after)" org-insert-heading-after-current)
   ("mt" "make todo (before)" org-insert-todo-heading)]
  ["property" :if org-at-heading-p
   ("sp" "property" org-set-property)
   ("sd" "delete property" org-delete-property)
   ]
  ["item" :if org-at-item-p
   ("i" "insert item" gwp::new-item-time-stamp)
   ("r" "repair" org-list-repair)
   ("*" "turn into tree" org-list-make-subtree)
   ("s" "sort" org-sort-list)
   ]
  ["checkbox"  :if org-at-item-checkbox-p
   ("RET" "toggle" org-toggle-checkbox)
   ]
  ["table"  :if org-at-table-p
   ("e" "edit all" org-table-edit-formulas)
   ("ir" "insert row" org-table-insert-row)
   ("ic" "insert column" org-table-insert-column)
   ("il" "insert hline" org-table-insert-hline)
   ("dc" "delete column" org-table-delete-column)
   ("dr" "delete row" org-table-kill-row)
   ("td" "toggle debug" org-table-toggle-formula-debugger)
   ]
  ["zotero"  :if gwp::org-in-zotero-link-p
   ("RET" "zotero menu" gwp/zotero-search-transient)
   ]
  ["motion"
   ("g" "goto" consult-org-heading)
   ])

;; org 跳转前后设置 mark, 方便跳回
(advice-add #'consult-org-heading :before #'org-mark-ring-push)
(advice-add #'consult-org-heading :after #'org-mark-ring-push)

(gwp::local-leader-def
  :keymaps 'org-mode-map
  ","  #'(gwp::org-menu-at-point :which-key "context menu"))
;; e13e8c0f ends here

;; [[file:../gwp-scratch.note::d737c18e][d737c18e]]
;; (setq org-export-backends '(ascii html icalendar latex odt md))
(require 'ox-md)
;; d737c18e ends here

;; [[file:../gwp-scratch.note::*zotero/export][zotero/export:1]]
(with-eval-after-load 'org-compat
  (defun gwp/org-zotero-export (path desc format)
    "Create the export version of zotero link specified by PATH and
DESC. FORMATs understood are 'odt','latex and 'html."
    (cond
     ((eq format 'html)
      (format "<a href=\"zotero:%s\">%s</a>" path desc))
     ((eq format 'latex)
      (format "\\href{zotero:%s}{%s}" path desc))
     ((eq format 'odt)
      ;; (format "<text:a xlink:type=\"simple\" xlink:href=\"zotero:%s\">%s</text:a>" path desc)
      (gwp/org-zotero-export-odt path desc))
     (t desc))))

;;;; The magic string of zitem:
;; ZOTERO_ITEM CSL_CITATION
;; {
;; "properties": {
;; "formattedCitation": "[1]",
;; "plainCitation": "[1]"
;; },
;; "citationItems": [
;;                   {
;;                   "uri": [
;;                           "http://zotero.org/users/15074/items/S5JM4V35"
;;                           ]
;;                   }
;;                   ],
;; "schema": "https://github.com/citation-style-language/schema/raw/master/csl-citation.json"
;; } %s-rnd

;; adopted from https://www.mail-archive.com/emacs-orgmode@gnu.org/msg48905.html
(defun gwp/org-zotero-export-odt (path desc)
  (let
      ((refmark "<text:reference-mark-start text:name=\"%s\"/>%s<text:reference-mark-end text:name=\"%s\"/>")
       (zitem "ZOTERO_ITEM CSL_CITATION {
    &quot;properties&quot;: {
        &quot;formattedCitation&quot;: &quot;%s&quot;,
        &quot;plainCitation&quot;: &quot;%s&quot;
    },
    &quot;citationItems&quot;: [
        {
            &quot;uri&quot;: [
                &quot;http://zotero.org/users/15074/items/%s&quot;
            ]
        }
    ],
    &quot;schema&quot;: &quot;https://github.com/citation-style-language/schema/raw/master/csl-citation.json&quot;
} %s ")

       (item-key (car (cdr (split-string path "_"))))
       (rnd (concat "RND" (substring (org-id-new) -10))))
    (setq zitem
          (format zitem
                  desc
                  desc
                  item-key
                  rnd)
          )
    (setq desc (format "%s" desc))
    (format refmark zitem desc zitem))
  )
;; zotero/export:1 ends here

;; [[file:../gwp-scratch.note::8fc79737][8fc79737]]
(use-package ox-odt
  :ensure nil
  :config
  (progn
    ;; continually numbering captions without outline level
    (setq org-odt-display-outline-level 0)

    ;; useful for odt export using dvipng
    (setq org-format-latex-options (plist-put org-format-latex-options :html-scale 3.0))
    (setq org-odt-pixels-per-inch 300.0)))
;; 8fc79737 ends here

;; [[file:../gwp-scratch.note::*odt export][odt export:2]]
;; adopted from https://github.com/tumashu/emacs-helper/blob/master/eh-org.el
(defun gwp/clear-unwanted-space (text)
  "clear unwanted space when exporting org-mode to other formats"
  (let ((regexp "[[:multibyte:]]")
        (string text))
    ;; org-mode 默认将一个换行符转换为空格，但中文不需要这个空格，删除。
    (setq string
          (replace-regexp-in-string
           (format "\\(%s\\) *\n *\\(%s\\)" regexp regexp)
           "\\1\\2" string))
    ;; 删除粗体之后的空格
    (dolist (str '("</b>" "</code>" "</del>" "</i>"))
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\)\\(%s\\)[ ]+\\(%s\\)" regexp str regexp)
             "\\1\\2\\3" string)))
    ;; 删除粗体之前的空格
    (dolist (str '("<b>" "<code>" "<del>" "<i>" "<span class=\"underline\">"))
      (setq string
            (replace-regexp-in-string
             (format "\\(%s\\)[ ]+\\(%s\\)\\(%s\\)" regexp str regexp)
             "\\1\\2\\3" string)))
    string)
  )

(defun gwp/ox-odt-wash-text (text backend info)
  "导出 org file 时，删除中文之间不必要的空格。"
  (when (org-export-derived-backend-p backend 'odt 'html 'latex)
    (gwp/clear-unwanted-space text)
    )
  )

(add-hook 'org-export-filter-headline-functions #'gwp/ox-odt-wash-text)
(add-hook 'org-export-filter-paragraph-functions #'gwp/ox-odt-wash-text)
;; odt export:2 ends here

;; [[file:../gwp-scratch.note::27b71342][27b71342]]
(use-package org
  :config
  (gwp::dwim-leader-def
    :keymaps 'org-mode-map
    "g" 'org-goto                                ; goto
    "t" 'org-todo                                ; todo
    "e" 'org-edit-special                        ; edit
    "a" 'org-attach                              ; attach
    "b" 'gwp::org-babel-tangle-dwim              ; babel
    "n" 'gwp::org-babel-narrow-to-tangle-heading ; narrow
    "j" 'gwp::org-babel-tangle-jump-to-file      ; jump to tangled file
    )

  (gwp::dwim-leader-def
    :keymaps 'org-src-mode-map
    "b" 'gwp::org-babel-tangle-dwim
    "q" 'org-edit-src-exit
    ))
;; 27b71342 ends here

;; [[file:../gwp-scratch.note::dfee4224][dfee4224]]
;; org-insert-structure-template
;; 换到 org-edit-special 上更方便
(unbind-key "C-c ,") ; 默认为org-priority 很少用
(unbind-key "C-c C-," org-mode-map)
(bind-key "C-c C-," 'org-edit-special org-mode-map)
(bind-key "C-c C-," 'org-edit-src-exit org-src-mode-map)

;; 方便 rust 编辑
(require 'conf-mode)
(bind-key "C-c C-," #'gwp::org-babel-tangle-jump-to-org prog-mode-map)
(bind-key "C-c C-," #'gwp::org-babel-tangle-jump-to-org conf-toml-mode-map)
(bind-key "C-c C-M-," #'org-babel-detangle prog-mode-map)
(bind-key "C-c C-M-," #'org-babel-detangle conf-toml-mode-map)
(bind-key "C-c C-M-," #'gwp::org-babel-tangle-jump-to-file org-mode-map)

;; 文件按 C-c C-< 或 C-c C-> 调整 heading level
(unbind-key "C-c <" 'org-mode-map)
(unbind-key "C-c >" 'org-mode-map)
(bind-key "C-c >" 'org-demote-subtree org-mode-map)
(bind-key "C-c <" 'org-promote-subtree org-mode-map)

(gwp::local-leader-def
  :keymaps 'org-mode-map
  "a"  #'(:ignore t :which-key "attach")
  "aa" #'org-attach
  "an" #'gwp::org-attach-auto-directory)

(transient-define-prefix gwp::org-babel-transient ()
  ["babel"
   ("n" "toggle tangle" gwp/org-babel-tangle-no)
   ("j" "jump to tangled file" gwp::org-babel-tangle-jump-to-file)
   ("t" "tangle subtree" gwp/org-tangle-subtree)
   ("M-p" "prev src block" org-previous-block :transient t)
   ("M-n" "next src block" org-next-block :transient t)
   ]
  ["buffer"
   ("b" "switch org buffer" org-switchb)
   ])

(gwp::local-leader-def
  :keymaps 'org-mode-map
  "b"  #'(gwp::org-babel-transient :which-key "babel/buffer"))

(transient-define-prefix gwp::org-mark-transient ()
  [("." "mark element" org-mark-element)
   ("m" "push mark" org-mark-ring-push)
   ("n" "goto mark" org-mark-ring-goto :transient t)
   ("l" "mark link" gwp::org-mark-link)
   ("s" "mark src block" org-babel-mark-block)])

(gwp::local-leader-def
  :keymaps 'org-mode-map
  "m"  #'(gwp::org-mark-transient :which-key "mark"))

(transient-define-prefix gwp::org-toggle-transient ()
  [("i" "toggle item (-)" org-ctrl-c-minus)
   ("h" "toggle heading" org-toggle-heading)
   (":" "toggle (:)" org-toggle-fixed-width)
   ;; 可用 C-c C-x C-l
   ("L" "toggle latex preview" org-latex-preview)
   ;; 可用 C-c C-x v
   ("I" "toggle inline images" org-toggle-inline-images)
   ("c" "toggle checkbox" gwp::org-toggle-checkbox)
   ("s" "toggle org sidebar" org-sidebar-tree)
   ])

(gwp::local-leader-def
  :keymaps 'org-mode-map
  "-"  #'(org-ctrl-c-minus :which-key "toggle item (-)")
  "*"  #'(org-ctrl-c-star :which-key "toggle headline (*)")
  "t"  #'(gwp::org-toggle-transient :which-key "toggle")
  )

(gwp::local-leader-def
  :keymaps 'org-mode-map
  [tab] '(org-next-link :which-key "goto next link")
  [backtab] '(org-previous-link :which-key "goto next link"))

(gwp::goto-leader-def
  :keymaps 'org-mode-map
  "k" '(org-up-element :which-key "goto up element")
  "j" '(org-next-visible-heading :which-key "next visible heading")
  ;; "h" '(org-beginning-of-line :which-key "goto the beginning of visible line")
  ;; "l" '(org-end-of-line :which-key "goto the end of visible line")
  ;; "k" '(org-backward-heading-same-level :which-key "backward heading")
  ;; "j" '(org-forward-heading-same-level :which-key "forward heading")
  )

(general-define-key
 :keymaps '(org-mode-map)
 "M-l" #'org-metaright   ; doom中默认为 demote-subtree
 "M-h" #'org-metaleft    ; doom中默认为 promote-subtree
 "M-k" #'org-metaup
 "M-j" #'org-metadown
 "M-p" #'org-backward-element
 "M-n" #'org-forward-element
 )
;; dfee4224 ends here

;; [[file:../gwp-scratch.note::183d2d8f][183d2d8f]]
(provide 'init-org)
;; 183d2d8f ends here
