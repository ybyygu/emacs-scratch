;; [[file:../gwp-scratch.note::e2f6b646][e2f6b646]]
(use-package org
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

;; [[file:../gwp-scratch.note::0caa1907][0caa1907]]
(use-package org-superstar
  :init
  ;; ◉ ○ ◆ » ◇ ▶ ▷
  (setq org-superstar-headline-bullets-list '("☰" "▶" "▷" "»"))
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
;; 0caa1907 ends here

;; [[file:../gwp-scratch.note::99465500][99465500]]
;; 编辑代码时在下方新开窗口
;;(setq org-src-window-setup 'split-window-below)
(setq org-src-window-setup 'current-window)
;; 99465500 ends here

;; [[file:../gwp-scratch.note::e9fca5dc][e9fca5dc]]
(with-eval-after-load 'ob
  (setq org-structure-template-alist
        '(
          ("py" . "src python :results output")
          ("rs" . "src rust")
          ("el" . "src emacs-lisp")
          ("sh" . "src sh")
          )))
;; e9fca5dc ends here

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
  (let ((tangle-file (cdr (assq :tangle (nth 2 (org-babel-get-src-block-info 'light)))))
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

;; [[file:../gwp-scratch.note::dfee4224][dfee4224]]
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
 :keymaps 'org-mode-map
 "M-l" #'org-metaright   ; doom中默认为 demote-subtree
 "M-h" #'org-metaleft    ; doom中默认为 promote-subtree
 "M-k" #'org-metaup
 "M-j" #'org-metadown
 "M-p" #'org-backward-element
 "M-n" #'org-forward-element
 )

(gwp::dwim-leader-def
  :keymaps 'org-mode-map
  "g" 'counsel-org-goto                        ; goto
  "t" 'org-todo                                ; todo
  "e" 'org-edit-special                        ; edit
  "a" 'org-attach                              ; attach
  "b" 'gwp::org-babel-tangle-dwim              ; babel
  "n" 'gwp::org-babel-narrow-to-tangle-heading ; narrow
  "j" 'gwp::org-babel-tangle-jump-to-file      ; jump to tangled file
  )

(gwp::dwim-leader-def
  :keymaps 'org-src-mode-map
  ;; "b" 'gwp/org-babel-tangle-dwim
  "q" 'org-edit-src-exit
  )
;; dfee4224 ends here

;; [[file:../gwp-scratch.note::183d2d8f][183d2d8f]]
(provide 'init-org)
;; 183d2d8f ends here
