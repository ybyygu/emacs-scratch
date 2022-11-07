;; [[file:../gwp-scratch.note::4cd1adaf][4cd1adaf]]
;; -*- lexical-binding: t; -*-
;; 4cd1adaf ends here

;; [[file:../gwp-scratch.note::6c574db8][6c574db8]]
;; 本地使用时不生成 backup~ 文件, 但在服务器端有必要.
(unless init-no-x-flag
  (customize-set-variable 'make-backup-files nil))
;; 6c574db8 ends here

;; [[file:../gwp-scratch.note::20ff3f1b][20ff3f1b]]
(use-package crux
  :config
  (bind-key [remap move-beginning-of-line] #'crux-move-beginning-of-line))
;; 20ff3f1b ends here

;; [[file:../gwp-scratch.note::f3c25d9e][f3c25d9e]]
(use-package transient
  :commands (transient-define-prefix define-infix-argument define-suffix-command)
  :custom
  ;; 延时显示
  (transient-show-popup 0.5)
  :bind
  (:map transient-map
        ([escape] . transient-quit-one)))
;; f3c25d9e ends here

;; [[file:../gwp-scratch.note::0a9921e5][0a9921e5]]
(setq bookmark-file-coding-system 'utf-8)
(setq magit-git-output-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
;; 0a9921e5 ends here

;; [[file:../gwp-scratch.note::3eff5fa2][3eff5fa2]]
(defun gwp::duplicate-region (beg end)
  (interactive "r")
  (save-excursion
    (let* ((beg (or beg (region-beginning)))
           (end (or end (region-end)))
           (region (buffer-substring beg end)))
      (goto-char end)
      (insert region))))

(defun gwp::duplicate-line (&optional stay)
  (save-excursion
    (move-end-of-line nil)
    (save-excursion
      (insert (buffer-substring (point-at-bol) (point-at-eol))))
    (newline)))

(defun gwp::duplicate-line-or-region()
  "复制当前行或选定区域"
  (interactive)
  (if (region-active-p)
      ;; 也可用 crux-duplicate-current-line-or-region
      (call-interactively #'gwp::duplicate-region)
    (gwp::duplicate-line)))
;; 3eff5fa2 ends here

;; [[file:../gwp-scratch.note::b5a74212][b5a74212]]
(setq kill-ring-max 999)

;; 粘贴时删除区域中的内容, 不污染clipboard, 方便连续yank.
(defun gwp::yank-dwim (arg)
  "粘贴并覆盖选定区域. 如果以C-u调用则提示从kill-ring中选择"
  (interactive "P")
  (when (region-active-p)
    (call-interactively #'delete-region))
  (if (equal arg '(4))                  ; C-u
      (call-interactively #'yank-pop)
    (call-interactively #'yank)))
(global-set-key (kbd "C-y") #'gwp::yank-dwim)

;; 保持和terminal中的行为一致: 删除选定区域或向后一个单词
(defun gwp::ctrl-w-dwim ()
  (interactive)
  (if (region-active-p)
      (call-interactively #'kill-region)
    (call-interactively #'backward-kill-word)))

;; (map! "C-w" #'gwp::ctrl-w-dwim); cut, copy: Alt-w
(global-set-key (kbd "C-w") #'gwp::ctrl-w-dwim)
;; 删除到行尾或删除整行
(global-set-key (kbd "C-k") #'crux-smart-kill-line)
;; b5a74212 ends here

;; [[file:../gwp-scratch.note::7d5caf69][7d5caf69]]
(defun gwp::ctrl-d-dwim (prefix)
  "清除区域或复制区域"
  (interactive "P")
  (if prefix                  ; C-u
      (call-interactively #'gwp::duplicate-line-or-region)
    (call-interactively #'gwp::delete-char-or-region)))

(defun gwp::delete-char-or-region()
  "清除光标前字符或选定区域"
  (interactive)
  (if mark-active
      (call-interactively #'delete-region)
    (delete-char 1)))

(global-set-key (kbd "C-d") #'gwp::ctrl-d-dwim)
;; 7d5caf69 ends here

;; [[file:../gwp-scratch.note::9e3bdda9][9e3bdda9]]
(use-package emacs
  :ensure nil
  :custom
  ;; 访问软链接文件时使用真实的文件路径, 避免文件项目
  (find-file-visit-truename t)
  (vc-follow-symlinks t))
;; 9e3bdda9 ends here

;; [[file:../gwp-scratch.note::9f41280c][9f41280c]]
(defun gwp::undo-dwim ()
  (interactive)
  (if (region-active-p)
      (let ((current-prefix-arg '(4)))     ; C-u
        (call-interactively #'undo))
    (call-interactively #'undo)))

;; 默认evil的undo会将多个小操作合并为一个大的, undo时很不适应.
;; (setq evil-want-fine-undo t)
;; 9f41280c ends here

;; [[file:../gwp-scratch.note::d9848746][d9848746]]
(use-package recentf
  :ensure nil
  :custom
  ;; then run M-x recentf-cleanup to make it work.
  (recentf-exclude '("/tmp/"
                     "/ssh:"
                     "/sudo:"
                     "/scp:"
                     "/scpx:"
                     "/ssh:"
                     ;; "\\.pdf$"
                     "\\.png$"
                     "autosave$"
                     ;; "\\.odt$"
                     "\\.note_archive$"
                     "_workspaces"
                     ".*/COMMIT_EDITMSG$" ; magit 临时编辑文件
                     ;; ".*/$"               ; 剔除目录
                     ))
  (recentf-max-saved-items 9999)   ; the default is only 20
  (recentf-keep '(gwp::recentf-keep-p))
  ;; clean up items when has been idle 1 hour
  ;; (recentf-auto-cleanup 3600)
  (recentf-auto-cleanup 'never)         ; doom 在退出时清理
  :config
  (recentf-mode 1))

(defun gwp::recentf-keep-p (file)
  "仅保留本地可读文件"
  (not (file-remote-p file))
  ;; (and (not (file-remote-p file))
  ;;      (not (file-directory-p file)))
  )

;; 定义保存临时文件列表. 默认仅当退出 emacs 才保存, 这会丢掉不少数据.
;; 2022-10-14 好像会丢记录, 问题再排查
;; (require 'midnight)
;; (add-hook! midnight #'recentf-save-list)
;; d9848746 ends here

;; [[file:../gwp-scratch.note::66843b91][66843b91]]
(use-package bookmark
  :ensure nil
  :custom
  (bookmark-default-file
   (expand-file-name "bookmarks" user-emacs-directory))
  ;; 修改 bookmark 后立刻保存, 防止冲突. 默认仅当退出 emacs 时保存
  (bookmark-save-flag 1)
  ;; 不提示, 直接读取硬盘中的内容. 避免不同电脑间冲突
  (bookmark-watch-bookmark-file 'silent)
  :config
  ;; 2022-10-31: 根据手册, 仅需设置 bookmark-set-flag, 以下代码仅供参考
  ;; (defun gwp::bookmark-save-advice (&rest r)
  ;;   (let ((save-silently t))
  ;;     (bookmark-save)))
  ;; (advice-add 'bookmark-set :after #'gwp::bookmark-save-advice)
  ;; (advice-add 'bookmark-rename :after #'gwp::bookmark-save-advice)
  ;; (advice-add 'bookmark-delete :after #'gwp::bookmark-save-advice)
  )
;; 66843b91 ends here

;; [[file:../gwp-scratch.note::e4fc036b][e4fc036b]]
;; 要保证 C-u C-@ 连续调用有效
(setq set-mark-command-repeat-pop nil)

(defun gwp::jump-to-previous-mark ()
  (interactive)
  (let ((current-prefix-arg '(4)))     ; C-u
    (call-interactively #'set-mark-command)))

;; 根据手册: C-SPC C-SPC两次
;; 仅标记当前位置(push to mark ring), 但不选择
(defun gwp::mark-current-position ()
  (interactive)
  (call-interactively #'set-mark-command)
  (deactivate-mark))

(defun gwp::mark-and-save-buffer()
  "标记光标所在位置, 并保存buffer"
  (interactive)
  (call-interactively #'gwp::mark-current-position)
  ;; 保存时同当清理光标处空格
  (call-interactively #'delete-trailing-whitespace)
  (save-buffer))

(setq global-mark-ring-max 99
      mark-ring-max 99)
;; e4fc036b ends here

;; [[file:../gwp-scratch.note::e48dc36a][e48dc36a]]
;; https://stackoverflow.com/a/27661338
(defun marker-is-point-p (marker)
  "test if marker is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

(defun push-mark-maybe ()
  "push mark onto `global-mark-ring' if mark head or tail is not current location"
  (if (not global-mark-ring) (error "global-mark-ring empty")
    (unless (or (marker-is-point-p (car global-mark-ring))
                (marker-is-point-p (car (reverse global-mark-ring))))
      (push-mark))))


(defun backward-global-mark ()
  "use `pop-global-mark', pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark))

(defun forward-global-mark ()
  "hack `pop-global-mark' to go in reverse, pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (setq global-mark-ring (nreverse global-mark-ring))
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark)
  (setq global-mark-ring (nreverse global-mark-ring)))
;; e48dc36a ends here

;; [[file:../gwp-scratch.note::ebb32bb1][ebb32bb1]]
;; https://github.com/deestan/emacs/blob/master/emacs-goodies-el/marker-visit.el
;;
;;; marker-visit.el --- navigate through a buffer's marks in order

;; Copyright (C) 2001 Benjamin Rutt
;;
;; Maintainer: Benjamin Rutt <brutt@bloomington.in.us>
;; Version: 1.1

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, send e-mail to
;; this program's maintainer or write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file provides a simple way to navigate among marks in a
;; buffer.  C-u C-SPC is similar, but takes you haphazardly around the
;; buffer.  Setting bookmarks is a lot of extra work if you just want
;; to jump around your buffer quickly; plus, you have to come up with
;; a name for every bookmark.

;; All the marks you've left while editing a buffer serve as bread
;; crumb trails of areas in the buffer you've edited.  It is
;; convenient to navigate back and forth among these marks in order.
;; This file provides two methods to do just that, marker-visit-prev
;; and marker-visit-next.  These two functions will take you, from
;; point, to the nearest mark in either direction.  The function
;; marker-visit-truncate-mark-ring will truncate the mark ring.

;; The marks you can visit in a buffer consist of: "the mark" plus the
;; contents of the mark-ring.

;;; Usage:

;; put this file in your load-path and add the line
;;
;; (require 'marker-visit)
;;
;; to your ~/.emacs file.
;;
;; This package is most useful when some easy-to-press keys are bound
;; to the functions marker-visit-prev and marker-visit-next.  See C-h
;; i m Emacs RET m Key Bindings RET for info on emacs key bindings.

;;; History:

;; 1.0 -> 1.1 Incorporated patch from Colin Walters to make the code
;; consistent with elisp code conventions mentioned in
;; (Info-goto-node "(elisp) Coding Conventions").

;;; Code:

;;utility remove-dupes function
(defun marker-visit-remove-dupes (ls)
  (cond
   ((null ls) '())
   ((member (car ls) (cdr ls)) (marker-visit-remove-dupes (cdr ls)))
   (t (cons (car ls) (marker-visit-remove-dupes (cdr ls))))))

;;create a sorted list of marks, including the point as mark, the
;;mark, and the contents of the mark-ring.
(defun marker-visit-get-sorted-mark-set (current-point-mark)
  (marker-visit-remove-dupes
   (sort
    (append (cons current-point-mark
                  (if (mark-marker) (list (mark-marker)) nil))
            (mapcar (lambda (id) id) mark-ring))
    (lambda (a b) (< a b)))))

(defun marker-visit-no-markers-p ()
  (and (null mark-ring)
       (or (not (mark-marker))
           (not (marker-position (mark-marker))))))

(defun marker-visit-warn (error-message)
  (message error-message)
  (beep))

;;;###autoload
(defun marker-visit-prev ()
  "From point, visit the nearest mark earlier in the buffer."
  (interactive)
  (if (marker-visit-no-markers-p)
      (marker-visit-warn "Mark does not point anywhere")
    (let* ((current-point-mark (point-marker))
           (sorted-marks (marker-visit-get-sorted-mark-set current-point-mark))
           (dest-mark nil))
      (while (not (equal current-point-mark (car sorted-marks)))
        (setq dest-mark (car sorted-marks))
        (setq sorted-marks (cdr sorted-marks)))
      (if (not dest-mark)
          (marker-visit-warn "No previous mark to visit")
        (goto-char dest-mark)
        (org-mark-jump-unhide)))))

;;;###autoload
(defun marker-visit-next ()
  "From point, visit the nearest mark later in the buffer."
  (interactive)
  (if (marker-visit-no-markers-p)
      (marker-visit-warn "Mark does not point anywhere")
    (let* ((current-point-mark (point-marker))
           (sorted-marks (marker-visit-get-sorted-mark-set current-point-mark))
           (dest-mark nil)
           (done nil))
      (while (not done)
        (if (equal current-point-mark (car sorted-marks))
            (progn
              (setq dest-mark (cadr sorted-marks))
              (setq done t))
          (setq sorted-marks (cdr sorted-marks))))
      (if (not dest-mark)
          (marker-visit-warn "No next mark to visit")
        (goto-char dest-mark)
        (org-mark-jump-unhide)))))

;;;###autoload
(defun marker-visit-truncate-mark-ring ()
  "Truncate the `mark-ring'."
  (interactive)
  (setq mark-ring nil))
;; ebb32bb1 ends here

;; [[file:../gwp-scratch.note::00b43976][00b43976]]
(require 'hydra)
(defhydra gwp::hydra-mark-ring-pop ()
  "goto last location"
  ("SPC" gwp::jump-to-previous-mark "prev mark")          ; 在org中可自动打开折叠的内容
  ("j" marker-visit-next "next mark")
  ("k" marker-visit-prev "prev mark")                     ; NOTE: org折叠的内容不会打开
  ("C-p" backward-global-mark "prev mark (global)")         ;
  ("C-n" forward-global-mark "next mark (global)")          ;
  ("r" gwp::org-show-context-at-point "org show context") ; 在org时: 跳转到被折叠的headline中很有用
  ("q" nil "quit"))
;; 00b43976 ends here

;; [[file:../gwp-scratch.note::1a29ba42][1a29ba42]]
(setq
 ;; doom里已默认为true
 auto-save-default t
 ;; 默认为5秒. 这里改大一些, 避免编辑时自动保存太快, 光标前的空格被吞掉
 auto-save-visited-interval 30)

;; 自动保存至当前文件名, 而非临时文件
(auto-save-visited-mode +1)
;; 1a29ba42 ends here

;; [[file:../gwp-scratch.note::649668b1][649668b1]]
;; 高亮括号配对
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  ;; (show-paren-style 'mixed)
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t)
  (show-paren-ring-bell-on-mismatch t)
  :custom-face
  (show-paren-match ((t (:foreground "gray100" :background "#9c7618" :weight bold)))))

(use-package smartparens
  :custom
  ;; Overlays are too distracting and not terribly
  ;; helpful. show-parens does this for us already (and is faster),
  ;; so...
  (sp-highlight-pair-overlay nil)
  (sp-highlight-wrap-tag-overlay nil)
  (sp-highlight-wrap-overlay nil)
  :hook
  (prog-mode . smartparens-mode)
  (org-mode . smartparens-mode))
;; (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
;; 649668b1 ends here

;; [[file:../gwp-scratch.note::dda75ec0][dda75ec0]]
(use-package simple
  :ensure nil
  :custom
  ;; 从其它程序复制的内容也放至在kill-ring中, 不会因为emacs的操作而覆盖之前的内容
  (save-interprogram-paste-before-kill t))
;; dda75ec0 ends here

;; [[file:../gwp-scratch.note::2bf53d30][2bf53d30]]
(use-package autorevert
  :ensure nil
  ;; revert buffers when their files/state have changed
  :hook (focus-in . doom-auto-revert-buffers-h)
  :hook (after-save . doom-auto-revert-buffers-h)
  :hook (doom-switch-buffer . doom-auto-revert-buffer-h)
  :hook (doom-switch-window . doom-auto-revert-buffer-h)
  :config
  (setq auto-revert-verbose t ; let us know when it happens
        auto-revert-use-notify nil
        auto-revert-stop-on-user-input nil
        ;; Only prompts for confirmation when buffer is unsaved.
        revert-without-query (list "."))

  ;; `auto-revert-mode' and `global-auto-revert-mode' would, normally, abuse the
  ;; heck out of file watchers _or_ aggressively poll your buffer list every X
  ;; seconds. Too many watchers can grind Emacs to a halt if you preform
  ;; expensive or batch processes on files outside of Emacs (e.g. their mtime
  ;; changes), and polling your buffer list is terribly inefficient as your
  ;; buffer list grows into the hundreds.
  ;;
  ;; Doom does this lazily instead. i.e. All visible buffers are reverted
  ;; immediately when a) a file is saved or b) Emacs is refocused (after using
  ;; another app). Meanwhile, buried buffers are reverted only when they are
  ;; switched to. This way, Emacs only ever has to operate on, at minimum, a
  ;; single buffer and, at maximum, ~10 buffers (after all, when do you ever
  ;; have more than 10 windows in any single frame?).
  (defun doom-auto-revert-buffer-h ()
    "Auto revert current buffer, if necessary."
    (unless (or auto-revert-mode (active-minibuffer-window))
      (let ((auto-revert-mode t))
        (auto-revert-handler))))

  (defun doom-visible-buffers (&optional buffer-list)
    "Return a list of visible buffers (i.e. not buried)."
    (let ((buffers (delete-dups (mapcar #'window-buffer (window-list)))))
      (if buffer-list
          (cl-delete-if (lambda (b) (memq b buffer-list))
			buffers)
	(delete-dups buffers))))

  (defun doom-auto-revert-buffers-h ()
    "Auto revert stale buffers in visible windows, if necessary."
    (dolist (buf (doom-visible-buffers))
      (with-current-buffer buf
        (doom-auto-revert-buffer-h)))))
;; 2bf53d30 ends here

;; [[file:../gwp-scratch.note::7b0203a1][7b0203a1]]
(provide 'init-core)
;; 7b0203a1 ends here
