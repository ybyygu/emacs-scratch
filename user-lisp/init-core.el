;; [[file:../gwp-scratch.note::b5a74212][b5a74212]]
(setq kill-ring-max 999)
	 
;; 粘贴时删除区域中的内容, 不污染clipboard, 方便连续yank.
(defun gwp::yank-dwim (arg)
  "粘贴并覆盖选定区域. 如果以C-u调用则提示从kill-ring中选择"
  (interactive "P")
  (when (region-active-p)
    (call-interactively #'delete-region))
  (if (equal arg '(4))                  ; C-u
      (call-interactively #'counsel-yank-pop)
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
  (deactivate-mark)
  )

(defun gwp::mark-and-save-buffer()
  "标记光标所在位置, 并保存buffer"
  (interactive)
  (call-interactively #'gwp::mark-current-position)
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
      (if dest-mark
	  (goto-char dest-mark)
	(marker-visit-warn "No previous mark to visit")))))

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
      (if dest-mark
	  (goto-char dest-mark)
	(marker-visit-warn "No next mark to visit")))))

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

;; [[file:../gwp-scratch.note::7b0203a1][7b0203a1]]
(provide 'init-core)
;; 7b0203a1 ends here
