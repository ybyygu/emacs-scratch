;; [[file:../gwp-scratch.note::0894dac2][0894dac2]]
(defconst gwp::dwim-leader-key "s-w")
(defconst gwp::goto-leader-key "s-g")
(defconst gwp::local-leader-key "s-,")
;; 0894dac2 ends here

;; [[file:../gwp-scratch.note::f3f75fec][f3f75fec]]
(use-package general
  :demand t
  :after meow
  :config
  ;;  prevent Key sequence starts with a non-prefix key errors
  (general-auto-unbind-keys)
  ;; 定义 "," 及 "g", "w" 开头的按键序列.
  (general-create-definer gwp::goto-leader-def
    :keymaps 'general-override-mode-map
    :prefix gwp::goto-leader-key)
  (general-create-definer gwp::local-leader-def
    :keymaps 'general-override-mode-map
    :prefix gwp::local-leader-key)
  (general-create-definer gwp::dwim-leader-def
    :keymaps 'meow-normal-state-keymap
    :prefix gwp::dwim-leader-key)

  ;; 方便定义在 Insert 状态下的一些编辑命令
  (general-create-definer gwp::text-edit-def
    ;; :prefix "C-c"
    :keymaps '(meow-insert-state-keymap))

  ;; 用于 help 及只读类文件
  (general-create-definer gwp::text-view-def
    :keymaps '(meow-motion-state-keymap meow-normal-state-keymap))

  ;; 高优先级
  ;; (general-create-definer gwp::local-def :keymaps 'local)

  ;; space leader key
  (general-create-definer gwp::leader-def
    :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
    :prefix "SPC"
    )

  ;; 取消某些容易误按, 不习惯的键
  (general-unbind "C-v" "C-z" "C-x C-z"))
;; f3f75fec ends here

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

;; [[file:../gwp-scratch.note::4e63ecbf][4e63ecbf]]
;;; editor/core/config.el -*- lexical-binding: t; -*-

;;;###autoload
;; https://www.emacswiki.org/emacs/CopyingWholeLines
(defun gwp::copy-current-line (&optional arg)
  (interactive "p")
  (let ((buffer-read-only t)
        (kill-read-only-ok t))
    (kill-whole-line arg)))

;;;###autoload
(defun gwp::meow-insert-at-the-beginning ()
  (interactive)
  (if mark-active
      (call-interactively #'meow-insert-mode)
    (meow-join 1)
    (meow-append)))

;;;###autoload
(defun gwp::meow-insert-at-the-end ()
  (interactive)
  (if mark-active
      (call-interactively #'meow-insert-mode)
    (meow-line 1)
    (meow-append)))

;;;###autoload
;; https://www.gnu.org/software/emacs/manual/html_node/efaq/Matching-parentheses.html
(defun gwp::match-paren (arg)
  (interactive "p")
  (cond ((looking-at "\\s(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s)") (forward-char 1) (backward-list 1))
        (t (call-interactively #'meow-block))))

;;;###autoload
(defun gwp::meow-change-to-the-end ()
  (interactive)
  (meow-insert)
  (kill-line))

;;;###autoload
(defun gwp::meow-change-whole-line ()
  (interactive)
  (call-interactively #'crux-move-beginning-of-line)
  (call-interactively #'gwp::meow-change-to-the-end))
;; 4e63ecbf ends here

;; [[file:../gwp-scratch.note::672c2d79][672c2d79]]
(defun meow/setup-normal ()
  ;; normal commands
  (meow-normal-define-key
   ;; (cons "SPC" meow-space-keymap)
   '("<escape>" . keyboard-quit)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("0" . meow-expand-0)
   '("-" . negative-argument)
   ;; 常规移动操作
   '("h" . meow-left)
   '("j" . meow-next)
   '("k" . meow-prev)
   '("l" . meow-right)
   '("^" . meow-back-to-indentation)
   ;; 常规编辑操作
   '("i" . meow-insert)
   '("I" . gwp::meow-insert-at-the-beginning)
   '("a" . meow-append)
   '("A" . gwp::meow-insert-at-the-end)
   '("x" . meow-kill)                   ; 同 vi, 剪入 king-ring, 无选区时等效于 C-x 按键
   '("y" . meow-save)                   ; 同 vi, 复制到 king-ring, 无选区时复制当前行
   '("c" . meow-change)                 ; 同 vi, 删除选区内容, 无选区时等效于 C-c 按键
   '("C" . gwp::meow-change-whole-line) ; 同 vi, 修改整行
   '("K" . gwp::meow-change-to-the-end) ; 像 C-k, 但进入 insert mode
   '("p" . meow-yank)
   '("O" . meow-open-above)
   '("J" . crux-top-join-line)      ; 同vi, 合并下一行至当前行
   '("r" . meow-change-char)        ; 删除当前字符或选区(不进入 kill-ring), 同时进入 insert state
   '("d" . meow-delete)             ; 删除当前字符或选区(不进入 kill-ring)
   '("DEL" . meow-backward-delete)
   '("D" . meow-kill-whole-line)
   ;; 选区扩展操作
   '("." . meow-line)                ; 向下扩选一行, 按 "-." 向上扩选
   '("e" . meow-next-word)           ; 向前扩选, 以 word 为单位
   '("E" . meow-next-symbol)         ; 向前扩选, 以 symbol 为单位 (包括连字符等)
   '("b" . meow-back-word)           ; 反向操作, 等效于 "-e"
   '("B" . meow-back-symbol)         ; 反向操作, 等效于 "-E"
   '("o" . meow-reverse)             ; 反转选区方向. 若无选区, 则相当于 vi 中为 o
   '("u" . gwp::undo-dwim)
   '("U" . meow-pop-selection)       ; 撤销选择
   ;; 搜索与跳转
   '("/" . meow-visit)            ; 快速搜索, 按C-M-j 搜索任意字串
   '("n" . meow-search)           ; 向选区方向搜索, 可按 o 键改变当前选区方向
   '("f" . meow-find)             ; 含搜索字符
   '("t" . meow-till)             ; 不含搜索字符
   '("m" . point-to-register)
   '("`" . jump-to-register)
   ;; 常规选择
   '("%" . gwp::match-paren)
   '("*" . meow-mark-symbol)
   ;; '("q" . meow-mark-word)
   '("s" . meow-inner-of-thing)
   '("S" . meow-bounds-of-thing)
   '("(" . meow-beginning-of-thing)
   '(")" . meow-end-of-thing)
   '(";" . meow-cancel-selection)
   '("v" . meow-cancel-selection) ; 仿 vi
   '("V" . meow-block)            ; 逐级扩选, 按U 回退, 可替代 expand-region
   '("G" . meow-grab)             ; 相当于 vi 中的 visual mode
   '("C-v" . meow-grab)
   ;; 特殊功能
   '("]" . sp-unwrap-sexp)
   '("R" . sp-unwrap-sexp)                         ; 比] 容易按一些
   '("$" . ispell-word)
   '("'" . repeat)                      ; 重复上一个命令
   '("=" . meow-goto-line)
   ;; '("z" . avy-goto-char-in-line)
   '("z" . meow-pop-selection)
   '("Z" . repeat-complex-command)      ; 重复上一个需要 minibuffer 输入的命令
   )

  ;; 当无选区时执行的功能
  (setq
   meow-selection-command-fallback
   '(
     (meow-reverse . meow-open-below)
     (meow-kill . meow-keypad-start)    ; for C-x
     (meow-change . meow-keypad-start)  ; for C-c
     (meow-save . gwp::copy-current-line)
     ;; (meow-pop-selection . meow-pop-grab)
     (meow-beacon-change . meow-beacon-change-char)
     (meow-cancel-selection . meow-right-expand) ; 仿vi, 取消选择或扩选
     )))
;; 672c2d79 ends here

;; [[file:../gwp-scratch.note::f4be1bd9][f4be1bd9]]
;; Leader Key
(defun meow/setup-leader ()
  ;; 与 which-key 集成度不高
  ;; (meow-leader-define-key
  ;;  '("/" . meow-keypad-describe-key)
  ;;  '("?" . meow-cheatsheet))
  ;; (meow-normal-define-key
  ;;  '("," . "s-,")
  ;;  '("g" . "s-g")
  ;;  '("w" . "s-w")
  ;;  )

  (general-define-key
   :keymaps 'meow-normal-state-keymap
   "," (general-simulate-key "s-," :which-key "local")
   "g" (general-simulate-key "s-g" :which-key "goto")
   "w" (general-simulate-key "s-w" :which-key "dwim")
   :keymaps 'meow-motion-state-keymap
   "," (general-simulate-key "s-,")
   "g" (general-simulate-key "s-g")
   "w" (general-simulate-key "s-w")
   "j" (general-simulate-key "C-n")
   "k" (general-simulate-key "C-p")
   "h" (general-simulate-key "C-b")
   "l" (general-simulate-key "C-f")
   "y" #'meow-save
   "v" #'meow-cancel-selection
   ))
;; f4be1bd9 ends here

;; [[file:../gwp-scratch.note::60483a2d][60483a2d]]
;; 比如 dired, magit 生成的 buffer, 也许单独处理更好?
(defun meow/setup-motion ()
  (meow-motion-overwrite-define-key
   '("j"  "meow-next")
   '("k"  "meow-prev")
   '("<escape>" . ignore)
   )
  (meow-motion-overwrite-define-key
   '("," . "s-,")
   '("g" . "s-g")
   '("w" . "s-w")
   ))
;; 60483a2d ends here

;; [[file:../gwp-scratch.note::9a723a5b][9a723a5b]]
(use-package meow
  :demand t
  :init
  (meow-global-mode 1)
  :custom
  ;; 扩选指示字符显示延时
  (meow-expand-hint-remove-delay 5.0)
  ;; 默认在 org 中不显示扩选指示字符
  (meow-expand-exclude-mode-list nil)
  ;; (meow-cursor-type-normal 'hbar)  :config
  :config
  (setq meow-cursor-type-normal '(box . 2))
  (setq meow-cursor-type-insert '(bar . 2))
  ;; (setq meow-cursor-type-region-cursor 'bar)
  ;; https://github.com/meow-edit/meow/discussions/87
  ;; 选择文字区域时光标位置与 vim 及其它软件一致
  ;; 2022-03-19: 会出一些怪问题, 禁用
  ;; (setq meow-use-cursor-position-hack t)
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow/setup-normal)
  (meow/setup-leader)
  (meow/setup-motion)
  ;; If you want relative line number in NORMAL s tate(for display-line-numbers-mode)
  (meow-setup-line-number)
  ;; If you need setup indicator, see `meow-indicator' for customizing  hand.
  (meow-setup-indicator))
;; 9a723a5b ends here

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

;; [[file:../gwp-scratch.note::6cb02a16][6cb02a16]]
(gwp::goto-leader-def
  :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
  "g" (general-simulate-key "M-<" :which-key "goto first line")
  "e" (general-simulate-key "M->" :which-key "goto last line")
  "h" (general-simulate-key "C-a" :which-key "goto the beggining of line")
  "l" (general-simulate-key "C-e" :which-key "goto the end of line")
  "." 'goto-line
  )
;; 6cb02a16 ends here

;; [[file:../gwp-scratch.note::4cf8c86c][4cf8c86c]]
(gwp::leader-def
 "SPC" '(gwp::mark-and-save-buffer :which-key "Save buffer")
 "," '(gwp::jump-to-previous-mark :which-key "Jump to previous mark")
 "`" '(gwp::switch-to-previous-buffer :which-key "Switch to previous buffer")
 "u" '(universal-argument :which-key "Universal argument")
 )
;; 4cf8c86c ends here

;; [[file:../gwp-scratch.note::33105bcf][33105bcf]]
(gwp::leader-def
 "b" '(:ignore t :which-key "buffer")
 "bb" '(switch-to-buffer :which-key "switch buffer")
 )
;; 33105bcf ends here

;; [[file:../gwp-scratch.note::45f27ad1][45f27ad1]]
(gwp::leader-def
 "f" '(:ignore t :which-key "file")
 "fr" '(counsel-recentf :which-key "recent files")
 "fj" '(dired-jump :which-key "jump to dired buffer")
 "fJ" '(dired-jump-other-window :which-key "jump to dired buffer (other window)")
 "fb" '(counsel-bookmark :which-key "open bookmarks")
 )
;; 45f27ad1 ends here

;; [[file:../gwp-scratch.note::860eb4b2][860eb4b2]]
(gwp::leader-def
 "q" '(:ignore t :which-key "quit/session")
 "qq" '(save-buffers-kill-terminal :which-key "Quit Emacs")
 "qk" '(save-buffers-kill-emacs :which-key "Kill Emacs (and daemon)")
 )
;; 860eb4b2 ends here

;; [[file:../gwp-scratch.note::e7792733][e7792733]]
(gwp::leader-def
 "h" '(:keymap help-map :which-key "Help" :package emacs))
;; e7792733 ends here

;; [[file:../gwp-scratch.note::c54b17b5][c54b17b5]]
(gwp::leader-def
 "w" '(:keymap gwp::window-map :which-key "Window" :package emacs))
;; c54b17b5 ends here

;; [[file:../gwp-scratch.note::185fc283][185fc283]]
(gwp::leader-def
 "g" '(:keymap gwp::magit-map :which-key "Magit" :package magit))
;; 185fc283 ends here

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'init-edit)
;; provide:1 ends here
