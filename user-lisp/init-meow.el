;; [[file:../gwp-scratch.note::1d60bcbf][1d60bcbf]]
;; -*- lexical-binding: t; -*-
;; 1d60bcbf ends here

;; [[file:../gwp-scratch.note::4e63ecbf][4e63ecbf]]
(require 'meow)

;;;###autoload
;; https://www.emacswiki.org/emacs/CopyingWholeLines
(defun gwp::copy-current-line (&optional arg)
  (interactive "p")
  (let ((buffer-read-only t)
        (kill-read-only-ok t))
    (kill-whole-line arg)))

;; 与 meow-sync-grab 类似, 但替换方向相反. 感觉这个更有用一些.
;;;###autoload
(defun gwp::meow-sync-with-secondary ()
  "将二级选区中的内容替换当前选区"
  (interactive)
  (let* ((s-sec (meow--second-sel-get-string)))
    (when s-sec
      (when (region-active-p)
        (delete-region (region-beginning) (region-end)))
      (insert s-sec))))

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

;; [[file:../gwp-scratch.note::802d8bd9][802d8bd9]]
(require 'avy)

;;;###autoload
(defun gwp::avy-meow-find ()
  "Jump to one of the current meow find candidates."
  (interactive)
  (let* ((isearch-regexp t)
         (isearch-string (car regexp-search-ring)))
    (call-interactively #'avy-isearch)))
;; 802d8bd9 ends here

;; [[file:../gwp-scratch.note::00b73661][00b73661]]
;;;###autoload
(defun meow/setup-states ()
  ;; 按自己需要, 逐个加
  (setq meow-mode-state-list
        '((fundamental-mode . normal)
          (text-mode . normal)
          (prog-mode . normal)
          (help-mode . motion)
          (org-agenda-mode . motion)
          ;; start shell/eshell in insert
          (shell-mode . insert)
          (eshell-mode . insert)
          ))
  ;; (add-to-list 'meow-mode-state-list '(text-mode . normal))
  ;; (add-to-list 'meow-mode-state-list '(prog-mode . normal))
  ;; (add-to-list 'meow-mode-state-list '(help-mode . motion))
  ;; (add-to-list 'meow-mode-state-list '(org-agenda-mode . motion))
  ;; (add-to-list 'meow-mode-state-list '(shell-mode . insert))
  ;; (add-to-list 'meow-mode-state-list '(eshell-mode . insert))
  )
;; 00b73661 ends here

;; [[file:../gwp-scratch.note::672c2d79][672c2d79]]
(require 'smartparens)

(defun meow/setup-normal ()
  ;; normal commands
  (meow-normal-define-key
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
   '("u" . gwp::undo-dwim)
   ;; 选区扩展操作
   '("." . meow-line)           ; 向下扩选一行, 按 "-." 向上扩选
   '("e" . meow-next-word)      ; 向前扩选, 以 word 为单位
   '("E" . meow-next-symbol)    ; 向前扩选, 以 symbol 为单位 (包括连字符等)
   '("b" . meow-back-word)      ; 反向操作, 等效于 "-e"
   '("B" . meow-back-symbol)    ; 反向操作, 等效于 "-E"
   '("o" . meow-reverse)        ; 反转选区方向. 若无选区, 则相当于 vi 中为 o
   ;; 搜索与跳转
   '("/" . meow-visit)             ; 快速搜索, 按C-M-j (ivy), M-RET (consult) 搜索任意字串
   '("n" . meow-search)            ; 向选区方向搜索, 可按 o 键改变当前选区方向
   '("N" . meow-pop-search)        ; 恢复上一次搜索
   '("f" . meow-find)              ; 含搜索字符
   '("T" . meow-till)              ; 不含搜索字符
   '("m" . point-to-register)
   ;; 常规选择
   '("%" . gwp::match-paren)
   '("*" . meow-mark-symbol)
   '("s" . meow-inner-of-thing)
   '("S" . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("P" . gwp::meow-sync-with-secondary)  ; 用二级选区内容替换当前选中内容
   '("X" . meow-swap-grab)                 ; 将当前选区内容和二级选区内容互换
   '("U" . meow-pop-selection)             ; 撤销前一步选择
   '("v" . meow-cancel-selection)          ; 仿 vi
   '("V" . meow-block)                     ; 逐级扩选, 按 U 回退, 可替代 expand-region
   '("t" . meow-to-block)                  ; 向前选择括号对所在区域, 可重复操作, 按 U 回退
   '("G" . meow-grab)                      ; 相当于 vi 中的 visual mode
   '("C-v" . meow-grab)                    ; 也可按 Alt-mouse 来选择
   ;; 特殊功能
   ;; 2022-11-16: q 更容易误按
   ;; '("q" . meow-quit)               ; 退出window 或 buffer
   '("q" . quit-window)                ; 退出window 或 buffer
   ;; '("F" . zoom)                       ; 将窗口调至最适大小
   '("Q" . unbury-buffer)              ; 还原退出的 buffer
   ;; '("`" . meow-last-buffer)        ; 快速切换 buffer, 其它模式下可按 SPC-`
   '(";" . meow-comment)            ; 相当于 M-;
   '("]" . sp-unwrap-sexp)
   '("R" . sp-unwrap-sexp)          ; 比] 容易按一些
   '(":" . meow-M-x)                ; 可能用 macro 更方便些?
   '("$" . ispell-word)
   '("=" . count-words-region)      ; 默认为 M-=
   '("z" . repeat)                  ; 重复上一个命令; 默认为 C-x z
   '("`" . gwp::avy-meow-find)      ; 使用 avy 来跳转到搜索项
   '("Z" . repeat-complex-command)  ; 重复上一个需要 minibuffer 输入的命令
   ;; TODO
   ;; "F"
   ;; "W"
   ;; "Y"
   ;; "["
   ;; "|"
   ;; "'"
   ;; "~"
   ;; "!"
   ;; "@"
   ;; "&"
   ;; "+"
   ;; "\\"
   ;; "\""
   )

  ;; 当无选区时执行的功能
  (setq
   meow-selection-command-fallback
   '(
     (meow-cancel . keyboard-quit)
     (meow-reverse . meow-open-below)
     (meow-kill . meow-keypad-start)    ; for C-x
     (meow-change . meow-keypad-start)  ; for C-c
     (meow-save . gwp::copy-current-line)
     (meow-beacon-change . meow-beacon-change-char)
     (meow-cancel-selection . meow-right-expand) ; 仿vi, 取消选择或扩选
     (meow-pop-selection . meow-pop-grab) ; 取消二级选区(类 Alt-mouse 选择)
     ;; (meow-replace . meow-yank) ; 似乎没什么必要设置
     ;; (meow-beacon-change . meow-pop-grab)
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
   '("," . "s-,")
   '("g" . "s-g")
   '("w" . "s-w")
   )
  ;; 可在这里统一定义, 但似乎不是最好的方式
  ;; (bind-key "*" 'meow-mark-symbol meow-motion-state-keymap)
  )
;; 60483a2d ends here

;; [[file:../gwp-scratch.note::37628911][37628911]]
(defun meow/setup-insert ()
  (gwp::text-edit-def "C-v" #'gwp::yank-dwim)
  ;; (gwp::text-edit-def "C-g" #'meow-insert-exit)
  )
(define-key meow-insert-state-keymap (kbd "C-g") #'meow-insert-exit)
;; 37628911 ends here

;; [[file:../gwp-scratch.note::9a723a5b][9a723a5b]]
(use-package meow
  :demand t
  :init
  (meow-global-mode 1)
  :custom
  ;; 修改选中内容后退出 insert 模式时不要再选中该区域
  (meow-select-on-change nil)
  ;; 扩选指示字符显示延时
  (meow-expand-hint-remove-delay 5.0)
  ;; 默认在 org 中不显示扩选指示字符
  (meow-expand-exclude-mode-list nil)
  ;; 修正 clipboard 行为
  (meow-use-clipboard t)
  ;; 选择文字区域时光标位置与 vim 及其它软件一致
  ;; 2022-03-19: 会出一些怪问题, 禁用
  ;; (meow-cursor-type-normal 'hbar)
  ;; (meow-use-cursor-position-hack t)
  :config
  (setq meow-cursor-type-normal '(box . 2))
  (setq meow-cursor-type-insert '(bar . 2))
  ;; (setq delete-active-region t)
  ;; (setq meow-cursor-type-region-cursor 'bar)
  ;; https://github.com/meow-edit/meow/discussions/87
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow/setup-normal)
  (meow/setup-leader)
  (meow/setup-motion)
  (meow/setup-insert)
  (meow/setup-states)
  ;; If you want relative line number in NORMAL state(for display-line-numbers-mode)
  (meow-setup-line-number)
  ;; If you need setup indicator, see `meow-indicator' for customizing  hand.
  (meow-setup-indicator))
;; 9a723a5b ends here

;; [[file:../gwp-scratch.note::0e5a2fde][0e5a2fde]]
(provide 'init-meow)
;; 0e5a2fde ends here
