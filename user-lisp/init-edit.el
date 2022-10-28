;; [[file:../gwp-scratch.note::1d60bcbf][1d60bcbf]]
;; -*- lexical-binding: t; -*-
;; 1d60bcbf ends here

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

;; [[file:../gwp-scratch.note::edaec1b8][edaec1b8]]
(general-define-key
 :prefix-map 'gwp::edit-map
 )

(general-define-key
 :prefix-map 'gwp::develop-map
 )
;; edaec1b8 ends here

;; [[file:../gwp-scratch.note::4e63ecbf][4e63ecbf]]
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
   '("=" . meow-goto-line)
   ;; '("z" . avy-goto-char-in-line)
   '("z" . meow-pop-selection)
   '("'" . repeat-complex-command)      ; 重复上一个需要 minibuffer 输入的命令
   '("Z" . repeat)                      ; 重复上一个命令
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

;; [[file:../gwp-scratch.note::37628911][37628911]]
(defun meow/setup-insert ()
  (gwp::text-edit-def "C-v" #'gwp::yank-dwim))
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
  ;; 选择文字区域时光标位置与 vim 及其它软件一致
  ;; 2022-03-19: 会出一些怪问题, 禁用
  ;; (meow-cursor-type-normal 'hbar)
  (meow-use-cursor-position-hack t)
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
  ;; If you want relative line number in NORMAL state(for display-line-numbers-mode)
  (meow-setup-line-number)
  ;; If you need setup indicator, see `meow-indicator' for customizing  hand.
  (meow-setup-indicator))
;; 9a723a5b ends here

;; [[file:../gwp-scratch.note::0e5a2fde][0e5a2fde]]
(provide 'init-edit)
;; 0e5a2fde ends here
