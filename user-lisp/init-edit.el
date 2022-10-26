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
(defun gwp::switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

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
 "bd" '(kill-current-buffer :which-key "kill buffer")
 "bk" '(kill-current-buffer :which-key "kill buffer")
 "br" '(revert-buffer :which-key "revert buffer")
 "bn" '(next-buffer :which-key "next buffer")
 "bp" '(previous-buffer :which-key "previous buffer")
 "bm" '(bookmark-set :which-key "set bookmark")
 "bR" '(crux-rename-buffer-and-file :which-key "rename buffer file")
 )
;; 33105bcf ends here

;; [[file:../gwp-scratch.note::f220a2a2][f220a2a2]]
(gwp::leader-def
 "s" '(:ignore t :which-key "search")
 "ss" '(isearch-forward :which-key "search buffer")
 "si" '(imenu :which-key "jump to symbol(imenu)")
 ;; gwp::rg 会虚拟命令, 待 consult remap
 "sd" '(gwp::rg :which-key "search directory")
 )
;; f220a2a2 ends here

;; [[file:../gwp-scratch.note::e13c7903][e13c7903]]
(defun spacemacs/open-in-external-app (file-path)
  "Open `file-path' in external application."
  (let ((process-connection-type nil))
    (start-process "" nil "xdg-open" file-path)))

(defun spacemacs/open-file-or-directory-in-external-app (arg)
  "Open current file in external application.
If the universal prefix argument is used then open the folder
containing the current file by the default explorer.
If two universal prefix arguments are used, then prompt for command to use."
  (interactive "P")
  (if (equal arg '(4))                  ; C-u
      (spacemacs/open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (if (equal arg '(16))         ; C-u C-u
              (progn
                (let ((program (read-shell-command "Open current file with: ")))
                  (call-process program nil 0 nil file-path)))
            (spacemacs/open-in-external-app file-path))
        ;; for EAF pdf
        (if (derived-mode-p 'eaf-mode)
            (eaf-open-external)
          (message "No file associated to this buffer."))))))
;; e13c7903 ends here

;; [[file:../gwp-scratch.note::45f27ad1][45f27ad1]]
(gwp::leader-def
 "f" '(:ignore t :which-key "file")
 "ff" '(find-file :which-key "find files")
 "fr" '(recentf-open-files :which-key "recent files")
 "fs" '(write-file :which-key "save file as")
 "fj" '(dired-jump :which-key "jump to dired buffer")
 "fJ" '(dired-jump-other-window :which-key "jump to dired buffer (other window)")
 "fb" '(bookmark-jump :which-key "open bookmarks")
 "fo" '(spacemacs/open-file-or-directory-in-external-app :which-key "open externally")
 )
;; 45f27ad1 ends here

;; [[file:../gwp-scratch.note::860eb4b2][860eb4b2]]
(gwp::leader-def
 "q" '(:ignore t :which-key "quit/session")
 "qq" '(save-buffers-kill-terminal :which-key "Quit Emacs")
 "qk" '(save-buffers-kill-emacs :which-key "Kill Emacs (and daemon)")
 )
;; 860eb4b2 ends here

;; [[file:../gwp-scratch.note::92af756a][92af756a]]
(gwp::leader-def
 "j" '(:ignore t :which-key "jump")
 "jo" '(gwp::org-babel-tangle-jump-to-org :which-key "jump to org src file")
 "jm" '(gwp::hydra-mark-ring-pop/body :which-key "emacs mark ring")
 )
;; 92af756a ends here

;; [[file:../gwp-scratch.note::826282dd][826282dd]]
(defun gwp/open-in-gnome-terminal (the-directory)
  "Open `the-directory' in external gnome-terminal."
  (let ((process-connection-type nil))
    ;; (start-process "" nil "terminal-dwim.sh" (concat "--working-directory=" the-directory) "-e" "tmux")
    (start-process "" nil "alacritty" (concat "--working-directory=" the-directory) "-e" "tmux")
    ))

(defun gwp::open-terminal-here ()
  "Open the current dir in a new terminal window"
  (interactive)
  (let ((default-directory (or (and (eq major-mode 'dired-mode)
                                    (dired-current-directory))
                               default-directory)))
    (gwp/open-in-gnome-terminal (expand-file-name default-directory))))

(gwp::leader-def
 "o" '(:ignore t :which-key "open")
 "ot" '(gwp::open-terminal-here :which-key "open terminal here")
 )
;; 826282dd ends here

;; [[file:../gwp-scratch.note::574271f2][574271f2]]
(gwp::leader-def
 "r" '(:ignore t :which-key "resume/ring")
 "rb" '(bookmark-jump :which-key "bookmarks")
 "rm" '(gwp::mark-ring :which-key "mark rings")
 ;; consult 里没有匹配的函数
 "rl" '(ivy-resume :which-key "resume last search")
 "rj" '(list-registers :which-key "resume registers")
  )
;; 574271f2 ends here

;; [[file:../gwp-scratch.note::e724170b][e724170b]]
(require 'recentf)
(defun gwp::zoxide-recent-directories ()
  (let* ((output (shell-command-to-string "zoxide query --list"))
         (dirs (split-string output "[\r\n]+" t)))
    dirs))

(defun gwp::dired-recent-directories ()
  (let* ((recent-dirs
          (mapcar (lambda (file)
                    (if (file-directory-p file) file (file-name-directory file)))
                  recentf-list)))
    recent-dirs))

(defun gwp::zoxide-add-directory (dir)
  "将 dir 加入 zoxide 数据库中"
  (message "add %s" dir)
  (when dir (call-process "zoxide" nil nil nil "add" dir)))

;; open recent directory
;; borrows from http://stackoverflow.com/questions/23328037/in-emacs-how-to-maintain-a-list-of-recent-directories
;;;###autoload
(defun gwp::recent-dirs ()
  "Present a list of recently used directories and open the selected one in dired"
  (interactive)
  (let* ((recent-dirs (delete-dups
		       (append (gwp::zoxide-recent-directories) (gwp::dired-recent-directories))))
	 ;; do not sort candidates
	 (vertico-sort-function nil)
	 (default-directory (completing-read "Directory: " recent-dirs nil t)))
    (gwp::zoxide-add-directory default-directory)
    (dired-jump)))

(gwp::leader-def
  "d" '(:ignore t :which-key "develop")
  "dr" '(gwp::recent-dirs :which-key "recent dirs")
  "dl" '(comment-dwim :which-key "comment/uncomment lines")
  "dh" '(:keymap gwp::symbol-overlay-map :package symbol-overlay :which-key "highlight symbol")
  )
;; e724170b ends here

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

;; [[file:../gwp-scratch.note::51e6b46e][51e6b46e]]
(general-define-key :prefix-map 'gwp::note-map)
(gwp::leader-def "n" '(:keymap gwp::note-map :which-key "note" :package emacs))
;; 51e6b46e ends here

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'init-edit)
;; provide:1 ends here
