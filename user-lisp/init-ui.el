;; [[file:../gwp-scratch.note::b0fdf59c][b0fdf59c]]
;; -*- lexical-binding: t; -*-
;; b0fdf59c ends here

;; [[file:../gwp-scratch.note::2f79944b][2f79944b]]
(setq inhibit-startup-message t)

;; 服务器上使用编译 --without-x 选项
(unless init-no-x-flag
  ;; Give some breathing room
  (set-fringe-mode 10)
  ;; Disable visible scrollbar
  (scroll-bar-mode -1))

(tool-bar-mode -1)          ; Disable the toolbar
(menu-bar-mode -1)          ; Disable the menu bar

;; 默认会bee bee
(setq visible-bell t)

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(setq-default line-spacing 0.1)
;; 2f79944b ends here

;; [[file:../gwp-scratch.note::62b5d5bd][62b5d5bd]]
(use-package doom-themes
  :init (load-theme 'doom-nova t)
  :config
  )

(custom-set-faces
 '(org-todo ((t (:background "#263238" :foreground "yellow" :weight bold))))
 '(org-done ((t (:foreground "yellow" :weight bold :background "#263238"))))
 '(org-table ((t (:foreground "#e3f2fd"))))
 '(org-level-1 ((t (:foreground "#e3f2fd" :height 1.1 :background nil :weight normal :box nil))))
 '(org-level-2 ((t (:foreground "#e3f2fd" :height 1.0 :background nil :weight normal :box nil))))
 '(org-headline-done ((t (:foreground "gray" :weight normal))))
 )

;; terminal 下不处理更好
(unless init-no-x-flag
  (custom-set-faces
   '(region ((t (:background "#555555"))))
   ;; meow-grab 时, 配色更清楚些
   '(secondary-selection ((t (:foreground "green"))))
   ))
;; 62b5d5bd ends here

;; [[file:../gwp-scratch.note::91a3ef0e][91a3ef0e]]
;; credit: https://oracleyue.github.io/post/emacs-setup-md/#load-different-themes-for-emacs-serversdaemons
;; Init or reload functions
(defun gwp::init-ui (&optional frame)
  (when (display-graphic-p)
    (setq user-font
          (cond
           ((find-font (font-spec :name  "Sarasa Term SC")) "Sarasa Term SC")
           ;; ((find-font (font-spec :name  "Maple Mono SC NF")) "Maple Mono SC NF")
           ((find-font (font-spec :name  "Iosevka")) "Iosevka")
           ((find-font (font-spec :name  "Inconsolata Nerd Font")) "Inconsolata Nerd Font")
           ((find-font (font-spec :name  "Ubuntu Mono")) "Ubuntu Mono")))

    (setq default-font-height 110)
    (set-face-attribute 'default nil :font user-font :height default-font-height)
    ;; Set the fixed pitch face
    (set-face-attribute 'fixed-pitch nil :font user-font :height default-font-height)
    ;; Set the variable pitch face
    (set-face-attribute 'variable-pitch nil :font user-font :height default-font-height :weight 'regular)

    ;; (set-fontset-font t 'cjk-misc user-font nil 'prepend)
    ;; (set-fontset-font t 'han user-font nil 'prepend)
    ;; (set-fontset-font t 'emoji user-font nil 'prepend)
    ;; (set-fontset-font t 'symbol user-font nil 'prepend)
    ;; (set-fontset-font t 'latin user-font nil 'prepend)
    (dolist (charset '(han cjk-misc kana symbol bopomofo))
      (set-fontset-font (frame-parameter nil 'font)
                        charset
                        (font-spec :family user-font)))
    (set-fontset-font t 'emoji (font-spec :family "Noto Color Emoji") nil 'prepend)
    ))

;; server 模式下字体需要特殊处理
(defun gwp::reload-ui-in-daemon (frame)
  "Reload the theme (and font) in an daemon frame."
  (when (or (daemonp) (not (display-graphic-p)))
    (with-selected-frame frame
      (run-with-timer 0.1 nil #'gwp::init-ui))))

;; Load the theme and fonts
(unless init-no-x-flag
  (if (daemonp)
      (add-hook 'after-make-frame-functions #'gwp::reload-ui-in-daemon)
    (gwp::init-ui)))
;; 91a3ef0e ends here

;; [[file:../gwp-scratch.note::5a1d21e9][5a1d21e9]]
;; https://github.com/jadestrong/dotfiles/blob/master/home/.doom.d/modules/input/chinese2/config.el
(defun gwp::rime-convert-string-at-point (&optional return-cregexp)
  "将光标前的字符串转换为中文."
  (interactive "P")
  (let ((string
         (if mark-active
             (buffer-substring-no-properties
              (region-beginning) (region-end))
           (buffer-substring-no-properties
            (point) (max (line-beginning-position) (- (point) 80)))))
        code
        length)
    (cond ((string-match "\\([a-z]+\\) *$" string)
           (setq code (match-string 0 string))
           (setq length (length code))
           (setq code (replace-regexp-in-string " +" "" code))
           (if mark-active
               (delete-region (region-beginning) (region-end))
             (when (> length 0)
               (delete-char (- 0 length))))
           (when (> length 0)
             (setq unread-command-events
                   (append (listify-key-sequence code)
                           unread-command-events))))
          (t (message "`rime-convert-string-at-point' did nothing.")))))

(defun gwp::rime-toggle-input ()
  "切换 rime 中文输入状态."
  (interactive)

  (let ((input-method "rime"))
    (toggle-input-method)
    ;; evil 下, 直接进入 insert 模式
    (when (rime-predicate-evil-mode-p)
      (if (= (+ 1 (point)) (line-end-position))
          (evil-append 1)
        (evil-insert 1)))

    ;; 进入 rime 输入状态后, 把误按的字符转换中文
    (when (meow-insert-mode-p)
      (when (string= current-input-method input-method)
        (gwp::rime-convert-string-at-point))
      )

    ;; 提示当前输入状态, 比看图标更醒目
    (if current-input-method
        (message "IME on")
      (message "IME off"))))

(use-package rime
  :unless init-no-x-flag
  :custom
  ;; terminal 下无需使用
  (default-input-method "rime")
  :config
  (setq rime-user-data-dir "~/.local/share/fcitx5/rime")
  ;;; support shift-l, shift-r, control-l, control-r
  (setq rime-inline-ascii-trigger 'shift-l)
  ;; 临时英文中阻止标点直接上屏
  (setq rime-inline-ascii-holder ?x)      ; Any single character that not trigger auto commit
  ;; 添加C-.快捷键, 方便切换中英文标点(需要在rime输入时有效)
  (setq rime-translate-keybindings
        '("C-f" "C-b" "C-n" "C-p" "C-g" "C-."))

  ;; NOTE: 以下有时会让emacs crash
  ;; (setq rime-posframe-properties
  ;;       (list :background-color "#333333"
  ;;             :foreground-color "#dcdccc"
  ;;             :internal-border-width 10))
  (setq rime-show-candidate 'posframe)

  ;; 自动进入英文录入状态, 相当于直接输入英文
  (setq rime-disable-predicates
        '(
          meow-normal-mode-p
          meow-motion-mode-p
          meow-beacon-mode-p
          ;; 首字母为是英文字母时进入英文模式
          rime-predicate-after-alphabet-char-p
          ;; 将要输入的为大写字母时
          rime-predicate-current-uppercase-letter-p
          ;; 在 prog-mode 和 conf-mode 中除了注释和引号内字符串之外的区域
          ;; rime-predicate-prog-in-code-p
          ;; 在 (La)TeX 数学环境中或者输入 (La)TeX 命令时
          rime-predicate-tex-math-or-command-p
          ;; 在中文字符且有空格之后
          rime-predicate-space-after-cc-p
          ))
  ;; 进入连续英文状态, 空格或回车键上屏
  (setq rime-inline-predicates
        '(
          rime-predicate-space-after-cc-p
          ))
  :bind
  ;; 这里需要与fcitx配合: 去掉GTK_IM_MODULE, XMODIFIERS等FCITX输入法设置变量.
  (("C-SPC" . gwp::rime-toggle-input)
   ;; 在输入且有码上屏的状态下, 可用TAB临时切换英文.
   ;; (map! :map rime-active-mode-map :after ivy [tab] 'rime-inline-ascii)
   :map rime-active-mode-map
   ([tab] . rime-inline-ascii)
   ))
;; 5a1d21e9 ends here

;; [[file:../gwp-scratch.note::6ca20167][6ca20167]]
(use-package hl-line
  :ensure nil
  :config
  ;; terminal 下显示很不同
  (unless init-no-x-flag
    (global-hl-line-mode 1)
    (set-face-background 'hl-line "#37474f")))
;; 6ca20167 ends here

;; [[file:../gwp-scratch.note::3d5eeec1][3d5eeec1]]
(use-package hl-todo
  :hook (prog-mode . hl-todo-mode))
;; 3d5eeec1 ends here

;; [[file:../gwp-scratch.note::2b13453c][2b13453c]]
(use-package helpful
  :demand t
  ;; :after meow
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :init
  ;; 解决 meow-describe-key 的问题
  ;; 2022-11-03: 二者调用接口不兼容, 放弃
  ;; (advice-add 'describe-key :override #'helpful-key)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key)
  :config
  ;; 解决 meow-describe-key 导致的问题
  (bind-key "C-h k" #'helpful-key)
  )
;; 2b13453c ends here

;; [[file:../gwp-scratch.note::4cd1143d][4cd1143d]]
(bind-key "n" 'help-go-forward helpful-mode-map)
(bind-key "p" 'help-go-back helpful-mode-map)
(bind-key "j" 'next-line helpful-mode-map)
(bind-key "k" 'previous-line helpful-mode-map)
(bind-key "h" 'backward-char helpful-mode-map)
(bind-key "l" 'forward-char helpful-mode-map)
(bind-key "*" 'meow-mark-symbol helpful-mode-map)
;; 4cd1143d ends here

;; [[file:../gwp-scratch.note::885c9fa9][885c9fa9]]
;; 方便绑定到 SPC-t-l
;;
;; 仅切换relative和none两种状态, doom的要切三种
(defun gwp::toggle-line-numbers ()
  (interactive)
  (if display-line-numbers
      (setq display-line-numbers 'nil)
    (setq display-line-numbers t)))

(defun gwp::display-line-numbers ()
  (setq display-line-numbers 'relative)
  (setq display-line-numbers t))

;; NOTE: org-mode在折叠状态下, 相对行号显示的是实际数目, 而非折叠后的, 这对编辑操作没多大帮助了.
;; ;; (add-hook 'org-mode-hook #'gwp::display-line-numbers)
(add-hook 'org-src-mode-hook #'gwp::display-line-numbers)
;; (add-hook 'prog-mode-hook #'gwp::display-line-numbers)
(add-hook 'rust-mode-hook #'gwp::display-line-numbers)

;; 全局设置
(setq display-line-numbers-type 'relative)
;; 885c9fa9 ends here

;; [[file:../gwp-scratch.note::34bcfc6f][34bcfc6f]]
(use-package ace-window
  :custom
  ;; 仅当多于两个窗口时才提示选择
  (aw-scope 'frame)
  (aw-dispatch-always nil)
  (aw-dispatch-when-more-than 2)
  (aw-ignore-current t)
  ;; Set window selection keys to the home row ones.
  (aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

;;;###autoload
(defun gwp::ace-select-window ()
  "选取某窗口放入当前窗口位置"
  (interactive)
  (call-interactively #'ace-swap-window)
  (call-interactively #'aw-flip-window)
  )
;; 34bcfc6f ends here

;; [[file:../gwp-scratch.note::a207c706][a207c706]]
(use-package burly)

(general-define-key
 :prefix-map 'gwp::window-map
 "S" #'burly-bookmark-windows
 "C-s" #'burly-bookmark-windows
 )
;; a207c706 ends here

;; [[file:../gwp-scratch.note::19e08aef][19e08aef]]
(defun gwp::display-current-buffer-other-frame ()
  "在其它 frame 中显式当前 buffer"
  (interactive)
  (switch-to-buffer-other-frame (current-buffer)))
;; 19e08aef ends here

;; [[file:../gwp-scratch.note::bfacbb8e][bfacbb8e]]
(use-package golden-ratio)
;; bfacbb8e ends here

;; [[file:../gwp-scratch.note::9a32eb12][9a32eb12]]
;; 新建frame时最大化窗口
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; 切换窗口最大化状态
;; ;; from https://gist.github.com/3402786
;; (defun gwp::toggle-maximize-window ()
;;   (interactive)
;;   (save-excursion
;;     (if (and (= 1 (length (window-list)))
;;              (assoc ?_ register-alist))
;;         (jump-to-register ?_)
;;       (progn
;;         (window-configuration-to-register ?_)
;;         (delete-other-windows)))))

;;;###autoload
(defun gwp::toggle-maximize-window ()
  "仅显示当前窗口?"
  (interactive)
  ;; (if (= 1 (length (window-list)))
  ;;     (winner-undo)
  ;;   (delete-other-windows))
  (if (and winner-mode
           (equal (selected-window) (next-window)))
      (winner-undo)
    (delete-other-windows)))

(defun gwp::maximize-window-vertically ()
  "纵向仅显示当前窗口"
  (interactive)
  (require 'windmove)
  (let* ((w1 (window-in-direction 'below))
         (w2 (window-in-direction 'above)))
    (cond ((or w1 w2)
           (save-excursion
             (while (ignore-errors (windmove-up)) (delete-window))
             (while (ignore-errors (windmove-down)) (delete-window))))
          ((and (not w1) (not w2))
           ;; 手动 undo 更合适些
           ;; (winner-undo)
           ))))

;; https://github.com/purcell/emacs.d/blob/master/lisp/init-windows.el
(defun sanityinc/split-window()
  "Split the window to see the most recent buffer in the other window.
Call a second time to restore the original window configuration."
  (interactive)
  (if (eq last-command 'sanityinc/split-window)
      (progn
        (jump-to-register :sanityinc/split-window)
        (setq this-command 'sanityinc/unsplit-window))
    (window-configuration-to-register :sanityinc/split-window)
    (switch-to-buffer-other-window nil)
    ))

(general-define-key
 :prefix-map 'gwp::window-map
 "z"       #'golden-ratio
 "g"       #'golden-ratio
 "1"       #'gwp::toggle-maximize-window
 "o"       #'gwp::toggle-maximize-window     ; show "only"
 "t"       #'gwp::maximize-window-vertically ; show top
 "`"       #'sanityinc/split-window
 )
;; 9a32eb12 ends here

;; [[file:../gwp-scratch.note::1429fad5][1429fad5]]
(setq split-width-threshold 200)        ; default is 160
;; 1429fad5 ends here

;; [[file:../gwp-scratch.note::f07dc327][f07dc327]]
(defhydra gwp/adjust-window-size ()
  "resize-window"
  ("h" enlarge-window-horizontally "decrease width")
  ("l" shrink-window-horizontally "decrease height")
  ("k" enlarge-window "increase height")
  ("j" shrink-window "increase width")
  ("q" nil "quit")
  )

(general-define-key
 :prefix-map 'gwp::window-map
 "a" #'gwp/adjust-window-size/body ; adjust
 )
;; f07dc327 ends here

;; [[file:../gwp-scratch.note::aaa39215][aaa39215]]
(add-to-list 'display-buffer-alist
             '("\\*compilation\\*"
               display-buffer-in-previous-window))

(add-to-list 'display-buffer-alist
             '("^*Async Shell Command*" . (display-buffer-no-window))
             '("^*Shell Command Output*" . (display-buffer-no-window)))

;; (add-to-list 'display-buffer-alist '("*Apropos*" display-buffer-same-window))
;; (add-to-list 'display-buffer-alist '("*Warning*" display-buffer-at-bottom))

(use-package simple
  :ensure nil
  :custom
  (async-shell-command-buffer 'new-buffer))
;; aaa39215 ends here

;; [[file:../gwp-scratch.note::3c2469e7][3c2469e7]]
;; credit: https://www.emacswiki.org/emacs/ToggleWindowSplit
(defun gwp::toggle-frame-split ()
  "If the frame is split vertically, split it horizontally or vice versa.
Assumes that the frame is only split into two."
  (interactive)
  (unless (= (length (window-list)) 2) (error "Can only toggle a frame split in two"))
  (let ((split-vertically-p (window-combined-p)))
    (delete-window) ; closes current window
    (if split-vertically-p
        (split-window-horizontally)
      (split-window-vertically)) ; gives us a split with the other window twice
    (switch-to-buffer nil))) ; restore the original window in this part of the frame

(global-set-key (kbd "C-x |") 'gwp::toggle-frame-split)
;; 3c2469e7 ends here

;; [[file:../gwp-scratch.note::44d5ec48][44d5ec48]]
;; keymaps for leader key
(use-package winner
  :ensure nil
  :custom
  ;; 去掉 C-c left, C-c right
  (winner-dont-bind-my-keys t))

(winner-mode 1)

(general-define-key
 :prefix-map 'gwp::window-map
 "s" #'split-window-below
 "v" #'split-window-right
 "h" #'windmove-left
 "j" #'windmove-down
 "k" #'windmove-up
 "l" #'windmove-right
 "d" #'delete-window
 "q" #'delete-window
 "=" #'balance-windows
 "u" #'winner-undo            ; 撤销窗口变动
 "w" #'ace-window             ; 替代 SPC-w-w
 "r" #'gwp::ace-select-window ; rotate
 "R" #'ace-swap-window        ; rotate
 "c" #'ace-delete-window      ; close other windows
 "f" #'tear-off-window        ; 类似于firefox中的标签变窗口 (float, move to new frame)
 "F" #'follow-mode            ; 同步滚动窗口, 可用于双窗口内容对比等
 )
;; 44d5ec48 ends here

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'init-ui)
;; provide:1 ends here
