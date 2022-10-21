;; [[file:../gwp-scratch.note::2f79944b][2f79944b]]
(setq inhibit-startup-message t)
;; 禁用不必要的界面元素
(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(menu-bar-mode -1)            ; Disable the menu bar

;; 默认会bee bee
(setq visible-bell t)

;; (tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(use-package doom-modeline
  :ensure t
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package doom-themes
  :init (load-theme 'doom-nova t))
;; 2f79944b ends here

;; [[file:../gwp-scratch.note::91a3ef0e][91a3ef0e]]
(when (display-graphic-p)
  (setq user-font
        (cond
         ((find-font (font-spec :name  "Sarasa Fixed SC")) "Sarasa Fixed SC")
         ((find-font (font-spec :name  "Iosevka")) "Iosevka")
         ((find-font (font-spec :name  "Inconsolata Nerd Font")) "Inconsolata Nerd Font")
         ((find-font (font-spec :name  "Ubuntu Mono")) "Ubuntu Mono")))

  (setq default-font-height 110)
  ;; (setq resolution-factor 2)
  ;; (setq ideal-font-size (eval (* 15 resolution-factor)))
  ;; (setq big-font-size (eval (* 18 resolution-factor)))
  (set-face-attribute 'default nil :font user-font :height default-font-height)
  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font user-font :height default-font-height)
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font user-font :height default-font-height :weight 'regular))
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
  :custom
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

;; [[file:../gwp-scratch.note::3d5eeec1][3d5eeec1]]
;; symbol-overlay
;;;  a highlight-symbol replacement.
(use-package symbol-overlay
  :config
  ;; 用 transient 不如下面的好. 下面的可以用"."命令来重做上次的操作.
  (general-define-key :prefix-map 'gwp::symbol-overlay-map
                      "h" 'symbol-overlay-put
                      "r" 'symbol-overlay-rename
                      "t" 'symbol-overlay-toggle-in-scope
                      "n" 'symbol-overlay-switch-forward ; 当在高亮的字符外时, 可快速返回.
                      "p" 'symbol-overlay-switch-backward
                      "d" 'symbol-overlay-remove-all
                      "R" 'symbol-overlay-query-replace)
  ;; 等价设置; 备忘
  ;; (setq symbol-overlay-map (make-sparse-keymap))
  ;; (setq gwp::symbol-overlay-map (make-sparse-keymap))
  ;; (define-key gwp::symbol-overlay-map (kbd "h") 'symbol-overlay-put)
  ;; (define-key gwp::symbol-overlay-map (kbd "n") 'symbol-overlay-jump-next)
  ;; (define-key gwp::symbol-overlay-map (kbd "p") 'symbol-overlay-jump-prev)
  ;; (define-key gwp::symbol-overlay-map (kbd "w") 'symbol-overlay-save-symbol)
  ;; (define-key gwp::symbol-overlay-map (kbd "t") 'symbol-overlay-toggle-in-scope)
  ;; (define-key gwp::symbol-overlay-map (kbd "e") 'symbol-overlay-echo-mark)
  ;; (define-key gwp::symbol-overlay-map (kbd "d") 'symbol-overlay-jump-to-definition)
  ;; (define-key gwp::symbol-overlay-map (kbd "s") 'symbol-overlay-isearch-literally)
  ;; (define-key gwp::symbol-overlay-map (kbd "q") 'symbol-overlay-query-replace)
  ;; (define-key gwp::symbol-overlay-map (kbd "r") 'symbol-overlay-rename)
  )
;; 3d5eeec1 ends here

;; [[file:../gwp-scratch.note::2b13453c][2b13453c]]
(use-package helpful
  :demand t
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))
;; 2b13453c ends here

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

(use-package avy
  :config
  (setq avy-all-windows t))
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

;; [[file:../gwp-scratch.note::bf66c13f][bf66c13f]]
(require 'ivy)

(defvar gwp::ivy-buffer-actions
  '(("j" switch-to-buffer-other-window "other window")
    ("x" counsel-open-buffer-file-externally "open externally")
    ("k" ivy--kill-buffer-action "kill")
    ("r" ivy--rename-buffer-action "rename")
    ("t" switch-to-buffer-other-tab "other tab")     ; 默认没有
    ("f" switch-to-buffer-other-frame "other frame") ; 默认没有
    )
  "Default ivy actions for files.")
(ivy-set-actions 'ivy-switch-buffer gwp::ivy-buffer-actions)
;; bf66c13f ends here

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

;; [[file:../gwp-scratch.note::44d5ec48][44d5ec48]]
;; keymaps for leader key
(use-package winner
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

;; [[file:../gwp-scratch.note::bf455395][bf455395]]
(use-package ivy-hydra)

(use-package swiper
  :after ivy
  :bind
  (
   ;; ("C-s"   . swiper-isearch)
   ;; ("C-r"   . swiper-isearch-backward)
   ;; ("C-c v p" . ivy-push-view)
   ;; ("C-c v o" . ivy-pop-view)
   ;; ("C-c v ." . ivy-switch-view)
   :map swiper-map
   ("M-s" . swiper-isearch-toggle)
   :map isearch-mode-map
   ("M-s" . swiper-isearch-toggle)
   :map ctl-x-4-map
   ("C-s" . ivy-push-view)
   ))
;; bf455395 ends here

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'init-ui)
;; provide:1 ends here
