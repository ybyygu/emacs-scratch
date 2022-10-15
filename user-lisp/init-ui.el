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
  ;; (setq resolution-factor 2)
  ;; (setq ideal-font-size (eval (* 15 resolution-factor)))

  (setq default-font-height 100)
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

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'init-ui)
;; provide:1 ends here
