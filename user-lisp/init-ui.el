;; [[file:../gwp.note::2f79944b][2f79944b]]
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

;; [[file:../gwp.note::91a3ef0e][91a3ef0e]]
(when (display-graphic-p)
  (setq user-font
        (cond
         ((find-font (font-spec :name  "Sarasa Fixed SC")) "Sarasa Fixed SC")
         ((find-font (font-spec :name  "Iosevka")) "Iosevka")
         ((find-font (font-spec :name  "Inconsolata Nerd Font")) "Inconsolata Nerd Font")
         ((find-font (font-spec :name  "Ubuntu Mono")) "Ubuntu Mono")))
  (setq resolution-factor 2)
  (setq ideal-font-size (eval (* 15 resolution-factor)))
  (setq big-font-size (eval (* 18 resolution-factor)))

  (setq default-font-height 100)
  (set-face-attribute 'default nil :font user-font :height default-font-height)
  ;; Set the fixed pitch face
  (set-face-attribute 'fixed-pitch nil :font user-font :height default-font-height)
  ;; Set the variable pitch face
  (set-face-attribute 'variable-pitch nil :font user-font :height default-font-height :weight 'regular))
;; 91a3ef0e ends here

;; [[file:../gwp.note::5a1d21e9][5a1d21e9]]
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
  ;; 在输入且有码上屏的状态下, 可用TAB临时切换英文.
  ;; (map! :map rime-active-mode-map :after ivy [tab] 'rime-inline-ascii)
  ;; NOTE: 以下有时会让emacs crash
  ;; (setq rime-posframe-properties
  ;;       (list :background-color "#333333"
  ;;             :foreground-color "#dcdccc"
  ;;             :font "WenQuanYi Micro Hei Mono-14"
  ;;             :internal-border-width 10))
  ;; (setq default-input-method "rime"
  ;;       rime-show-candidate 'posframe)
  )
;; 这里需要与fcitx配合: 去掉GTK_IM_MODULE, XMODIFIERS等FCITX输入法设置变量.
;; (map! :nieg "C-SPC" 'toggle-input-method)
;; NOTE: 因为与ivy的默认绑定有冲突, minibuffer下不能切换
;; ivy-call-and-recenter
;; 2021-10-13: 直接map不太有效, 时灵不灵的
;; (map! :map ivy-minibuffer-map "C-SPC" #'toggle-input-method)
;; NOTE: 可用M-RET来预览选中条目, 而不退出ivy窗口
;; (map! :after ivy :map ivy-minibuffer-map [remap ivy-call-and-recenter] 'toggle-input-method)
;; 5a1d21e9 ends here

;; [[file:../gwp.note::3d5eeec1][3d5eeec1]]
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

;; [[file:../gwp.note::*provide][provide:1]]
(provide 'init-ui)
;; provide:1 ends here
