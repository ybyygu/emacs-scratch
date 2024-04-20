;; -*- lexical-binding: t; -*-

;; [[file:../gwp-scratch.note::0894dac2][0894dac2]]
(defconst gwp::dwim-leader-key "s-w")
(defconst gwp::goto-leader-key "s-g")
(defconst gwp::local-leader-key "s-,")
;; 0894dac2 ends here

;; [[file:../gwp-scratch.note::25771477][25771477]]
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
;; 25771477 ends here

;; [[file:../gwp-scratch.note::edaec1b8][edaec1b8]]
(general-define-key :prefix-map 'gwp::edit-map)

(general-define-key :prefix-map 'gwp::develop-map)

;; (general-define-key :prefix-map 'gwp::open-map)

(general-define-key :prefix-map 'gwp::magit-map)

(general-define-key :prefix-map 'gwp::buffer-map)
;; edaec1b8 ends here

;; [[file:../gwp-scratch.note::afede203][afede203]]
(provide 'init-general)
;; afede203 ends here
