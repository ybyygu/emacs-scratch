;; [[file:../gwp-scratch.note::ab84316d][ab84316d]]
;; -*- lexical-binding: t; -*-
;; ab84316d ends here

;; [[file:../gwp-scratch.note::24325443][24325443]]
(use-package with-editor)

(add-hook 'shell-mode-hook  'with-editor-export-editor)
(add-hook 'eshell-mode-hook 'with-editor-export-editor)
(add-hook 'term-exec-hook   'with-editor-export-editor)
(add-hook 'vterm-mode-hook  'with-editor-export-editor)

;; 进入 insert 编辑模式
(add-hook 'with-editor-mode-hook 'meow-insert-mode)
;; 24325443 ends here

;; [[file:../gwp-scratch.note::8970c514][8970c514]]
(general-define-key :prefix-map 'gwp::magit-map)

(use-package magit
  :demand t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  ;; 隐藏untracked文件列表. 更多时候的操作是stage/commit
  (setq magit-section-initial-visibility-alist (quote ((untracked . hide))))
  ;;禁用magit中的gravatars支持, 响应能快一些.
  (setq magit-revision-show-gravatars nil)
  ;; 进入 magit-status 后, 将光标定在 unstaged 一栏
  (setq magit-status-initial-section '(2))

  :bind
  (:map gwp::magit-map
        ;; ("j" . magit-next-line)
        ;; ("k" . magit-previous-line)
        ("g" . magit-status)
        ("s" . magit-status)
        ("x" . magit-checkout)
        ("c" . magit-commit)
        ("p" . magit-push)
        ("u" . magit-pull)
        ("e" . magit-ediff-resolve)
        ("r" . magit-rebase-interactive)
        :map magit-status-mode-map
        ;; ("j" . magit-next-line)
        ;; ("k" . magit-previous-line)
        :map magit-hunk-section-map
        ;; ("j" . magit-next-line)
        ;; ("k" . magit-previous-line)
        ))


(use-package magit-popup)

;; 显示 src 中的 TODO FIXME 等项
(use-package magit-todos
  :diminish
  :after magit
  :config
  (magit-todos-mode)
  )
;; 8970c514 ends here

;; [[file:../gwp-scratch.note::a267f2ee][a267f2ee]]
(use-package rust-mode)
(use-package cargo)

(eval-after-load 'rust-mode
  '(require 'smartparens-rust))
;; a267f2ee ends here

;; [[file:../gwp-scratch.note::f2289888][f2289888]]
;; 2022-10-28: 不设置的话不能正常处理 elisp 代码(org src block 中)
(use-package format-all
  :demand t
  :bind ("C-c C-f" . format-all-buffer)
  :config
  (setq format-all-default-formatters
        '(("Emacs Lisp" emacs-lisp)
          ("Python" black)
          ("Rust" rustfmt)
          ("Shell" shfmt)
          ("TOML" prettier)
          ("Lua" lua-fmt)
          ("Dockerfile" dockfmt)
          ("CMake" cmake-format)
          ("C" clang-format)
          ("C++" clang-format)
          ("HTML" html-tidy)
          ("JSON" prettier)
          ("YAML" prettier))))
;; f2289888 ends here

;; [[file:../gwp-scratch.note::f8651bde][f8651bde]]
(use-package citre
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  ;; 用 transient 不如下面的好. 下面的可以用"."命令来重做上次的操作.
  :config
  (general-define-key :prefix-map 'gwp::citre-map
                      "j" 'citre-jump
                      "b" 'citre-jump-back
                      "c" 'citre-create-tags-file
                      "p" 'citre-peek))
;; f8651bde ends here

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'init-develop)
;; provide:1 ends here
