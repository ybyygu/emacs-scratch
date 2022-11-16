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

;; [[file:../gwp-scratch.note::81cb1ab5][81cb1ab5]]
(use-package find-file-in-project
  :config
  (setq ffip-use-rust-fd t))

;;;###autoload
(defun gwp::find-file-from-clipboard ()
  "打开 clipboard 中复制的文件路径"
  (interactive)
  (require 'find-file-in-project)
  (let ((path (simpleclip-get-contents)))
    (ffip-find-files path nil)))
;; 81cb1ab5 ends here

;; [[file:../gwp-scratch.note::8970c514][8970c514]]
(use-package magit
  :demand t
  :unless init-no-x-flag
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  ;; 隐藏untracked文件列表. 更多时候的操作是stage/commit
  (setq magit-section-initial-visibility-alist (quote ((untracked . hide))))
  ;;禁用magit中的gravatars支持, 响应能快一些.
  (setq magit-revision-show-gravatars nil)
  ;; 进入 magit-status 后, 将光标定在 unstaged 一栏
  (setq magit-status-initial-section '(2))
  (gwp::local-leader-def
    :keymaps 'dired-mode-map
    "l"      #'(magit-dired-log :which-key "git log")
    )
  (gwp::local-leader-def
    :keymaps 'magit-status-mode-map
    "D" #'magit-file-delete
    "j" #'magit-dired-jump
    "o" #'magit-diff-visit-file-other-window
    "O" #'magit-diff-visit-file-other-frame
    "r" #'magit-file-rename
    "t" #'magit-todos-list
    "f" #'magit-find-file
    )

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
        ("f" . magit-file-dispatch)
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
  ;; 2022-11-01: 会影响 magit 响应速度, 现禁用
  ;; (magit-todos-mode)
  )
;; 8970c514 ends here

;; [[file:../gwp-scratch.note::275df196][275df196]]
(require 'yadm)

(bind-key "." #'yadm-status gwp::magit-map)
(bind-key "." #'yadm-find-file gwp::develop-map)

(gwp::local-leader-def
  :keymaps 'dired-mode-map
  "a" #'yadm-add-file)
;; 275df196 ends here

;; [[file:../gwp-scratch.note::a267f2ee][a267f2ee]]
(use-package rust-mode
  :requires smartparens
  :config
  (require 'smartparens-rust)
  ;; Don't pair lifetime specifiers
  (sp-local-pair 'rust-mode "'" nil :actions nil)
  ;; rust 回车后自动格式化 {|}
  ;; https://emacs.stackexchange.com/questions/2837/automatically-formatting-brackets
  (sp-local-pair 'rust-mode "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
  ;; Rust closure中使用
  (sp-with-modes '(rust-mode)
    (sp-local-pair "|" "|")))

(use-package cargo)

(require 'rust-edit)
(gwp::local-leader-def
  :keymaps 'rust-mode-map
  "e" #'rust-edit-transient
  "b" #'rust-edit-cargo-transient)
;; a267f2ee ends here

;; [[file:../gwp-scratch.note::f2289888][f2289888]]
;; 2022-10-28: 不设置的话不能正常处理 elisp 代码(org src block 中)
(use-package format-all
  :demand t
  :bind ("C-c C-f" . format-all-buffer)
  :custom
  (format-all-default-formatters
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
     ("YAML" prettier)))
  :config
  )
;; f2289888 ends here

;; [[file:../gwp-scratch.note::f8651bde][f8651bde]]
(use-package citre
  :requires transient
  :commands (citre-jump citre-jump-back citre-peak citre-create-tags-file)
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  :config
  (transient-define-prefix gwp::citre-transient ()
    "citre tags"
    ["Jump:"
     ("j" "jump" citre-jump)
     ("b" "jump back" citre-jump-back)
     ("p" "peek" citre-peak)
     ]
    ["Edit"
     ("c" "create tags file" citre-create-tags-file)
     ]
    )
  :bind
  (:map gwp::develop-map
        ("j" . gwp::citre-transient)))
;; f8651bde ends here

;; [[file:../gwp-scratch.note::0deb729c][0deb729c]]
;; symbol-overlay
;;;  a highlight-symbol replacement.
(use-package symbol-overlay
  :requires transient
  :config
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
  ;; 以下命令仅在高亮区域外才用得上
  ;; (add-hook 'symbol-overlay-mode-hook #'org-mark-jump-unhide)
  (advice-add #'symbol-overlay-jump-next :after #'gwp::goto-line-unhide)
  (advice-add #'symbol-overlay-jump-prev :after #'gwp::goto-line-unhide)

  ;; 方便 hjkl 移动
  (unbind-key "h" symbol-overlay-map)
  (bind-key "?" #'symbol-overlay-map-help symbol-overlay-map)

  (transient-define-prefix gwp::symbol-overlay-transient ()
    "citre tags"
    ["View:"
     ("n" "next" symbol-overlay-switch-forward :transient t) ; 当在高亮的字符外时, 可快速返回.
     ("p" "previous" symbol-overlay-switch-backward :transient t)
     ("t" "toggle in scope" symbol-overlay-toggle-in-scope)
     ]
    ["Edit"
     ("h" "highlight" symbol-overlay-put) ; 原位时可用 i
     ("d" "remove all" symbol-overlay-remove-all)
     ("r" "rename" symbol-overlay-rename)
     ("R" "replace" symbol-overlay-query-replace)
     ]
    )
  :bind
  (:map gwp::develop-map
        ("h" . gwp::symbol-overlay-transient)))
;; 0deb729c ends here

;; [[file:../gwp-scratch.note::985a2495][985a2495]]
(gwp::local-leader-def
  :keymaps 'emacs-lisp-mode-map
  "e" #'eval-last-sexp
  "r" #'eval-region
  "b" #'eval-buffer
  "d" #'eval-defun
  )
;; 985a2495 ends here

;; [[file:../gwp-scratch.note::a9baf9f2][a9baf9f2]]
(setq python-indent-guess-indent-offset-verbose nil)
;; a9baf9f2 ends here

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'init-develop)
;; provide:1 ends here
