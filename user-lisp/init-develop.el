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
;; 8970c514 ends here

;; [[file:../gwp-scratch.note::a267f2ee][a267f2ee]]
(use-package rust-mode)
(use-package cargo)

(eval-after-load 'rust-mode
  '(require 'smartparens-rust))
;; a267f2ee ends here

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'init-develop)
;; provide:1 ends here
