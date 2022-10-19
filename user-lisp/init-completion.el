;; [[file:../gwp-scratch.note::7db2aa5a][7db2aa5a]]
(use-package yasnippet
  :commands
  (yas-expand yas-minor-mode)
  :init
  (defun entropy/emacs-yas-enable-or-expand (&rest args)
    "Auto enable `yas-global-mode' when not as it and call
`yas-expand'."
    (interactive)
    (require 'yasnippet)
    (cond
     ((not yas-global-mode)
      (yas-global-mode)
      (yas-expand))
     (t
      (yas-expand))))
  :bind
  (:map prog-mode-map
   ("M-i" . entropy/emacs-yas-enable-or-expand)
   :map org-mode-map
   ("M-i" . entropy/emacs-yas-enable-or-expand)
   )
  :config
  ;; 不用TAB, 因为要避免 org 中与 tab 键冲突
  (unbind-key "TAB" yas-minor-mode-map)
  (unbind-key "<tab>" yas-minor-mode-map))
;; 7db2aa5a ends here

;; [[file:../gwp-scratch.note::7f307588][7f307588]]
;; Use hippie-expand instead of dabbrev-expand
;; (global-set-key (kbd "M-/") #'dabbrev-expand)
(global-set-key (kbd "M-/") #'hippie-expand)
;; the same behavior as the original `dabbrev-expand'
(setq hippie-expand-dabbrev-skip-space t)

;; adjust the list of functions that hippie-expand will try
(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev-visible      ; first try the expansions from the currently visible parts
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-line
        try-expand-dabbrev-from-kill
        ;; try-expand-all-abbrevs
        ;; try-expand-list
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        ))
;; 7f307588 ends here

;; [[file:../gwp-scratch.note::74ebe55a][74ebe55a]]
(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark
  :bind (
         ;; 相当于选中文件弹出右键菜单
         ("C-;" . embark-act)
         ;; 相当于选中文件双击, 用处不大
         ;; ("C-." . embark-dwim)
         ("C-h B" . embark-bindings)
         )
  :custom
  ;; 前置命令序列后按C-h 可选择后续要执行的命令. 相当于 which-key 中的提示的可选
  ;; 命令可搜索后再选择了
  (prefix-help-command #'embark-prefix-help-command)
  )
;; 74ebe55a ends here

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'init-completion)
;; provide:1 ends here