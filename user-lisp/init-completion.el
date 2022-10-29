;; [[file:../gwp-scratch.note::290a045b][290a045b]]
;; -*- lexical-binding: t; -*-
;; 290a045b ends here

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

;; [[file:../gwp-scratch.note::23685638][23685638]]
(use-package hydra)
;; 23685638 ends here

;; [[file:../gwp-scratch.note::3702e7df][3702e7df]]
;; Enable vertico
(use-package vertico
  :init
  (vertico-mode)

  ;; Different scroll margin
  ;; (setq vertico-scroll-margin 0)

  ;; Show more candidates
  ;; (setq vertico-count 20)

  ;; Grow and shrink the Vertico minibuffer
  ;; (setq vertico-resize t)

  ;; Optionally enable cycling for `vertico-next' and `vertico-previous'.
  (setq vertico-cycle t))

(use-package vertico-repeat
  :after vertico
  :ensure nil
  :hook (minibuffer-setup . vertico-repeat-save)
  :bind
  ([remap ivy-resume] . vertico-repeat)
  )

;; 可用鼠标操纵 minibuffer
(use-package vertico-mouse
  :ensure nil
  :after vertico
  :config
  (vertico-mouse-mode))

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :init
  (savehist-mode))

;; 启用模糊匹配
(use-package orderless
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))
;; 3702e7df ends here

;; [[file:../gwp-scratch.note::02437dc0][02437dc0]]
(use-package consult
  :custom
  ;; https://github.com/minad/consult#live-previews
  ;; preview delayed
  ;; (consult-preview-key '(:debounce 1 any))
  ;; preview manually
  (consult-preview-key (kbd "M-."))
  :bind (
         ([remap apropos-command] . consult-apropos) ; SPC-h-a
         ;; avy-goto-line 更好用
         ;; ([remap goto-line] . consult-goto-line)
         ([remap isearch-forward] . consult-line)
         ([remap yank-pop] . consult-yank-pop)
         ([remap repeat-complex-command] . consult-complex-command)
         ([remap recentf-open-files] . consult-recent-file)
         ([remap bookmark-jump] . consult-bookmark)
         ([remap project-switch-to-buffer] . consult-project-buffer)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap switch-to-buffer-other-window] . consult-buffer-other-window)
         ([remap switch-to-buffer-other-frame] . consult-buffer-other-frame)
         ([remap imenu] . consult-imenu)
         ([remap list-registers] . consult-register)
         ([remap point-to-register] . consult-register-store)
         ([remap gwp::rg] . consult-ripgrep)
         ([remap gwp::mark-ring] . consult-mark)
         ;; Minibuffer history
         :map minibuffer-local-map
         ("M-s" . consult-history)                 ;; orig. next-matching-history-element
         ("M-r" . consult-history)                 ;; orig. previous-matching-history-element
         )
  ;; The :init configuration is always executed (Not lazy)
  :init
  :config
  (with-eval-after-load 'org
    (define-key org-mode-map [remap org-goto] #'consult-org-heading))
  )

;; (use-package consult-notes
;;   :commands (consult-notes consult-notes-search-in-all-notes)
;;   :config
;;   (setq consult-notes-sources `(("GTD"  ?g  "~/Notes/") ("all notes"  ?o  "~/.cache/notes")))
;;   )
;; 02437dc0 ends here

;; [[file:../gwp-scratch.note::74ebe55a][74ebe55a]]
;; 补全窗口显示补助等信息
(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark
  :bind (
         ;; 相当于选中文件弹出右键菜单
         ("C-;" . embark-act)
         ;; more like `ivy-occur'
         ("C-c C-o" . embark-export)
         ;; 相当于选中文件双击, 用处不大
         ;; ("C-." . embark-dwim)
         )
  :custom
  ;; 前置命令序列后按C-h 可选择后续要执行的命令. 相当于 which-key 中的提示的可选
  ;; 命令可搜索后再选择了
  (prefix-help-command #'embark-prefix-help-command)
  )

(use-package embark-consult
  :if (featurep 'embark)
  :after consult)

;; 查看不同 mode 下的按键更方便(C-h RET)
(bind-key "C-m" 'embark-bindings-in-keymap help-map)
;; 74ebe55a ends here

;; [[file:../gwp-scratch.note::b0577e97][b0577e97]]
(use-package which-key
  :ensure t
  :init
  (which-key-mode)
  :diminish which-key-mode
  :custom
  (which-key-idle-delay 1)
  (which-key-sort-uppercase-first nil)
  :bind (([remap describe-keymap] . which-key-show-keymap))
  :config
  )
;; b0577e97 ends here

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'init-completion)
;; provide:1 ends here
