;; [[file:../gwp-scratch.note::9e836961][9e836961]]
;; -*- lexical-binding: t; -*-
;; 9e836961 ends here

;; [[file:../gwp-scratch.note::b76494d2][b76494d2]]
(gwp::text-edit-def
  "M-u" #'upcase-dwim
  "M-l" #'downcase-dwim
  "M-c" #'capitalize-dwim
  )
;; b76494d2 ends here

;; [[file:../gwp-scratch.note::28526451][28526451]]
;; 比 goto-last-change 好用一些?
(use-package goto-chg)

(defhydra gwp::hydra-last-change ()
  ("j" goto-last-change "last change")  ; 用 p 按键容易误操作, 用N 安全些
  ("k" goto-last-change-reverse "previous change")
  ("c" recenter "recenter")
  ("r" gwp::org-show-context-at-point "org show context")
  ("q" nil "quit"))

(gwp::goto-leader-def
  :keymaps '(prog-mode-map org-mode-map)
  ";" '(goto-last-change :which-key "Go to where the last edit was made")
  )
;; 28526451 ends here

;; [[file:../gwp-scratch.note::c84fab18][c84fab18]]
(use-package avy
  :config
  :after init-bindings
  (setq avy-all-windows t)
  :config

  ;; 2022-10-28: 不太管用
  ;; :bind
  ;; ([remap goto-line] . avy-goto-line)
  ;; ([remap goto-char] . avy-goto-char-timer)
  )

(bind-key "l" 'avy-copy-line gwp::edit-map)
(bind-key "L" 'avy-copy-region gwp::edit-map)
;; c84fab18 ends here

;; [[file:../gwp-scratch.note::ab440ea2][ab440ea2]]
;;;###autoload
(defun gwp::insert-date (arg)
  "Insert date at point. With prefix argument, insert date and time."
  (interactive "P")
  (insert (format-time-string "%Y-%m-%d"))
  (when arg
    (insert (format-time-string " %H:%M"))))

;; make it easier to update time-stamp
(gwp::text-edit-def "C-c i" #'gwp::insert-date)
;; ab440ea2 ends here

;; [[file:../gwp-scratch.note::989c4b3c][989c4b3c]]
(setq show-trailing-whitespace t)

(gwp::text-edit-def
  ;; 删除多余空行, 仅保留一行
  "C-x C-o" #'delete-blank-lines
  ;; "C-c C-o" #'delete-blank-lines
  "C-o" #'cycle-spacing
  )
;; 989c4b3c ends here

;; [[file:../gwp-scratch.note::6cb02a16][6cb02a16]]
(gwp::goto-leader-def
  :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
  "g" (general-simulate-key "M-<" :which-key "goto first line")
  "e" (general-simulate-key "M->" :which-key "goto last line")
  "h" (general-simulate-key "C-a" :which-key "goto the beggining of line")
  "l" (general-simulate-key "C-e" :which-key "goto the end of line")
  "." '(gwp::goto-transient :which-key "avy jump")
  "f" '(find-file-at-point :which-key "Locate file") ; emacs 自带的就很好 (ffap)
  )

(transient-define-prefix gwp::goto-transient ()
  "goto utilities"
  ["avy"
   ("." "goto line" avy-goto-line)
   ("," "jump back" avy-pop-mark) ; 回到 avy 起跳点
   ("c" "find char" avy-goto-char-timer)
   ("r" "avy resume" avy-resume)
   ]
  )
;; 6cb02a16 ends here

;; [[file:../gwp-scratch.note::382df7e2][382df7e2]]
(provide 'init-xxx)
;; 382df7e2 ends here
