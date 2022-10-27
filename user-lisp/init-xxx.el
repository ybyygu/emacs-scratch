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
  ;; ("r" gwp::org-show-context-at-point "org show context")
  ("q" nil "quit"))

(gwp::goto-leader-def
  :keymaps '(prog-mode-map org-mode-map)
  ;; "g" '(beginning-of-buffer :which-key "goto first line")
  ;; "e" '(end-of-buffer :which-key "goto last line")
  ;; "l" '(end-of-line :which-key "goto the end of line")
  ;; "h" '(beginning-of-line :which-key "goto the beginning of line")
  ;; "d" '(+lookup/definition :which-key "Jump to definition")
  ;; "f" '(+lookup/file :which-key "Locate file")
  ;; "d" '(xref-find-definitions :which-key "Jump to definition")
  ;; "d" (general-simulate-key "M-.")
  "f" '(find-file-at-point :which-key "Locate file") ; emacs 自带的就很好 (ffap)
  ";" '(goto-last-change :which-key "Go to where the last edit was made")
  )
;; 28526451 ends here

;; [[file:../gwp-scratch.note::c84fab18][c84fab18]]
;; (use-package avy)
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

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'init-xxx)
;; provide:1 ends here
