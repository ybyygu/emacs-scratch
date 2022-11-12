;; [[file:../gwp-scratch.note::9e836961][9e836961]]
;; -*- lexical-binding: t; -*-
;; 9e836961 ends here

;; [[file:../gwp-scratch.note::b76494d2][b76494d2]]
(gwp::text-edit-def
  "M-u" #'upcase-dwim
  "M-l" #'downcase-dwim
  "M-c" #'capitalize-dwim
  )
;; 原用于 downcase-region, upcase-region. 可释放
(global-unset-key (kbd "C-x C-l"))
(global-unset-key (kbd "C-x C-u"))
;; b76494d2 ends here

;; [[file:../gwp-scratch.note::28526451][28526451]]
;; 比 goto-last-change 好用一些?
(use-package goto-chg)

;;;###autoload
(defun gwp::goto-last-change ()
  (interactive)
  (call-interactively #'goto-last-change)
  (org-mark-jump-unhide))

;;;###autoload
(defun gwp::goto-last-change-reverse ()
  (interactive)
  (call-interactively #'goto-last-change-reverse)
  (org-mark-jump-unhide))

(defhydra gwp::hydra-last-change ()
  ("j" gwp::goto-last-change "last change")  ; 用 p 按键容易误操作, 用N 安全些
  ("k" gwp::goto-last-change-reverse "previous change")
  ("c" recenter "recenter")
  ;; ("r" gwp::org-show-context-at-point "org show context")
  ("q" nil "quit"))


(gwp::goto-leader-def
  ;; :keymaps '(prog-mode-map org-mode-map)
  :keymaps '(meow-normal-state-keymap)
  ";" '(gwp::goto-last-change :which-key "go to last change")
  )
;; 28526451 ends here

;; [[file:../gwp-scratch.note::500a7c61][500a7c61]]
;; https://github.com/manateelazycat/delete-block/blob/master/delete-block.el
(require 'subword)

(defun delete-block-forward ()
  (interactive)
  (if (eobp)
      (message "End of buffer")
    (let* ((syntax-move-point
            (save-excursion
              (skip-syntax-forward (string (char-syntax (char-after))))
              (point)
              ))
           (subword-move-point
            (save-excursion
              (subword-forward)
              (point))))
      (kill-region (point) (min syntax-move-point subword-move-point)))))

(defun delete-block-backward ()
  (interactive)
  (if (bobp)
      (message "Beginning of buffer")
    (let* ((syntax-move-point
            (save-excursion
              (skip-syntax-backward (string (char-syntax (char-before))))
              (point)
              ))
           (subword-move-point
            (save-excursion
              (subword-backward)
              (point))))
      (kill-region (point) (max syntax-move-point subword-move-point)))))

(gwp::text-edit-def
  "M-d" #'delete-block-forward
  "M-<backspace>" #'delete-block-backward
  )
;; 500a7c61 ends here

;; [[file:../gwp-scratch.note::c84fab18][c84fab18]]
(use-package avy
  :config
  :after init-bindings
  (setq avy-all-windows t))
;; c84fab18 ends here

;; [[file:../gwp-scratch.note::02dde369][02dde369]]
;;;###autoload
(defun gwp::avy-copy-region-to ()
  "将当前行或当前区域复制到远处某行"
  (interactive)
  (call-interactively #'kill-ring-save)
  (call-interactively #'avy-goto-line)
  (call-interactively #'yank))
;; 02dde369 ends here

;; [[file:../gwp-scratch.note::93427a65][93427a65]]
(require 'transient)
(transient-define-prefix gwp::avy-transient ()
  "goto utilities"
  ["avy"
   ("l" "goto line" avy-goto-line)      ; C-u 调用时, 限定当前窗口. 以其它数字参数调用时, 直达该行
   ("b" "jump back" avy-pop-mark)       ; 回到 avy 起跳点
   ("c" "find char" avy-goto-char-timer) ; C-u 调用时, 作用同avy-goto-line.
   ("r" "avy resume" avy-resume)
   ]
  )

;; 替换默认按键(M-g)
(bind-key [remap goto-line] 'avy-goto-line)
(bind-key [remap goto-char] 'avy-goto-char-timer)

(gwp::goto-leader-def
  ;; :keymaps '(prog-mode-map org-mode-map)
  "," '(avy-pop-mark :which-key "avy go back")
  )

(bind-keys :map gwp::edit-map
           ("l" . avy-copy-line)
           ("c" . gwp::avy-copy-region-to)
           ("L" . avy-copy-region))
;; 93427a65 ends here

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

;; [[file:../gwp-scratch.note::4c0b6f58][4c0b6f58]]
(use-package isearch
  :ensure nil
  :requires avy
  :config
  :bind
  (:map search-map
        ("`" . avy-isearch))
  (:map isearch-mode-map
        ("C-c j" . avy-isearch)
        ("C-c C-j" . avy-isearch)))
;; 4c0b6f58 ends here

;; [[file:../gwp-scratch.note::6cb02a16][6cb02a16]]
(gwp::goto-leader-def
  ;; :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
  "g" (general-simulate-key "M-<" :which-key "goto first line")
  "e" (general-simulate-key "M->" :which-key "goto last line")
  "h" (general-simulate-key "C-a" :which-key "goto the beggining of line")
  "l" (general-simulate-key "C-e" :which-key "goto the end of line")
  "f" '(find-file-at-point :which-key "Locate file") ; emacs 自带的就很好 (ffap)
  "." 'goto-line
  "c" 'goto-char
  )
;; 6cb02a16 ends here

;; [[file:../gwp-scratch.note::dfa96ecd][dfa96ecd]]
(use-package keyfreq
  :config
  (keyfreq-mode 1)
  (keyfreq-autosave-mode 1))
;; dfa96ecd ends here

;; [[file:../gwp-scratch.note::382df7e2][382df7e2]]
(provide 'init-edit)
;; 382df7e2 ends here
