;; -*- lexical-binding: t; -*-

;; [[file:../gwp-scratch.note::503f98ae][503f98ae]]
(defun unfill-toggle ()
  "Toggle filling/unfilling of the current region, or current paragraph if no
region active."
  (interactive)
  (let (deactivate-mark
        (fill-column
         (if (eq last-command this-command)
             (progn (setq this-command nil)
                    most-positive-fixnum)
           fill-column)))
    (call-interactively 'fill-paragraph)))

; 2025-02-22: 以下不管用
;; (bind-key [remap fill-paragraph] 'unfill-toggle)
(gwp::text-edit-def
  "M-q" #'unfill-toggle)
;; 503f98ae ends here

;; [[file:../gwp-scratch.note::a3f23a6a][a3f23a6a]]
(use-package xah-replace-pairs
  :straight t)

(defun xah-remove-punctuation-trailing-redundant-space (Begin End)
  "Remove redundant whitespace after punctuation.
Works on current line or text selection.

When called in emacs lisp code, the Begin End are cursor positions for region.

See also `xah-convert-english-chinese-punctuation'.

URL `http://xahlee.info/emacs/emacs/elisp_convert_chinese_punctuation.html'
Version: 2015-08-22"
  (interactive
   (if (use-region-p)
       (list (region-beginning) (region-end))
     (list (line-beginning-position) (line-end-position))))
  (require 'xah-replace-pairs)
  (xah-replace-regexp-pairs-region
   Begin End
   [
    ;; clean up. Remove extra space.
    [" +," ","]
    [",  +" ", "]
    ["?  +" "? "]
    ["!  +" "! "]
    ["\\.  +" ". "]

    ;; fullwidth punctuations
    ["， +" "，"]
    ["。 +" "。"]
    ["： +" "："]
    ["？ +" "？"]
    ["； +" "；"]
    ["！ +" "！"]
    ["、 +" "、"]
    ]
   t t))
;; a3f23a6a ends here

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
  :custom
  ;; change how many seconds to wait for char timeout
  (avy-timeout-seconds 0.8)
  (avy-all-windows 'all-frames)
  )
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
  "C-o" #'cycle-spacing
  "C-c SPC" #'untabify                  ; 将TAB 变为空格, 反向命令为 tabify
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

;; [[file:../gwp-scratch.note::7628d03d][7628d03d]]
;; https://endlessparentheses.com/disable-mouse-only-inside-emacs.html
(define-minor-mode disable-mouse-mode
  "A minor-mode that disables all mouse keybinds."
  :global t
  :lighter " 🐭"
  :keymap (make-sparse-keymap)

  (dolist (type '(mouse
                  down-mouse
                  drag-mouse
                  double-mouse
                  triple-mouse))
    (dolist (prefix '("" C- M- S- M-S- C-M- C-S- C-M-S-))
      ;; Yes, I actually HAD to go up to 7 here.
      (dotimes (n 3)
        (let ((k (format "%s%s-%s" prefix type n)))
          (define-key disable-mouse-mode-map
            (vector (intern k)) #'ignore))))))

(defun turn-off-disable-mouse-mode ()
  (disable-mouse-mode -1))

(defun turn-on-disable-mouse-mode ()
  (disable-mouse-mode 1))

;; 在insert状态下禁用鼠标, 避免误碰触控板
;; (add-hook! 'evil-insert-state-entry-hook #'turn-on-disable-mouse-mode)
;; (add-hook! 'evil-insert-state-exit-hook #'turn-off-disable-mouse-mode)
;; (map! :leader
;;       (:prefix-map ("t" . "toggle")
;;        :desc "禁用鼠标" "m" #'disable-mouse-mode
;;        ))
;; 7628d03d ends here

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

;; [[file:../gwp-scratch.note::9e0bf95e][9e0bf95e]]
(defun gwp::replace-chinese-punctuation ()
  "Replace Chinese punctuation with English punctuation in the selected region."
  (interactive)
  (let* ((punctuation-map '(("，" . ", ") ("。" . ". ") ("；" . "; ")
                            ("：" . ": ") ("！" . "! ") ("？" . "? ")
                            ("“" . "\"") ("”" . "\"") ("‘" . "'")
                            ("’" . "'") ("（" . "(") ("）" . ")")))
         (regexp (regexp-opt (mapcar #'car punctuation-map) t))
         (start (region-beginning))
         (end (copy-marker (region-end) t))) ; 使用 marker 确保区域末尾动态更新
    (save-excursion
      (goto-char start)
      (while (re-search-forward regexp end t)
        (replace-match (cdr (assoc (match-string 0) punctuation-map)))))))

(defun gwp::remove-org-bold-markup ()
  "删除当前区域或整个缓冲区中的 Org mode 加粗标记（**文字**）。"
  (interactive)
  (let ((start (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      (while (re-search-forward "\\*\\*\\([^*]+?\\)\\*\\*" end t)
        (replace-match "\\1")))))

(defun gwp::remove-chinese-spaces ()
  "删除中文字符之间的空格，但保留中英文之间的空格。"
  (interactive)
  (let ((start (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (save-excursion
      (goto-char start)
      ;; 匹配中文字符后面的空格再跟中文字符的模式
      ;; 中文Unicode范围是[\u4e00-\u9fff]，也可能包括其他CJK字符和标点
      (while (re-search-forward "\\(\\cc\\) +\\(\\cc\\)" end t)
        (replace-match "\\1\\2")))))

(defun gwp::remove-punctuation-trailing-spaces ()
  "删除标点符号后的多余空格。处理中英文标点符号。"
  (interactive)
  (let ((start (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    (save-excursion
      ;; 西文标点处理
      (goto-char start)
      (while (re-search-forward " +," end t)
        (replace-match ","))
      (goto-char start)
      (while (re-search-forward ", +" end t)
        (replace-match ", "))
      (goto-char start)
      (while (re-search-forward "\\? +" end t)
        (replace-match "? "))
      (goto-char start)
      (while (re-search-forward "! +" end t)
        (replace-match "! "))
      (goto-char start)
      (while (re-search-forward "\\. +" end t)
        (replace-match ". "))

      ;; 中文全角标点处理
      (goto-char start)
      (while (re-search-forward "， +" end t)
        (replace-match "，"))
      (goto-char start)
      (while (re-search-forward "。 +" end t)
        (replace-match "。"))
      (goto-char start)
      (while (re-search-forward "： +" end t)
        (replace-match "："))
      (goto-char start)
      (while (re-search-forward "？ +" end t)
        (replace-match "？"))
      (goto-char start)
      (while (re-search-forward "； +" end t)
        (replace-match "；"))
      (goto-char start)
      (while (re-search-forward "！ +" end t)
        (replace-match "！"))
      (goto-char start)
      (while (re-search-forward "、 +" end t)
        (replace-match "、")))))

(defun gwp::format-chinese-paragraph ()
  "综合格式化中文段落：
1. 删除加粗标记（**文字**）
2. 删除中文字符之间的多余空格
3. 删除标点符号后的多余空格"
  (interactive)
  (let ((start (if (region-active-p) (region-beginning) (point-min)))
        (end (if (region-active-p) (region-end) (point-max))))
    ;; 保存当前选区信息
    (let ((region-active (region-active-p)))
      ;; 删除加粗标记
      (gwp::remove-org-bold-markup)
      ;; 重新设置选区（如果原本有选区的话）
      (when region-active
        (set-mark start)
        (goto-char end)
        (activate-mark))
      ;; 删除中文之间的空格
      (gwp::remove-chinese-spaces)
      ;; 删除中文标点后的多余空格
      (gwp::remove-punctuation-trailing-spaces))))
;; 9e0bf95e ends here

;; [[file:../gwp-scratch.note::382df7e2][382df7e2]]
(provide 'init-edit)
;; 382df7e2 ends here
