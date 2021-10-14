;; [[file:../gwp.note::e2f6b646][e2f6b646]]
(use-package org
  :config
  ;; treat .note files as org-mode
  (add-to-list 'auto-mode-alist '("\\.note\\'" . org-mode))
  (add-to-list 'auto-mode-alist '("NOTE" . org-mode))

  (setq org-blank-before-new-entry nil)
  (setq org-default-notes-file (concat org-directory "/life.note"))

  ;; 保留以前的 Alt-Return 键行为, Alt-Return
  (org-defkey org-mode-map [(meta return)] 'org-meta-return)

  ;; https://orgmode.org/manual/Clean-view.html
  (setq org-startup-indented t)      ;Enable `org-indent-mode' on Org startup
  (with-eval-after-load 'org-indent
    (setq org-indent-indentation-per-level 1)) ;; default = 2

  ;; 对齐headline中的TAGs
  (setq org-tags-column -80)

  ;; 避免误编辑
  (setq org-catch-invisible-edits 'show-and-error))

(use-package org-superstar
  :init
  ;; ◉ ○ ◆ » ◇ ▶ ▷
  (setq org-superstar-headline-bullets-list '("◉" "▶" "▷" "»"))
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
;; e2f6b646 ends here

;; [[file:../gwp.note::*provide][provide:1]]
(provide 'init-org)
;; provide:1 ends here
