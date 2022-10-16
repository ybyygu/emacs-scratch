;; [[file:../gwp-scratch.note::e2f6b646][e2f6b646]]
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
;; e2f6b646 ends here

;; [[file:../gwp-scratch.note::0caa1907][0caa1907]]
(use-package org-superstar
  :init
  ;; ◉ ○ ◆ » ◇ ▶ ▷
  (setq org-superstar-headline-bullets-list '("☰" "▶" "▷" "»"))
  (add-hook 'org-mode-hook (lambda () (org-superstar-mode 1))))
;; 0caa1907 ends here

;; [[file:../gwp-scratch.note::dfee4224][dfee4224]]
(gwp::goto-leader-def
  :keymaps 'org-mode-map
  "k" '(org-up-element :which-key "goto up element")
  "j" '(org-next-visible-heading :which-key "next visible heading")
  ;; "h" '(org-beginning-of-line :which-key "goto the beginning of visible line")
  ;; "l" '(org-end-of-line :which-key "goto the end of visible line")
  ;; "k" '(org-backward-heading-same-level :which-key "backward heading")
  ;; "j" '(org-forward-heading-same-level :which-key "forward heading")
  )

(general-define-key
 :keymaps 'org-mode-map
 "M-l" #'org-metaright   ; doom中默认为 demote-subtree
 "M-h" #'org-metaleft    ; doom中默认为 promote-subtree
 "M-k" #'org-metaup
 "M-j" #'org-metadown
 "M-p" #'org-backward-element
 "M-n" #'org-forward-element
 )

(gwp::dwim-leader-def
  :keymaps 'org-mode-map
  "g" 'counsel-org-goto                        ; goto
  "t" 'org-todo                                ; todo
  "e" 'org-edit-special                        ; edit
  "a" 'org-attach                              ; attach
  ;; "b" 'gwp/org-babel-tangle-dwim               ; babel
  ;; "n" 'gwp::org-babel-narrow-to-tangle-heading ; narrow
  )

(gwp::dwim-leader-def
  :keymaps 'org-src-mode-map
  ;; "b" 'gwp/org-babel-tangle-dwim
  "q" 'org-edit-src-exit
  )
;; dfee4224 ends here

;; [[file:../gwp-scratch.note::183d2d8f][183d2d8f]]
(provide 'init-org)
;; 183d2d8f ends here
