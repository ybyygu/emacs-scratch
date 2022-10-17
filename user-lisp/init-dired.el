;; [[file:../gwp-scratch.note::f3b2a13e][f3b2a13e]]
;;; init-dired.el --- Dired customisations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(setq-default dired-dwim-target t)

;; Hook up dired-x global bindings without loading it up-front
(define-key ctl-x-map "\C-j" 'dired-jump)
(define-key ctl-x-4-map "\C-j" 'dired-jump-other-window)

(with-eval-after-load 'dired
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (define-key dired-mode-map (kbd "j") 'dired-next-line)
  (define-key dired-mode-map (kbd "k") 'dired-previous-line)
  (define-key dired-mode-map (kbd "h") 'dired-up-directory)
  (define-key dired-mode-map (kbd "l") 'dired-view-file)
  (define-key dired-mode-map (kbd "K") 'dired-kill-file) ; 移除 dired buffer 中某行, 不影响文件, 相当于过滤
  (define-key dired-mode-map (kbd "C-S-n") 'dired-create-directory)
  (define-key dired-mode-map (kbd "C-S-f") 'dired-create-empty-file)
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode))

(provide 'init-dired)
;; f3b2a13e ends here
