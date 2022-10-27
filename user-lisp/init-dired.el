;; [[file:../gwp-scratch.note::e59d26a0][e59d26a0]]
;;; -*- lexical-binding: t -*-

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
;; e59d26a0 ends here

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'init-dired)
;; provide:1 ends here
