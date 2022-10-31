;; [[file:../gwp-scratch.note::e59d26a0][e59d26a0]]
;;; -*- lexical-binding: t -*-
;; e59d26a0 ends here

;; [[file:../gwp-scratch.note::c18cdaa7][c18cdaa7]]
(use-package fd-dired)

;;;###autoload
(defun gwp::dired-copy-file-path()
  "复制当前文件全路径"
  (interactive)
  (let ((current-prefix-arg '(0)))
    (call-interactively 'dired-copy-filename-as-kill)))

;;;###autoload
(defun gwp::dired-fd ()
  "使用 fd 递归搜索文件名, 生成 dired 视图"
  (interactive)
  (require 'fd-dired)
  (let ((args (read-string
               "Run fd (with args and search): "
               fd-dired-input-fd-args
               '(fd-dired-args-history . 1))))
    (fd-dired "." args)))
;; c18cdaa7 ends here

;; [[file:../gwp-scratch.note::47bd2234][47bd2234]]
;; emacs 内置的包
(use-package dired-x
  :ensure nil
  :custom
  (dired-omit-verbose t)
  (dired-omit-files (rx (or
                         (seq bol (? ".") "#")
                         (seq bol "." (* anychar) eol) ; example: ".", "..", ".foo"
                         )))
  :init
  (add-hook 'dired-mode-hook #'dired-omit-mode))

(use-package dired-collapse
  :commands (dired-collapse-mode)
  :init
  (add-hook 'dired-mode-hook #'dired-collapse-mode))
;; 47bd2234 ends here

;; [[file:../gwp-scratch.note::f3b2a13e][f3b2a13e]]
(use-package dired
  :ensure nil
  :custom
  ;; 方便多个文件夹文件互动
  ;; Set this variable to non-nil, Dired will try to guess a default
  ;; target directory. This means: if there is a dired buffer
  ;; displayed in the next window, use its current subdir, instead
  ;; of the current subdir of this dired buffer. The target is used
  ;; in the prompt for file copy, rename etc.
  (dired-dwim-target t)
  (dired-auto-revert-buffer t)          ; don't prompt to revert; just do it
  (dired-recursive-copies  'always)
  (dired-recursive-deletes 'top)

  ;; Dired listing switches
  ;;  -a : Do not ignore entries starting with .
  ;;  -l : Use long listing format.
  ;;  -G : Do not print group names like 'users'
  ;;  -h : Human-readable sizes like 1K, 234M, ..
  ;;  -v : Do natural sort .. so the file names starting with . will show up first.
  (dired-listing-switches "-alhvG --group-directories-first") ; default: "-al"

  :config
  ;; Don't complain about this command being disabled when we use it
  (put 'dired-find-alternate-file 'disabled nil)
  ;; 安全第一
  (setq delete-by-moving-to-trash t))
;; f3b2a13e ends here

;; [[file:../gwp-scratch.note::5a48a92b][5a48a92b]]
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
  (define-key dired-mode-map (kbd "C-c C-q") 'wdired-change-to-wdired-mode)
  (define-key dired-mode-map (kbd "DEL") 'dired-up-directory)
  (define-key dired-mode-map (kbd "RET") 'dired-find-alternate-file))

(gwp::local-leader-def
      :keymaps 'dired-mode-map
      "y" '(gwp::dired-copy-file-path :which-key "Copy file path")
      "h" '(dired-omit-mode :which-key "toggle hidden files")
      "t" '(dired-hide-details-mode :which-key "hide details")
      "!" '(dired-do-async-shell-command :which-key "Async shell command")
      "f" '(gwp::dired-fd :which-key "fd files"))
;; 5a48a92b ends here

;; [[file:../gwp-scratch.note::*provide][provide:1]]
(provide 'init-dired)
;; provide:1 ends here
