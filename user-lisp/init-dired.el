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
                         (seq bol (? ".") "#") ; emacs autosave files
                         (seq "~" eol)                 ; emacs default backup files
                         ;; "." 或 ".." 挺有用, 鼠标点击"." 相当于redisplay, ".." 相当于返回上级目录
                         ;; (seq bol "." (* anychar) eol) ; example: ".", "..", ".foo"
                         (seq bol "." (not (any "."))) ;; only dot-files, but keep "." or ".."
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

;; [[file:../gwp-scratch.note::ef983ce4][ef983ce4]]
(use-package dired-x
  :ensure nil
  :config
  (add-to-list 'dired-guess-shell-alist-user '("\\.gjf\\'" "agui"))
  (add-to-list 'dired-guess-shell-alist-user '("\\.com\\'" "agui"))
  (add-to-list 'dired-guess-shell-alist-user '("\\.xyz\\'" "jmol"))
  (add-to-list 'dired-guess-shell-alist-user '("\\.cif\\'" "jmol"))
  )
;; ef983ce4 ends here

;; [[file:../gwp-scratch.note::241e0f16][241e0f16]]
;; (unbind-key "<mouse-2>" dired-mode-map)
(bind-keys :map dired-mode-map
           ;; 2022-11-01: 绑定在中键上会被 dired 覆盖
           ;; ([double-mouse-1] . gwp::dired-mouse-open-file-externally)
           ([mouse-3] . gwp::dired-mouse-open-file-externally))

(defun gwp::dired-mouse-open-file-externally (event)
  "使用外部命令打开鼠标点击的文件或文件夹"
  (interactive "e")
  (dired-mouse-find-file event #'spacemacs/open-in-external-app #'spacemacs/open-in-external-app))
;; 241e0f16 ends here

;; [[file:../gwp-scratch.note::8903fab8][8903fab8]]
;; credit: https://protesilaos.com/emacs/denote#h:d35d8d41-f51b-4139-af8f-9c8cc508e35b
(defvar prot-dired--limit-hist '()
  "Minibuffer history for `prot-dired-limit-regexp'.")

;;;###autoload
(defun prot-dired-limit-regexp (regexp omit)
  "Limit Dired to keep files matching REGEXP.

With optional OMIT argument as a prefix (\\[universal-argument]),
exclude files matching REGEXP.

Restore the buffer with \\<dired-mode-map>`\\[revert-buffer]'."
  (interactive
   (list
    (read-regexp
     (concat "Files "
             (when current-prefix-arg
               (propertize "NOT " 'face 'warning))
             "matching PATTERN: ")
     nil 'prot-dired--limit-hist)
    current-prefix-arg))
  (dired-mark-files-regexp regexp)
  (unless omit (dired-toggle-marks))
  (dired-do-kill-lines))
;; 8903fab8 ends here

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
  ;; 像 nautilus 中一样处理
  (define-key dired-mode-map (kbd "C-s") 'prot-dired-limit-regexp)
  (define-key dired-mode-map (kbd "C-r") 'dired-do-redisplay)
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
