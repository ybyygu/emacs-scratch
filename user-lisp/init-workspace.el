;; [[file:../gwp-scratch.note::902df907][902df907]]
;; -*- lexical-binding: t; -*-
;; 902df907 ends here

;; [[file:../gwp-scratch.note::36ca6867][36ca6867]]
(require 'esh-mode)

;; credit: http://www.howardism.org/Technical/Emacs/eshell-fun.html
(defun gwp::open-eshell-here ()
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    ;; (split-window-vertically (- height))
    ;; (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))
    (insert (concat "ls -l"))
    (eshell-send-input)))

(defun eshell-exit-when-eolp ()
  "Exit Eshell if point is at end of line.
Otherwise delete one character."
  (interactive)
  (if (not (eolp))
      (delete-char 1)
    (insert "exit")
    (eshell-send-input)))

;; credit: https://github.com/rafoo/my-emacs-config/blob/master/.emacs.d/elisp/eshell-conf.el
(defun eshell-C-d-hook ()
  "Hook binding `C-d' to `eshell-exit-when-eolp' in eshell buffers."
  ;; I don't know how to do this whitout local-set-key
  ;; because eshell-mode-map is buffer-local
  ;; (and I don't know why).
  (local-set-key
   (kbd "C-d")
   'eshell-exit-when-eolp))

;; C-d in eshell exit
(add-hook 'eshell-mode-hook #'eshell-C-d-hook)

(defun eshell/vi (&rest args)
  "Invoke `find-file' on the file.
\"vi +42 foo\" also goes to line 42 in the buffer."
  (while args
    (if (string-match "\\`\\+\\([0-9]+\\)\\'" (car args))
        (let* ((line (string-to-number (match-string 1 (pop args))))
               (file (pop args)))
          (find-file file)
          (goto-line line))
      (find-file (pop args)))))

(defun gwp::eshell-insert-scp ()
  (interactive)
  (insert "scp hpc44:.lpwd/"))

(bind-keys :map eshell-mode-map
           ("C-c t" . gwp::eshell-insert-scp))

(defun gwp::open-in-x-terminal (the-directory)
  (let ((process-connection-type nil))
    (start-process "" nil "alacritty" (concat "--working-directory=" the-directory) "-e" "tmux")))

;;;###autoload
(defun gwp::open-terminal-here ()
  "Open the current dir in a new terminal window"
  (interactive)
  (let ((default-directory (or (and (eq major-mode 'dired-mode)
                                    (dired-current-directory))
                               default-directory)))
    (gwp::open-in-x-terminal (expand-file-name default-directory))))
;; 36ca6867 ends here

;; [[file:../gwp-scratch.note::62d090d7][62d090d7]]
(use-package zoxide
  :ensure nil
  :commands (gwp::recent-dirs)
  :bind
  (:map gwp::develop-map
        ("r" . gwp::recent-dirs)))
;; 62d090d7 ends here

;; [[file:../gwp-scratch.note::942579e1][942579e1]]
;; credit: https://github.com/joodland/bm
(use-package bm
  :commands (bm-buffer-restore bm-buffer-save bm-toggle bm-next bm-previous bm-buffer-save-all)
  :custom
  ;; do not cross-buffer for 'next'
  (bm-cycle-all-buffers nil)
  (bm-highlight-style 'bm-highlight-only-fringe)
  ;; save bookmarks
  (bm-buffer-persistence t)
  ;; where to store persistant files
  (bm-repository-file (expand-file-name "bm-repository" user-emacs-directory))

  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)

  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'find-file-hooks 'bm-buffer-restore)
  ;; (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  ;; (add-hook 'kill-buffer-hook 'bm-buffer-save)
  ;; (add-hook 'after-save-hook 'bm-buffer-save)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
  ;; 自动展开 org heading
  (add-hook 'bm-after-goto-hook 'org-bookmark-jump-unhide)

  :config
  (defun gwp-mouse-toggle-bm (e)
    "Toggle bookmarking
This command should be bound to a mouse key.
Argument E is a mouse event used by `mouse-set-point'."
    (interactive "@e")
    (save-excursion
      (mouse-set-point e)
      (bm-toggle)))

  ;; toggle bm 时第一时间保存, 避免冲突
  (defun gwp::bm-buffer-save-advice (&rest r)
    (bm-buffer-save))
  (advice-add 'bm-toggle :after #'gwp::bm-buffer-save-advice)

  ;; :hook
  ;; Loading the repository from file when on start up.
  ;; (add-hook 'after-init-hook 'bm-repository-load)
  :bind (
         ([left-fringe double-mouse-1] . gwp-mouse-toggle-bm)
         ([left-margin double-mouse-1] . gwp-mouse-toggle-bm)
         ([left-fringe mouse-5] . bm-next-mouse)
         ([left-margin mouse-5] . bm-next-mouse)
         ([left-fringe mouse-4] . bm-previous-mouse)
         ([left-margin mouse-4] . bm-previous-mouse)
         ))
;; 942579e1 ends here

;; [[file:../gwp-scratch.note::ebe60f2d][ebe60f2d]]
(use-package bm
  :requires transient
  :config
  (transient-define-prefix gwp::bookmark-transient ()
    "visual bookmarks"
    ["Edit bookmarks:"
     ("b" "Toggle bookmarks" bm-toggle)
     ("e" "Setting bookmarks based on a regexp" bm-bookmark-regexp)
     ("d" "Remove all bookmarks in current buffer" bm-remove-all-current-buffer)
     ("a" "Annotate bookmarks" bm-bookmark-annotate)
     ("s" "Save bookmarks" bm-buffer-save)
     ]
    ["Navigate bookmarks"
     ;; 以下不设置 :transient, 按z 键 repeat 更方便些
     ("n" "Next bookmark" bm-next)
     ("p" "Prev bookmark" bm-previous)
     ]
    )
  :bind (
         :map gwp::develop-map
         ("b" . gwp::bookmark-transient)
         ))
;; ebe60f2d ends here

;; [[file:../gwp-scratch.note::fc2cdcbf][fc2cdcbf]]
;; credit: doom/tools/tmux
;;
(defvar +tmux-last-command nil
  "The last command ran by `+tmux'. Used by `+tmux/rerun'")

(defvar +tmux-last-retcode nil
  "The last tmux return code.")

;;
;; Commands

;;;###autoload
(defun +tmux (command &rest args)
  "Execute COMMAND in tmux"
  (let ((bin (executable-find "tmux")))
    (unless bin
      (error "Could not find tmux executable"))
    (let* ((args (mapcar #'shell-quote-argument (delq nil args)))
           (cmdstr (format "%s %s" bin (if args (apply #'format command args) command)))
           (output (get-buffer-create " *tmux stdout*"))
           (errors (get-buffer-create " *tmux stderr*"))
           code)
      (unwind-protect
          (if (= 0 (setq code (shell-command cmdstr output errors)))
              (with-current-buffer output
                (setq +tmux-last-command `(,(substring cmdstr (+ 1 (length bin))) ,@args))
                (buffer-string))
            (error "[%d] tmux $ %s (%s)"
                   code
                   (with-current-buffer errors
                     (buffer-string))
                   cmdstr))
        (and (kill-buffer output)
             (kill-buffer errors))))))

;;;###autoload
(defun +tmux/run (command &optional noreturn)
  "Run COMMAND in tmux. If NORETURN is non-nil, send the commands as keypresses
but do not execute them."
  (interactive
   (list (read-string "tmux $ ")
         current-prefix-arg))
  (+tmux (concat "send-keys C-u "
                 (shell-quote-argument command)
                 (unless noreturn " Enter"))))

;;;###autoload
(defun +tmux/send-region (beg end &optional noreturn)
  "Send region to tmux."
  (interactive (list (region-beginning)
                     (region-end)
                     current-prefix-arg))
  (+tmux/run (string-trim (buffer-substring-no-properties beg end))
             noreturn))

;;;###autoload
(defun +tmux/rerun ()
  "Rerun the last command executed by `+tmux' and `+tmux/run'."
  (interactive)
  (unless +tmux-last-command
    (user-error "No last command to run"))
  (apply #'+tmux +tmux-last-command))

;;;###autoload
(defun +tmux/cd (&optional directory noreturn)
  "Change the pwd of the currently active tmux pane to DIRECTORY.

DIRECTORY defaults to `default-directory' if omitted, or to `doom-project-root'
if prefix arg is non-nil.

If NORETURN is non-nil, send the cd command to tmux, but do not execute the
command."
  (interactive "D")
  (+tmux/run (format "cd %S" (or directory default-directory))
             noreturn))

;;;###autoload
(defun +tmux/cd-to-here ()
  "cd into `default-directory' in tmux."
  (interactive)
  (+tmux/cd default-directory))
;; fc2cdcbf ends here

;; [[file:../gwp-scratch.note::90e483a3][90e483a3]]
;;;###autoload
(defun gwp::tmux-open-vertical ()
  "Open a vertical pane."
  (interactive)
  (+tmux "split-window -h -p 40"))

;;;###autoload
(defun gwp::tmux-open-horizontal ()
  "Open a horizontal pane."
  (interactive)
  (+tmux "split-window -v -p 40"))

;;;###autoload
(defun gwp::tmux-new-window ()
  "Open a new window."
  (interactive)
  (+tmux "new-window"))

;; (unless (display-graphic-p)
;;   (bind-keys :map gwp::open-map
;;              ("t" . gwp::tmux-new-window)
;;              ("v" . gwp::tmux-open-vertical)))
;; 90e483a3 ends here

;; [[file:../gwp-scratch.note::c93aeaa5][c93aeaa5]]
(defun +same-major-mode-buffer-list ()
  "返回与当前 buffer major-mode 一致的 buffer list"
  (let* ((buffers (buffer-list))
         (buffers (delq (current-buffer) buffers))
         (mode major-mode))
    (cl-loop for buf in buffers
             if (eq (with-current-buffer buf major-mode) mode)
             collect buf)))

;;;###autoload
(defun gwp::switch-buffer-dwim ()
  "Switch to buffer, filtered by `major-mode'."
  (interactive)

  (require 'consult)
  (let* ((buffers (+same-major-mode-buffer-list))
         (candidates (mapcar #'list (mapcar #'buffer-name buffers)))
         ;; 保留原 buffer list 中的次序
         (vertico-sort-function nil))
    (switch-to-buffer
     (consult--read candidates :prompt (format "Similar buffer for %s: " major-mode)
		    :category 'buffer))))

(bind-key "b" #'gwp::switch-buffer-dwim gwp::buffer-map)
;; c93aeaa5 ends here

;; [[file:../gwp-scratch.note::3413b84e][3413b84e]]
(unless init-no-x-flag
  (use-package vterm
    :ensure t
    :config
    (add-to-list 'meow-mode-state-list '(vterm-mode . insert))))
;; 3413b84e ends here

;; [[file:../gwp-scratch.note::7edef0cf][7edef0cf]]
(require 'tab-bar)
(require 'embark)
(defun gwp::bookmark-jump-other-tab (bookmark)
  "Jump to BOOKMARK in another tab."
  (interactive
   (list (bookmark-completing-read "Jump to bookmark (in another tab)"
                                   bookmark-current-bookmark)))
  (tab-new)
  (bookmark-jump bookmark)
  (tab-rename bookmark))

(bind-key "t" #'gwp::bookmark-jump-other-tab embark-bookmark-map)
(bind-key "t" #'find-file-other-tab embark-file-map)
(bind-key "t" #'switch-to-buffer-other-tab embark-buffer-map)

(use-package tab-bar
  :ensure nil
  :custom
  ;; 不显示 tab-bar 上的 "x" 按钮
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  ;; (tab-bar-show nil)
  :config
  ;; 不显示tab-bar
  ;; 改进 tab-bar 显示样式
  (set-face-attribute 'tab-bar nil :inherit 'mode-line)
  (set-face-attribute 'tab-bar-tab nil
                      :weight 'bold
                      :slant 'italic
                      :underline t
                      :foreground "#aaee77")
  (set-face-attribute 'tab-bar-tab-inactive nil
                      ;; :slant 'italic
                      :foreground "#afafaf"))
;; 7edef0cf ends here

;; [[file:../gwp-scratch.note::f95a72e3][f95a72e3]]
(unbind-key "C-x C-p")
;; f95a72e3 ends here

;; [[file:../gwp-scratch.note::f0f6f6eb][f0f6f6eb]]
(provide 'init-workspace)
;; f0f6f6eb ends here
