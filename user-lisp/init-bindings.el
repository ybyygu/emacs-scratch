;; [[file:../gwp-scratch.note::a4d73c60][a4d73c60]]
;; -*- lexical-binding: t; -*-
;; a4d73c60 ends here

;; [[file:../gwp-scratch.note::4cf8c86c][4cf8c86c]]
(defun gwp::switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(gwp::leader-def
  "SPC" '(gwp::mark-and-save-buffer :which-key "Save buffer")
  "," '(gwp::jump-to-previous-mark :which-key "Jump to previous mark")
  "`" '(gwp::switch-to-previous-buffer :which-key "Switch to previous buffer")
  "u" '(universal-argument :which-key "Universal argument")
  "Q" '(unbury-buffer :which-key "recover old buffer")
  )
;; 4cf8c86c ends here

;; [[file:../gwp-scratch.note::33105bcf][33105bcf]]
(defun gwp::find-last-killed-file ()
  (interactive)
  (let ((active-files (cl-loop for buf in (buffer-list)
                               when (buffer-file-name buf) collect it)))
    (cl-loop for file in recentf-list
             unless (member file active-files) return (find-file file))))

(gwp::leader-def
  "b" '(:keymap gwp::buffer-map :which-key "buffer" :package emacs))

(bind-keys :map gwp::buffer-map
           ("d" . kill-current-buffer)
           ("k" . kill-current-buffer)
           ("r" . revert-buffer)
           ("n" . next-buffer)
           ("p" . previous-buffer)
           ("u" . gwp::find-last-killed-file)
           ("K" . project-kill-buffers)
           ("R" . crux-rename-buffer-and-file))
;; 33105bcf ends here

;; [[file:../gwp-scratch.note::f220a2a2][f220a2a2]]
(gwp::leader-def
 "s" '(:ignore t :which-key "search")
 "ss" '(consult-line :which-key "search buffer")
 "si" '(imenu :which-key "jump to symbol(imenu)")
 ;; gwp::rg 会虚拟命令, 待 consult remap
 "sd" '(gwp::rg :which-key "search directory")
 "sg" '(gwp::git-grep :which-key "git grep")
 "sn" '(gwp::search-all-notes :which-key "search all .note files"))
;; f220a2a2 ends here

;; [[file:../gwp-scratch.note::e13c7903][e13c7903]]
(defun spacemacs/open-in-external-app (file-path)
  "Open `file-path' in external application."
  (let ((process-connection-type nil))
    (start-process "" nil "xdg-open" file-path)))

(defun spacemacs/open-file-or-directory-in-external-app (arg)
  "Open current file in external application.
If the universal prefix argument is used then open the folder
containing the current file by the default explorer.
If two universal prefix arguments are used, then prompt for command to use."
  (interactive "P")
  (if (equal arg '(4))                  ; C-u
      (spacemacs/open-in-external-app (expand-file-name default-directory))
    (let ((file-path (if (derived-mode-p 'dired-mode)
                         (dired-get-file-for-visit)
                       buffer-file-name)))
      (if file-path
          (if (equal arg '(16))         ; C-u C-u
              (progn
                (let ((program (read-shell-command "Open current file with: ")))
                  (call-process program nil 0 nil file-path)))
            (spacemacs/open-in-external-app file-path))
        ;; for EAF pdf
        (if (derived-mode-p 'eaf-mode)
            (eaf-open-external)
          (message "No file associated to this buffer."))))))
;; e13c7903 ends here

;; [[file:../gwp-scratch.note::45f27ad1][45f27ad1]]
(gwp::leader-def
 "f" '(:ignore t :which-key "file")
 "ff" '(find-file :which-key "find files")
 "fr" '(recentf-open-files :which-key "recent files")
 "fs" '(write-file :which-key "save file as")
 "fj" '(dired-jump :which-key "jump to dired buffer")
 "fJ" '(dired-jump-other-window :which-key "jump to dired buffer (other window)")
 "fb" '(bookmark-jump :which-key "open bookmarks")
 "fo" '(spacemacs/open-file-or-directory-in-external-app :which-key "open externally")
 "fw" '(write-region :which-key "write region to file")
 )
;; 45f27ad1 ends here

;; [[file:../gwp-scratch.note::860eb4b2][860eb4b2]]
;; 可仿 vi :q, :q
(defalias 'q 'save-buffers-kill-terminal)
(defalias 'wq 'save-buffers-kill-terminal)

(gwp::leader-def
 "q" '(:ignore t :which-key "quit/session")
 "qq" '(save-buffers-kill-terminal :which-key "Quit Emacs")
 "qk" '(save-buffers-kill-emacs :which-key "Kill Emacs (and daemon)")
 )
;; 860eb4b2 ends here

;; [[file:../gwp-scratch.note::92af756a][92af756a]]
(gwp::leader-def
  "j" '(:ignore t :which-key "jump")
  "ja" '(gwp::avy-transient :which-key "jump using avy")
  "jl" '(avy-goto-line :which-key "avy goto line")
  "jo" '(gwp::org-babel-tangle-jump-to-org :which-key "jump to org src file")
  "jm" '(gwp::hydra-mark-ring-pop/body :which-key "emacs mark ring")
  "jc" '(gwp::hydra-last-change/body :which-key "last changed positions")
  )
;; 92af756a ends here

;; [[file:../gwp-scratch.note::826282dd][826282dd]]
(transient-define-prefix gwp::open-transient ()
  ["core"
   ("f" "new frame" make-frame-command)
   ("e" "eshell" gwp::open-eshell-here)
   ]
  ["tmux" :if-not display-graphic-p
   ("t" "tmux window" gwp::tmux-new-window)
   ("v" "tmux vertical split" gwp::tmux-open-vertical)
   ]
  ["normal" :if display-graphic-p
   ("t" "tmux terminal" gwp::open-terminal-here)
   ]
  )

(gwp::leader-def
  "o" '(gwp::open-transient :which-key "open"))
;; 826282dd ends here

;; [[file:../gwp-scratch.note::574271f2][574271f2]]
(gwp::leader-def
 "r" '(:ignore t :which-key "resume/ring")
 "rb" '(bookmark-jump :which-key "bookmarks")
 "rm" '(gwp::mark-ring :which-key "mark rings")
 ;; consult 里没有匹配的函数
 "rl" '(ivy-resume :which-key "resume last search")
 "rj" '(list-registers :which-key "resume registers")
  )
;; 574271f2 ends here

;; [[file:../gwp-scratch.note::b357bbd9][b357bbd9]]
(gwp::leader-def
  "e" '(:keymap gwp::edit-map :which-key "edit" :package emacs))
;; b357bbd9 ends here

;; [[file:../gwp-scratch.note::773bd9bb][773bd9bb]]
(gwp::leader-def
 "l" '(:ignore t :which-key "tab")
 "ll" '(tab-recent :which-key "last tab")
 "lt" '(tab-new :which-key "new tab")
 "ld" '(tab-close :which-key "close tab")
 "lr" '(tab-rename :which-key "rename tab")
 "lb" '(project-switch-to-buffer :which-key "project buffer")
 "ln" '(tab-next :which-key "next tab")
 "lf" '(tab-detach :which-key "detach tab")
 "lu" '(tab-undo :which-key "undo closed tab")
 "lp" '(tab-previous :which-key "prev tab")
 "lj" '(dired-jump-other-tab :which-key "dired other tab")
 "l." '(tab-bar-mode :which-key "toggle tab bar")
 "l TAB" '(tab-switch :which-key "switch tab")
 "l SPC" '(tab-next :which-key "next tab")
)
;; 773bd9bb ends here

;; [[file:../gwp-scratch.note::e724170b][e724170b]]
(gwp::leader-def
  "d" '(:keymap gwp::develop-map :which-key "develop" :package emacs))

;; (gwp::leader-def
;;   ;; "d" '(:keymap gwp::develop-map :which-key "develop" :package emacs))
;;   "d" '(:ignore t :which-key "develop")
;;   "dr" '(gwp::recent-dirs :which-key "recent dirs")
;;   "dl" '(comment-dwim :which-key "comment/uncomment lines")
;;   "dh" '(:keymap gwp::symbol-overlay-map :package symbol-overlay :which-key "highlight symbol")
;;   "dj" '(:keymap gwp::citre-map :package citre :which-key "citre jump")
;;   )
;; e724170b ends here

;; [[file:../gwp-scratch.note::e7792733][e7792733]]
(gwp::leader-def
  "h" '(:keymap help-map :which-key "Help" :package emacs))

;; 可以查看不同 mode 下的按键绑定
(bind-key "m" 'describe-keymap help-map)
(bind-key "M" 'describe-mode help-map)
;; 按变量的值反应查找对应的变量
(bind-key "V" 'apropos-value help-map)
;; default: describe-key-briefly
(bind-key "c" 'describe-char help-map)

;;;###autoload
(defun gwp::help-show-major-mode-bindings ()
  (interactive)
  (let* ((keymap (concat (symbol-name major-mode) "-map")))
    (which-key-show-keymap (intern-soft keymap))))
(bind-key "b" 'gwp::help-show-major-mode-bindings help-map)
;; e7792733 ends here

;; [[file:../gwp-scratch.note::c54b17b5][c54b17b5]]
(gwp::leader-def
 "w" '(:keymap gwp::window-map :which-key "Window" :package emacs))
;; c54b17b5 ends here

;; [[file:../gwp-scratch.note::703c9a6f][703c9a6f]]
;; (global-so-long-mode 1)

(gwp::leader-def
 "t" '(:ignore t :which-key "toggle")
 "td" '(toggle-debug-on-error :which-key "debug on error")
 "tr" '(read-only-mode :which-key "read only")
 "tw" '(visual-line-mode :which-key "soft line wrapping")
 "tl" '(gwp::toggle-line-numbers :which-key "display line numbers")
 "tg" '(gwp::toggle-golden-ratio :which-key "auto zoom window")
 )
;; 703c9a6f ends here

;; [[file:../gwp-scratch.note::185fc283][185fc283]]
(gwp::leader-def
 "g" '(:keymap gwp::magit-map :which-key "Magit" :package magit))
;; 185fc283 ends here

;; [[file:../gwp-scratch.note::035db3ed][035db3ed]]
;;;###autoload
(defun gwp::yank-relative-file-path ()
  "将 clipboard 的路径以相对路径的形式插入"
  (interactive)
  (let ((relative-path (file-relative-name (current-kill 0))))
    (unless (string= relative-path "")
      (kill-new relative-path)
      (call-interactively #'gwp::yank-dwim)
      )))

(gwp::leader-def
 "y" '(:ignore t :which-key "yank/clipboard")
 "yo" '(gwp::find-file-from-clipboard :which-key "find file from clipboard")
 "yf" '(gwp::yank-relative-file-path :which-key "yank relative file path")
 )
;; 035db3ed ends here

;; [[file:../gwp-scratch.note::51e6b46e][51e6b46e]]
(general-define-key :prefix-map 'gwp::note-map)
(gwp::leader-def "n" '(:keymap gwp::note-map :which-key "note" :package emacs))
;; 51e6b46e ends here

;; [[file:../gwp-scratch.note::1d4f6e72][1d4f6e72]]
(provide 'init-bindings)
;; 1d4f6e72 ends here
