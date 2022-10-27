;; [[file:../gwp-scratch.note::a4d73c60][a4d73c60]]
;; -*- lexical-binding: t; -*-
;; a4d73c60 ends here

;; [[file:../gwp-scratch.note::6cb02a16][6cb02a16]]
(gwp::goto-leader-def
  :keymaps '(meow-normal-state-keymap meow-motion-state-keymap)
  "g" (general-simulate-key "M-<" :which-key "goto first line")
  "e" (general-simulate-key "M->" :which-key "goto last line")
  "h" (general-simulate-key "C-a" :which-key "goto the beggining of line")
  "l" (general-simulate-key "C-e" :which-key "goto the end of line")
  "c" 'goto-char
  "." 'goto-line
  )
;; 6cb02a16 ends here

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
 )
;; 4cf8c86c ends here

;; [[file:../gwp-scratch.note::33105bcf][33105bcf]]
(gwp::leader-def
 "b" '(:ignore t :which-key "buffer")
 "bb" '(switch-to-buffer :which-key "switch buffer")
 "bd" '(kill-current-buffer :which-key "kill buffer")
 "bk" '(kill-current-buffer :which-key "kill buffer")
 "br" '(revert-buffer :which-key "revert buffer")
 "bn" '(next-buffer :which-key "next buffer")
 "bp" '(previous-buffer :which-key "previous buffer")
 "bm" '(bookmark-set :which-key "set bookmark")
 "bR" '(crux-rename-buffer-and-file :which-key "rename buffer file")
 )
;; 33105bcf ends here

;; [[file:../gwp-scratch.note::f220a2a2][f220a2a2]]
(gwp::leader-def
 "s" '(:ignore t :which-key "search")
 "ss" '(isearch-forward :which-key "search buffer")
 "si" '(imenu :which-key "jump to symbol(imenu)")
 ;; gwp::rg 会虚拟命令, 待 consult remap
 "sd" '(gwp::rg :which-key "search directory")
 )
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
 )
;; 45f27ad1 ends here

;; [[file:../gwp-scratch.note::860eb4b2][860eb4b2]]
(gwp::leader-def
 "q" '(:ignore t :which-key "quit/session")
 "qq" '(save-buffers-kill-terminal :which-key "Quit Emacs")
 "qk" '(save-buffers-kill-emacs :which-key "Kill Emacs (and daemon)")
 )
;; 860eb4b2 ends here

;; [[file:../gwp-scratch.note::92af756a][92af756a]]
(gwp::leader-def
 "j" '(:ignore t :which-key "jump")
 "jo" '(gwp::org-babel-tangle-jump-to-org :which-key "jump to org src file")
 "jm" '(gwp::hydra-mark-ring-pop/body :which-key "emacs mark ring")
 )
;; 92af756a ends here

;; [[file:../gwp-scratch.note::826282dd][826282dd]]
(defun gwp/open-in-gnome-terminal (the-directory)
  "Open `the-directory' in external gnome-terminal."
  (let ((process-connection-type nil))
    ;; (start-process "" nil "terminal-dwim.sh" (concat "--working-directory=" the-directory) "-e" "tmux")
    (start-process "" nil "alacritty" (concat "--working-directory=" the-directory) "-e" "tmux")
    ))

(defun gwp::open-terminal-here ()
  "Open the current dir in a new terminal window"
  (interactive)
  (let ((default-directory (or (and (eq major-mode 'dired-mode)
                                    (dired-current-directory))
                               default-directory)))
    (gwp/open-in-gnome-terminal (expand-file-name default-directory))))

(gwp::leader-def
 "o" '(:ignore t :which-key "open")
 "ot" '(gwp::open-terminal-here :which-key "open terminal here")
 )
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

;; [[file:../gwp-scratch.note::e724170b][e724170b]]
(require 'recentf)
(defun gwp::zoxide-recent-directories ()
  (let* ((output (shell-command-to-string "zoxide query --list"))
         (dirs (split-string output "[\r\n]+" t)))
    dirs))

(defun gwp::dired-recent-directories ()
  (let* ((recent-dirs
          (mapcar (lambda (file)
                    (if (file-directory-p file) file (file-name-directory file)))
                  recentf-list)))
    recent-dirs))

(defun gwp::zoxide-add-directory (dir)
  "将 dir 加入 zoxide 数据库中"
  (message "add %s" dir)
  (when dir (call-process "zoxide" nil nil nil "add" dir)))

;; open recent directory
;; borrows from http://stackoverflow.com/questions/23328037/in-emacs-how-to-maintain-a-list-of-recent-directories
;;;###autoload
(defun gwp::recent-dirs ()
  "Present a list of recently used directories and open the selected one in dired"
  (interactive)
  (let* ((recent-dirs (delete-dups
		       (append (gwp::zoxide-recent-directories) (gwp::dired-recent-directories))))
	 ;; do not sort candidates
	 (vertico-sort-function nil)
	 (default-directory (completing-read "Directory: " recent-dirs nil t)))
    (gwp::zoxide-add-directory default-directory)
    (dired-jump)))

(gwp::leader-def
  "d" '(:ignore t :which-key "develop")
  "dr" '(gwp::recent-dirs :which-key "recent dirs")
  "dl" '(comment-dwim :which-key "comment/uncomment lines")
  "dh" '(:keymap gwp::symbol-overlay-map :package symbol-overlay :which-key "highlight symbol")
  )
;; e724170b ends here

;; [[file:../gwp-scratch.note::e7792733][e7792733]]
(gwp::leader-def
 "h" '(:keymap help-map :which-key "Help" :package emacs))

;; 可以查看不同 mode 下的按键绑定
(bind-key "m" 'describe-keymap help-map)
(bind-key "M" 'describe-mode help-map)
;; e7792733 ends here

;; [[file:../gwp-scratch.note::c54b17b5][c54b17b5]]
(gwp::leader-def
 "w" '(:keymap gwp::window-map :which-key "Window" :package emacs))
;; c54b17b5 ends here

;; [[file:../gwp-scratch.note::185fc283][185fc283]]
(gwp::leader-def
 "g" '(:keymap gwp::magit-map :which-key "Magit" :package magit))
;; 185fc283 ends here

;; [[file:../gwp-scratch.note::51e6b46e][51e6b46e]]
(general-define-key :prefix-map 'gwp::note-map)
(gwp::leader-def "n" '(:keymap gwp::note-map :which-key "note" :package emacs))
;; 51e6b46e ends here

;; [[file:../gwp-scratch.note::1d4f6e72][1d4f6e72]]
(provide 'init-bindings)
;; 1d4f6e72 ends here
