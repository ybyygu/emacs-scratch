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

(bind-keys :map gwp::open-map
           ("f" . make-frame)
           ("e" . gwp::open-eshell-here))

(unless init-no-x-flag
  (defun gwp/open-in-gnome-terminal (the-directory)
    "Open `the-directory' in external gnome-terminal."
    (let ((process-connection-type nil))
      (start-process "" nil "alacritty" (concat "--working-directory=" the-directory) "-e" "tmux")))

  (defun gwp::open-terminal-here ()
    "Open the current dir in a new terminal window"
    (interactive)
    (let ((default-directory (or (and (eq major-mode 'dired-mode)
                                      (dired-current-directory))
                                 default-directory)))
      (gwp/open-in-gnome-terminal (expand-file-name default-directory))))

  (bind-keys :map gwp::open-map
             ("t" . gwp::open-terminal-here)))
;; 36ca6867 ends here

;; [[file:../gwp-scratch.note::62d090d7][62d090d7]]
(use-package zoxide
  :custom
  ;; related issues:
  ;; 1. https://gitlab.com/Vonfry/zoxide.el/-/issues/3
  ;; 2. https://github.com/ajeetdsouza/zoxide/issues/421
  (zoxide-get-path-function
   (lambda (&rest _) (expand-file-name default-directory)))
  :hook
  ((find-file
    consult-find-file
    dired-after-readin) . zoxide-add))

;; 需要保留 zoxide 目录的次序
;;;###autoload
(defun gwp::zoxide-travel ()
  (interactive)
  (let ((vertico-sort-function nil))
    ;; do not sort candidates
    (call-interactively #'zoxide-travel)))

(bind-keys :map gwp::develop-map
           ("r" . gwp::zoxide-travel))
;; 62d090d7 ends here

;; [[file:../gwp-scratch.note::942579e1][942579e1]]
(defun gwp-mouse-toggle-bm (e)
  "Toggle bookmarking
This command should be bound to a mouse key.
Argument E is a mouse event used by `mouse-set-point'."
  (interactive "@e")
  (save-excursion
    (mouse-set-point e)
    (bm-toggle)))

;; credit: https://github.com/joodland/bm
(use-package bm
  :commands (bm-buffer-restore bm-buffer-save bm-toggle bm-next bm-previous bm-buffer-save-all)
  :init
  ;; restore on load (even before you require bm)
  (setq bm-restore-repository-on-load t)
  ;; Allow cross-buffer 'next'
  (setq bm-cycle-all-buffers nil)
  ;; save bookmarks
  (setq-default bm-buffer-persistence t)
  (setq bm-highlight-style 'bm-highlight-only-fringe)

  (add-hook 'find-file-hooks 'bm-buffer-restore)
  (add-hook 'kill-buffer-hook 'bm-buffer-save)
  (add-hook 'after-save-hook 'bm-buffer-save)
  (add-hook 'after-revert-hook 'bm-buffer-restore)
  (add-hook 'vc-before-checkin-hook 'bm-buffer-save)
  (add-hook 'find-file-hooks 'bm-buffer-restore)

  ;; Saving the repository to file when on exit.
  ;; kill-buffer-hook is not called when emacs is killed, so we
  ;; must save all bookmarks first.
  (add-hook 'kill-emacs-hook #'(lambda nil
                                 (bm-buffer-save-all)
                                 (bm-repository-save)))
  ;; 自动展开 org heading
  (add-hook 'bm-after-goto-hook 'org-bookmark-jump-unhide)

  :custom
  ;; where to store persistant files
  (bm-repository-file (expand-file-name "bm-repository" user-emacs-directory))

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
(require 'transient)
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
   ("n" "Next bookmark" bm-next)
   ("p" "Prev bookmark" bm-previous)
   ]
  )

(bind-keys :map gwp::develop-map
           ("b" . gwp::bookmark-transient))
;; ebe60f2d ends here

;; [[file:../gwp-scratch.note::f95a72e3][f95a72e3]]
(unbind-key "C-x C-p")
;; f95a72e3 ends here

;; [[file:../gwp-scratch.note::f0f6f6eb][f0f6f6eb]]
(provide 'init-workspace)
;; f0f6f6eb ends here
