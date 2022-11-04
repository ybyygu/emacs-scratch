;; [[file:../gwp-scratch.note::902df907][902df907]]
;; -*- lexical-binding: t; -*-
;; 902df907 ends here

;; [[file:../gwp-scratch.note::36ca6867][36ca6867]]
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

;; [[file:../gwp-scratch.note::f95a72e3][f95a72e3]]
(unbind-key "C-x C-p")
;; f95a72e3 ends here

;; [[file:../gwp-scratch.note::f0f6f6eb][f0f6f6eb]]
(provide 'init-workspace)
;; f0f6f6eb ends here
